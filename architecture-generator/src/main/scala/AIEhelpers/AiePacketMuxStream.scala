package aiehelpers

import chisel3._
import chisel3.experimental.prefix
import chisel3.util.{Fill, PopCount, isPow2, log2Ceil}

import chext.{elasticnew => e}
import e.ConnectOp._

import chext.amba.axi4s
import axi4s.Casts._

/**
  * Configuration for a PL-to-AIE packetizer whose logical inputs contain
  * TLAST-delimited, variable-length streams.
  *
  * Logical input beats are packed into a fixed 32-, 64-, or 128-bit physical
  * packet stream. The first output beat contains the 32-bit AIE header followed
  * immediately by payload; subsequent beats continue the payload without gaps.
  */
case class AiePacketMuxStream_Config(
    packetIds: Vector[Int],
    dataWidth: Int,
    fifoDepth: Int = 2,
    outputWidth: Int = 32
) {
  val fanIn: Int = packetIds.length

  require(fanIn >= 2 && fanIn <= 32,
    "AiePacketMuxStream fan-in must be between 2 and 32")
  require(packetIds.distinct.length == fanIn, "Packet IDs must be unique")
  require(packetIds.forall(id => id >= 0 && id < 32),
    "Every packet ID must be between 0 and 31")
  require(dataWidth >= 8 && isPow2(dataWidth),
    "AiePacketMuxStream data width must be a byte-aligned power of two")
  require(fifoDepth > 0, "FIFO depth must be positive")
  require(Set(32, 64, 128).contains(outputWidth),
    "AiePacketMuxStream output width must be 32, 64, or 128")

  val selectWidth: Int = log2Ceil(fanIn)
  val inputCfg: axi4s.Config = axi4s.Config(wData = dataWidth)
  val packetCfg: axi4s.Config = axi4s.Config(wData = outputWidth)

  def moduleSuffix: String =
    if (outputWidth == 32) s"${fanIn}_${dataWidth}"
    else s"${fanIn}_${dataWidth}_${outputWidth}"
}

private class AiePacketMuxStream_PacketBeat(packetCfg: axi4s.Config)
    extends Bundle {
  val channel = new axi4s.FullChannel(packetCfg)
  val isHeader = Bool()
}


class AiePacketMuxStream_Basic(cfg: AiePacketMuxStream_Config) extends Module {
  import cfg._

  override def desiredName: String =
    s"AiePacketMuxStream_Basic_${cfg.moduleSuffix}"

  val packetIn = IO(Vec(fanIn, axi4s.Slave(inputCfg)))
  val packetOut = IO(axi4s.Master(packetCfg))

  private val inputsAndRequests = packetIn.zipWithIndex.map {
    case (input, lane) =>
      prefix(s"packetize$lane") {
        val source = e.SourceBuffer(input.asFull, count = fifoDepth)
        val packetBeat = e.EWire(new AiePacketMuxStream_PacketBeat(packetCfg))

        /*
         * Header-aware width transducer.
         *
         * Bytes are accumulated from the least-significant lane upward. A new
         * packet starts with four valid header bytes, so a 128-bit first beat
         * is [payload word 2 | payload word 1 | payload word 0 | header].
         *
         * Transducer actions provide all ready/valid control:
         *   consume: collect a narrow input without producing an output beat;
         *   produce: emit part of a wide input while retaining that input;
         *   accept:  emit the final part and consume the retained input.
         */
        val inputBytes = dataWidth / 8
        val outputBytes = outputWidth / 8
        val bufferBytes = inputBytes + outputBytes
        val bufferBits = bufferBytes * 8
        val countWidth = math.max(1, log2Ceil(bufferBytes + 1))

        val bufferedData = RegInit(0.U(bufferBits.W))
        val bufferedStrobe = RegInit(0.U(bufferBytes.W))
        val bufferedKeep = RegInit(0.U(bufferBytes.W))
        val bufferedBytes = RegInit(0.U(countWidth.W))
        val packetActive = RegInit(false.B)
        val sourceIncluded = RegInit(false.B)
        val firstOutputBeat = RegInit(true.B)

        val headerData = AiePacketHeader(packetIds(lane).U(5.W))
        val baseData = Wire(UInt(bufferBits.W))
        val baseStrobe = Wire(UInt(bufferBytes.W))
        val baseKeep = Wire(UInt(bufferBytes.W))
        val baseBytes = Wire(UInt(countWidth.W))

        baseData := Mux(packetActive, bufferedData, headerData.pad(bufferBits))
        baseStrobe := Mux(
          packetActive,
          bufferedStrobe,
          Fill(4, 1.U).pad(bufferBytes)
        )
        baseKeep := Mux(
          packetActive,
          bufferedKeep,
          Fill(4, 1.U).pad(bufferBytes)
        )
        baseBytes := Mux(packetActive, bufferedBytes, 4.U)

        val inputValidBytes = PopCount(source.bits.keep)
        val appendBitShift = baseBytes << 3
        val appendedData = baseData |
          ((source.bits.data.pad(bufferBits) << appendBitShift)(bufferBits - 1, 0))
        val appendedStrobe = baseStrobe |
          ((source.bits.strobe.pad(bufferBytes) << baseBytes)(bufferBytes - 1, 0))
        val appendedKeep = baseKeep |
          ((source.bits.keep.pad(bufferBytes) << baseBytes)(bufferBytes - 1, 0))
        val appendedBytes = baseBytes + inputValidBytes

        /*
         * A produced beat can retain its source token. In that case the source
         * has already been appended to the buffer and must not be appended a
         * second time on the following cycle.
         */
        val currentData = Mux(sourceIncluded, bufferedData, appendedData)
        val currentStrobe = Mux(sourceIncluded, bufferedStrobe, appendedStrobe)
        val currentKeep = Mux(sourceIncluded, bufferedKeep, appendedKeep)
        val currentBytes = Mux(sourceIncluded, bufferedBytes, appendedBytes)

        new e.Transducer(source, packetBeat) {
          out := 0.U.asTypeOf(out)
          out.channel.data := currentData(outputWidth - 1, 0)
          out.channel.strobe := currentStrobe(outputBytes - 1, 0)
          out.channel.keep := currentKeep(outputBytes - 1, 0)
          out.channel.last := in.last.asBool && currentBytes <= outputBytes.U
          out.isHeader := firstOutputBeat

          private def clearPacket(): Unit = {
            bufferedData := 0.U
            bufferedStrobe := 0.U
            bufferedKeep := 0.U
            bufferedBytes := 0.U
            packetActive := false.B
            sourceIncluded := false.B
            firstOutputBeat := true.B
          }

          private def retainRemainder(): Unit = {
            bufferedData := currentData >> outputWidth
            bufferedStrobe := currentStrobe >> outputBytes
            bufferedKeep := currentKeep >> outputBytes
            bufferedBytes := currentBytes - outputBytes.U
            packetActive := true.B
            sourceIncluded := true.B
            firstOutputBeat := false.B
          }

          private def finishSource(): Unit = {
            bufferedData := 0.U
            bufferedStrobe := 0.U
            bufferedKeep := 0.U
            bufferedBytes := 0.U
            packetActive := true.B
            sourceIncluded := false.B
            firstOutputBeat := false.B
          }

          private def collectSource(): Unit = {
            bufferedData := currentData
            bufferedStrobe := currentStrobe
            bufferedKeep := currentKeep
            bufferedBytes := currentBytes
            packetActive := true.B
            sourceIncluded := false.B
          }

          packet {
            when(currentBytes > outputBytes.U) {
              produce {
                retainRemainder()
              }
            }.elsewhen(currentBytes === outputBytes.U) {
              accept {
                when(in.last.asBool) {
                  clearPacket()
                }.otherwise {
                  finishSource()
                }
              }
            }.elsewhen(in.last.asBool) {
              accept {
                clearPacket()
              }
            }.otherwise {
              consume {
                collectSource()
              }
            }
          }
        }

        val muxInput = e.EWire(new axi4s.FullChannel(packetCfg))
        val requestCandidate = e.EWire(Bool())

        new e.Fork(packetBeat) {
          fork { in.channel } :=> muxInput
          fork { in.isHeader } :=> requestCandidate
        }

        val request = e.EWire(UInt(selectWidth.W))
        new e.Drop(requestCandidate, request) {
          out := lane.U(selectWidth.W)
          cond { !in }
        }

        (muxInput, request)
      }
  }

  private val muxInputs = inputsAndRequests.map(_._1)
  private val requests = inputsAndRequests.map(_._2)

  // Preserve the original four-lane order (3, 0, 1, 2), generalized to N.
  private val arbitrationOrder = Seq(fanIn - 1) ++ (0 until fanIn - 1)
  private val selectCandidate = e.EWire(UInt(selectWidth.W))

  e.Arbiter(
    arbitrationOrder.map(requests),
    selectCandidate,
    e.Chooser.rr
  )

  private val select = e.SourceBuffer(
    selectCandidate,
    count = 1,
    pipe = true
  )

  e.Mux(
    muxInputs,
    packetOut.asFull,
    select,
    isLastFn = ((in: axi4s.FullChannel) => in.last.asBool)
  )
}

// object AiePacketMuxStreamEmit extends App {
//   require(
//     args.length >= 3,
//     "Usage: AiePacketMuxStreamEmit ID0,ID1,... DATA_WIDTH OUTPUT_WIDTH [firtool options]"
//   )

//   val packetIds = args(0).split(',').map(Integer.decode(_).intValue()).toVector
//   val dataWidth = args(1).toInt
//   val outputWidth = args(2).toInt

//   _root_.circt.stage.ChiselStage.emitSystemVerilogFile(
//     new AiePacketMuxStream_Basic(
//       AiePacketMuxStream_Config(
//         packetIds = packetIds,
//         dataWidth = dataWidth,
//         outputWidth = outputWidth
//       )
//     ),
//     args.drop(3)
//   )
// }
