package aiehelpers

import chisel3._
import chisel3.experimental.prefix
import chisel3.util.{Fill, PopCount, isPow2, log2Ceil, Queue}
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
    outputWidth: Int,
    segmentBeats: Int,
    creditCapacities: Vector[Int],
    initialCredits: Vector[Int],
    creditPerSegment: Boolean = true
) {
  val fanIn: Int = packetIds.length

  require(fanIn >= 2 && fanIn <= 32,
    "AiePacketMuxStream fan-in must be between 2 and 32")
  require(Set(2, 4).contains(fanIn),
    "V9 verified receive-FIFO mode supports only fan-in 2 or 4")
  require(creditCapacities.length == fanIn && initialCredits.length == fanIn,
    "Every packet-mux lane requires a capacity and initial credit count")
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
  val outputCfg: axi4s.Config = axi4s.Config(wData = outputWidth)

  def moduleSuffix: String =
    if (outputWidth == 32) s"${fanIn}_${dataWidth}"
    else s"${fanIn}_${dataWidth}_${outputWidth}"
}

/**
  * Counted stream admission. A receive-slot credit admits one bounded transport
  * segment. A request-derived token admits one complete logical read response,
  * even when it spans several bounded transport segments.
  */
class AiePacketMuxStreamCredit_Basic(
    cfg: AiePacketMuxStream_Config
) extends Module {
  import cfg._

  require(segmentBeats > 0, "V5 stream credit mux requires positive segmentBeats")

  override def desiredName: String =
    s"AiePacketMuxStreamCredit_Basic_${cfg.moduleSuffix}_s${segmentBeats}_c${creditCapacities.mkString("_")}_i${initialCredits.mkString("_")}"

  val packetIn = IO(Vec(fanIn, axi4s.Slave(inputCfg)))
  val packetOut = IO(axi4s.Master(outputCfg))
  val creditReturn = IO(Input(Vec(fanIn, Bool())))

  private val innerInputs = Wire(Vec(fanIn, axi4s.Master(inputCfg)))
  private val counters = Module(new V5CreditCounter(
    creditCapacities, initialCredits))
  counters.replenish := creditReturn
  private val segmentActive = RegInit(VecInit(Seq.fill(fanIn)(false.B)))
  // A read request grants one complete logical response. That response may be
  // divided into several bounded transport packets, but consumes its grant
  // only once. Ordinary AIE receive-slot credits remain segment based.
  private val logicalStreamActive = RegInit(VecInit(Seq.fill(fanIn)(false.B)))
  private val beatWidth = math.max(1, chisel3.util.log2Ceil(math.max(2, segmentBeats)))
  private val beatIndex = RegInit(VecInit(Seq.fill(fanIn)(0.U(beatWidth.W))))

  for (lane <- 0 until fanIn) {
    val source = packetIn(lane).asFull
    val sink = innerInputs(lane).asFull
    val creditUnitActive =
      if (creditPerSegment) segmentActive(lane) else logicalStreamActive(lane)
    val mayEnter = creditUnitActive || counters.available(lane)
    val reachesBound = beatIndex(lane) === (segmentBeats - 1).U
    val physicalLast = source.bits.last.asBool || reachesBound

    new e.Stall(source, sink) {
      out := in
      out.last := physicalLast
      cond { !mayEnter }
    }

    val accepted = source.valid && source.ready
    val startsCreditUnit = accepted && !creditUnitActive
    val endsSegment = accepted && physicalLast

    counters.consume(lane) := startsCreditUnit
    when(accepted) {
      logicalStreamActive(lane) := !source.bits.last.asBool
      when(endsSegment) {
        segmentActive(lane) := false.B
        beatIndex(lane) := 0.U
      }.otherwise {
        segmentActive(lane) := true.B
        beatIndex(lane) := beatIndex(lane) + 1.U
      }
    }
  }

  private val inner = Module(new V5PacketizedStreamMuxCore(cfg))
  for (lane <- 0 until fanIn) {
    inner.packetIn(lane) <> innerInputs(lane)
  }
  packetOut <> inner.packetOut

  val creditAvailable = WireInit(counters.available.asUInt)
    .suggestName("creditAvailable")
  val creditCount = WireInit(counters.count.asUInt)
    .suggestName("creditCount")
  val admittedSegment = WireInit(segmentActive.asUInt)
    .suggestName("admittedSegment")
  val admittedLogicalStream = WireInit(logicalStreamActive.asUInt)
    .suggestName("admittedLogicalStream")
  val creditReturnMask = WireInit(creditReturn.asUInt)
    .suggestName("creditReturnMask")
}

// The stream implementation owns its packetizer, just as ReadStream owns its inner module.
private class V5PacketMuxStreamPacketBeat(outputCfg: axi4s.Config)
    extends Bundle {
  val channel = new axi4s.FullChannel(outputCfg)
  val isHeader = Bool()
}


private[aiehelpers] class V5PacketizedStreamMuxCore(cfg: AiePacketMuxStream_Config) extends Module {
  import cfg._

  override def desiredName: String =
    s"V5PacketizedStreamMuxCore_${cfg.moduleSuffix}"

  val packetIn = IO(Vec(fanIn, axi4s.Slave(inputCfg)))
  val packetOut = IO(axi4s.Master(outputCfg))

  private val inputsAndRequests = packetIn.zipWithIndex.map {
    case (input, lane) =>
      prefix(s"packetize$lane") {
        // Store-and-forward: never lock the shared mux onto an incomplete segment.
        val segment = Module(new Queue(new axi4s.FullChannel(inputCfg), segmentBeats))
        val complete = RegInit(false.B)
        val ingress = input.asFull
        segment.io.enq.bits := ingress.bits
        segment.io.enq.valid := ingress.valid && !complete && !reset.asBool
        ingress.ready := segment.io.enq.ready && !complete && !reset.asBool
        val source = e.EWire(new axi4s.FullChannel(inputCfg))
        source.bits := segment.io.deq.bits
        source.valid := segment.io.deq.valid && complete && !reset.asBool
        segment.io.deq.ready := source.ready && complete && !reset.asBool
        when(segment.io.enq.fire && segment.io.enq.bits.last.asBool) { complete := true.B }
        when(segment.io.deq.fire && segment.io.deq.bits.last.asBool) { complete := false.B }
        val packetBeat = e.EWire(new V5PacketMuxStreamPacketBeat(outputCfg))

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

        val muxInput = e.EWire(new axi4s.FullChannel(outputCfg))
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

  // Use natural lane order.
  private val arbitrationOrder = 0 until fanIn
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
