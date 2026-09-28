package aiehelpers

import chisel3._
import chisel3.util.{Cat, Fill, log2Ceil}

import chext.{elasticnew => e}

import chext.amba.axi4s
import axi4s.Casts._

/** AMD AIE packet header for a packet originating in the programmable logic. */
private[aiehelpers] object AiePacketHeader {
  def apply(packetId: UInt): UInt = {
    require(packetId.getWidth == 5, "Packet ID must be five bits wide")

    val lower31 = Cat(
      0.U(3.W),
      "h7f".U(7.W),
      "h1f".U(5.W),
      0.U(1.W),
      0.U(3.W),
      0.U(7.W),
      packetId
    )

    Cat(!lower31.xorR, lower31)
  }
}


case class AiePacketMuxSingle_Config(
    packetIds: Vector[Int],
    dataWidth: Int,
    fifoDepth: Int = 2
) {
  val fanIn: Int = packetIds.length

  require(fanIn >= 2 && fanIn <= 32,
    "AiePacketMuxSingle fan-in must be between 2 and 32")
  require(packetIds.distinct.length == fanIn, "Packet IDs must be unique")
  require(packetIds.forall(id => id >= 0 && id < 32),
    "Every packet ID must be between 0 and 31")
  require(dataWidth > 0 && dataWidth % 8 == 0,
    "Data width must be a positive multiple of eight")
  require(fifoDepth > 0, "FIFO depth must be positive")

  val selectWidth: Int = log2Ceil(fanIn)
  val packetWidth: Int = dataWidth + 32
  val packetBytes: Int = packetWidth / 8

  val inputCfg: axi4s.Config = axi4s.Config(wData = dataWidth, true)
  val packetCfg: axi4s.Config = axi4s.Config(wData = packetWidth)

  def moduleSuffix: String = s"${fanIn}_${dataWidth}"
}


class AiePacketMuxSingle_Basic(cfg: AiePacketMuxSingle_Config) extends Module {
  import cfg._

  override def desiredName: String =
    s"AiePacketMuxSingle_Basic_${cfg.moduleSuffix}"

  val packetIn = IO(Vec(fanIn, axi4s.Slave(inputCfg)))
  val packetOut = IO(axi4s.Master(packetCfg))

  private val bufferedInputs =
    packetIn.map(source => e.SourceBuffer(source.asLite, fifoDepth))

  // Preserve the lane rotation used by AieTaskTx and the verified merger.
  private val arbitrationOrder = Seq(fanIn - 1) ++ (0 until fanIn - 1)
  private val selectedPayload = e.EWire(UInt(dataWidth.W))
  private val selectedIndex = e.EWire(UInt(selectWidth.W))

  e.Arbiter(
    arbitrationOrder.map(bufferedInputs),
    selectedPayload,
    e.Chooser.rr,
    select = Some(selectedIndex)
  )

  new e.Transform(e.Zip(selectedPayload, selectedIndex), packetOut.asFull) {
    private val packetId = VecInit(
      arbitrationOrder.map(index => packetIds(index).U(5.W))
    )(in._2)

    out := 0.U.asTypeOf(out)
    out.data := Cat(in._1, AiePacketHeader(packetId))
    out.strobe := Fill(packetBytes, 1.U)
    out.keep := Fill(packetBytes, 1.U)
    out.last := true.B
  }
}

// object AiePacketMuxSingleEmit extends App {
//   require(
//     args.length >= 2,
//     "Usage: AiePacketMuxSingleEmit ID0,ID1,... DATA_WIDTH [firtool options]"
//   )

//   val packetIds = args(0).split(',').map(Integer.decode(_).intValue()).toVector
//   val dataWidth = args(1).toInt

//   _root_.circt.stage.ChiselStage.emitSystemVerilogFile(
//     new AiePacketMuxSingle_Basic(
//       AiePacketMuxSingle_Config(packetIds, dataWidth)
//     ),
//     args.drop(2)
//   )
// }






/**
  * Width-generic AieTaskTx data path.
  *
  * One accepted input item produces exactly one output item:
  *
  *   bits [31:0]              = AIE packet header
  *   bits [dataWidth+31:32]   = logical payload
  *   TLAST                    = 1
  */



  /**
  * Configuration for a PL-to-AIE packetizer in which every logical input
  * transfer is one complete packet.
  *
  * The data path is the width-generic form of the proven AieTaskTx design:
  * buffer each lane, arbitrate complete logical items, and then attach the
  * compiler-assigned header belonging to the selected lane.
  */