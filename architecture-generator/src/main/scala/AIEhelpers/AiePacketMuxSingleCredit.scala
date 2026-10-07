package aiehelpers

import chisel3._
import chisel3.util.{Cat, Fill, log2Ceil}

import chext.{elasticnew => e}
import chext.amba.axi4s
import axi4s.Casts._

case class AiePacketMuxSingle_Config(
    packetIds: Vector[Int],
    dataWidth: Int,
    fifoDepth: Int = 2,
    creditCapacities: Vector[Int],
    initialCredits: Vector[Int],
    coalesceCreditsAtCapacity: Boolean = false
) {
  val fanIn: Int = packetIds.length

  require(fanIn >= 2 && fanIn <= 32,
    "AiePacketMuxSingle fan-in must be between 2 and 32")
  require(Set(2, 4).contains(fanIn),
    "V9 verified receive-FIFO mode supports only fan-in 2 or 4")
  require(creditCapacities.length == fanIn && initialCredits.length == fanIn,
    "Every packet-mux lane requires a capacity and initial credit count")
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
  val outputCfg: axi4s.Config = axi4s.Config(wData = packetWidth)

  def moduleSuffix: String = s"${fanIn}_${dataWidth}"
}

/**
  * V5 work-conserving fixed-record mux.
  *
  * A lane is eligible only when it has both a buffered record and a complete
  * destination receive-slot credit.  Selection can therefore never stop the
  * shared PLIO merely because a different destination is full.
  */
class AiePacketMuxSingleCredit_Basic(
    cfg: AiePacketMuxSingle_Config
) extends Module {
  import cfg._

  override def desiredName: String =
    s"AiePacketMuxSingleCredit_Basic_${cfg.moduleSuffix}_c${creditCapacities.mkString("_")}_i${initialCredits.mkString("_")}"

  val packetIn = IO(Vec(fanIn, axi4s.Slave(inputCfg)))
  val packetOut = IO(axi4s.Master(outputCfg))
  val creditReturn = IO(Input(Vec(fanIn, Bool())))

  private val counters = Module(new V5CreditCounter(
    creditCapacities, initialCredits, coalesceCreditsAtCapacity))
  counters.replenish := creditReturn

  private val bufferedInputs =
    packetIn.map(source => e.SourceBuffer(source.asLite, fifoDepth))
  private val eligibleInputs = bufferedInputs.zipWithIndex.map {
    case (source, lane) =>
      val eligible = e.EWire(UInt(dataWidth.W))
      new e.Stall(source, eligible) {
        out := in
        cond { !counters.available(lane) }
      }
      eligible
  }

  private val arbitrationOrder = 0 until fanIn
  private val selectedPayload = e.EWire(UInt(dataWidth.W))
  private val selectedOrderIndex = e.EWire(UInt(selectWidth.W))

  e.Arbiter(
    arbitrationOrder.map(eligibleInputs),
    selectedPayload,
    e.Chooser.rr,
    select = Some(selectedOrderIndex)
  )

  private val selectedLane = VecInit(
    arbitrationOrder.map(_.U(selectWidth.W))
  )(selectedOrderIndex.bits)

  new e.Transform(e.Zip(selectedPayload, selectedOrderIndex), packetOut.asFull) {
    private val packetId = VecInit(
      arbitrationOrder.map(index => packetIds(index).U(5.W))
    )(in._2)

    out := 0.U.asTypeOf(out)
    out.data := Cat(in._1, AiePacketHeader(packetId))
    out.strobe := Fill(packetBytes, 1.U)
    out.keep := Fill(packetBytes, 1.U)
    out.last := true.B
  }

  private val sendFire = packetOut.TVALID && packetOut.TREADY
  for (lane <- 0 until fanIn) {
    counters.consume(lane) := sendFire && selectedLane === lane.U
  }

  // Stable waveform names.
  val creditAvailable = WireInit(counters.available.asUInt)
    .suggestName("creditAvailable")
  val creditCount = WireInit(counters.count.asUInt)
    .suggestName("creditCount")
  val eligible = WireInit(VecInit(eligibleInputs.map(_.valid)).asUInt)
    .suggestName("eligible")
  val selected = WireInit(selectedLane).suggestName("selectedLane")
  val creditReturnMask = WireInit(creditReturn.asUInt)
    .suggestName("creditReturnMask")
}
