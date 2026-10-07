package aiehelpers

import chisel3._
import chisel3.util.log2Ceil

/**
  * Per-destination counted credits shared by V5 fixed-record and segmented
  * stream admission.  Credits are complete receive slots.  Simultaneous
  * consume/replenish leaves the count unchanged.
  */
class V5CreditCounter(
    laneCapacities: Seq[Int],
    laneInitialCredits: Seq[Int],
    coalesceAtCapacity: Boolean = false
) extends Module {
  private val laneCount = laneCapacities.length
  require(laneCount >= 2 && laneCount <= 32,
    "V5 credit lane count must be in [2, 32]")
  require(laneInitialCredits.length == laneCount,
    "V5 initial-credit vector must match the lane count")
  require(laneCapacities.forall(_ > 0),
    "V5 lane credit capacities must be positive")
  require(laneInitialCredits.zip(laneCapacities).forall {
    case (initial, capacity) => initial >= 0 && initial <= capacity
  }, "V5 lane initial credits must be in [0, lane capacity]")

  def this(laneCount: Int, capacity: Int, initial: Int,
      coalesceAtCapacity: Boolean) =
    this(Seq.fill(laneCount)(capacity), Seq.fill(laneCount)(initial),
      coalesceAtCapacity)

  val consume = IO(Input(Vec(laneCount, Bool())))
  val replenish = IO(Input(Vec(laneCount, Bool())))
  val available = IO(Output(Vec(laneCount, Bool())))
  private val countWidth = math.max(1, log2Ceil(laneCapacities.max + 1))
  val count = IO(Output(Vec(laneCount, UInt(countWidth.W))))

  private val credits = RegInit(VecInit(
    laneInitialCredits.map(_.U(countWidth.W))))

  for (lane <- 0 until laneCount) {
    available(lane) := credits(lane) =/= 0.U
    count(lane) := credits(lane)

    when(replenish(lane) && !consume(lane)) {
      if (coalesceAtCapacity) {
        when(credits(lane) < laneCapacities(lane).U) {
          credits(lane) := credits(lane) + 1.U
        }
      } else {
        assert(credits(lane) < laneCapacities(lane).U,
          "V5 credit counter overflow")
        credits(lane) := credits(lane) + 1.U
      }
    }.elsewhen(consume(lane) && !replenish(lane)) {
      assert(credits(lane) =/= 0.U,
        "V5 credit counter underflow")
      credits(lane) := credits(lane) - 1.U
    }
  }
}
