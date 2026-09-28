package chext.elasticnew

import chisel3._
import chisel3.util._
import chisel3.experimental.AffectsChiselPrefix

abstract trait Chooser {
  def apply(valid: Vec[Bool], ready: Bool): UInt
}

object Chooser {
  object rr extends Chooser {
    def apply(valid: Vec[Bool], ready: Bool): UInt = {
      val wChoice = log2Ceil(valid.length)

      val max = (-1).S(wChoice.W).asUInt
      val last = RegInit(0.U(wChoice.W))

      val priority0 = PriorityEncoder(valid)
      val rr0 =
        Mux(
          last === max,
          0.U(wChoice.W),
          PriorityEncoder(valid.zipWithIndex.map { case (x, i) =>
            i.U > last && x
          })
        )
      val rr = Mux(valid(rr0), rr0, priority0)

      val locked = RegInit(false.B)
      val lockedChoice = RegInit(0.U(wChoice.W))

      val thisChoice = Mux(locked, lockedChoice, rr)

      val shouldLock = valid(thisChoice) && !ready
      locked := shouldLock
      lockedChoice := thisChoice

      when(valid(thisChoice) && ready) {
        last := thisChoice
      }

      thisChoice
    }
  }

  object priority extends Chooser {
    def apply(valid: Vec[Bool], ready: Bool): UInt = {
      val wChoice = log2Ceil(valid.length)

      val priority = PriorityEncoder(valid)

      val locked = RegInit(false.B)
      val lockedChoice = RegInit(0.U(wChoice.W))

      val thisChoice = WireInit(Mux(locked, lockedChoice, priority))

      val shouldLock = valid(thisChoice) && !ready
      locked := shouldLock
      lockedChoice := thisChoice

      thisChoice
    }
  }
}
