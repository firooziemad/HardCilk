package aiehelpers

import chisel3._
import chisel3.util.ReadyValidIO
import chext.{elasticnew => e}

/**
  * Removes explicit AIE credit records from an existing output stream while
  * passing ordinary application records through unchanged.
  */
object V5CreditReturn {
  def apply[T <: Bits](source: ReadyValidIO[T], sink: ReadyValidIO[T],
      tags: Seq[Int], resetting: Bool): Seq[Bool] = {
    require(tags.nonEmpty && tags.distinct.size == tags.size)
    require(source.bits.getWidth >= 64 && source.bits.getWidth == sink.bits.getWidth)

    val gated = e.EWire(chiselTypeOf(source.bits))
    new e.Stall(source, gated) {
      out := in
      cond { resetting }
    }

    val isCredit =
      gated.bits(63, 32) === V5CreditProtocol.TypeHeaderHigh32.U(32.W)
    val matches = tags.map(tag => gated.bits(31, 0) === tag.U(32.W))

    new e.Drop(gated, sink) {
      out := in
      cond { isCredit }
    }

    when(gated.fire && isCredit) {
      assert(VecInit(matches).asUInt.orR,
        "V5 credit has an unknown sharing-group tag")
    }
    matches.map(matched => gated.fire && isCredit && matched)
  }
}
