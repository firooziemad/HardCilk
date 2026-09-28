package chext.elasticnew

import chisel3._
import chisel3.util._
import chisel3.experimental.SourceInfo

import chext.bundles._

import scala.collection.mutable.ListBuffer

private object _wrap {
  def apply[T <: Data](t: T)(implicit si: SourceInfo): Interface[T] = {
    val result = Wire(Interface(chiselTypeOf(t)))
    result.bits := t
    result
  }
}

object Zip {
  def apply[T1 <: Data](rv1: Interface[T1])(implicit si: SourceInfo) = {
    val rv = _wrap(WireBundleN(rv1.bits))
    joinImpl.join(Seq(rv1), rv)
    rv
  }

  def apply[T1 <: Data, T2 <: Data](
      rv1: Interface[T1],
      rv2: Interface[T2]
  )(implicit si: SourceInfo) = {
    val rv = _wrap(WireBundleN(rv1.bits, rv2.bits))
    joinImpl.join(Seq(rv1, rv2), rv)
    rv
  }

  def apply[T1 <: Data, T2 <: Data, T3 <: Data](
      rv1: Interface[T1],
      rv2: Interface[T2],
      rv3: Interface[T3]
  )(implicit si: SourceInfo) = {
    val rv = _wrap(WireBundleN(rv1.bits, rv2.bits, rv3.bits))
    joinImpl.join(Seq(rv1, rv2, rv3), rv)
    rv
  }

  def apply[T1 <: Data, T2 <: Data, T3 <: Data, T4 <: Data](
      rv1: Interface[T1],
      rv2: Interface[T2],
      rv3: Interface[T3],
      rv4: Interface[T4]
  )(implicit si: SourceInfo) = {
    val rv = _wrap(WireBundleN(rv1.bits, rv2.bits, rv3.bits, rv4.bits))
    joinImpl.join(Seq(rv1, rv2, rv3, rv4), rv)
    rv
  }

  def apply[T1 <: Data, T2 <: Data, T3 <: Data, T4 <: Data, T5 <: Data](
      rv1: Interface[T1],
      rv2: Interface[T2],
      rv3: Interface[T3],
      rv4: Interface[T4],
      rv5: Interface[T5]
  )(implicit si: SourceInfo) = {
    val rv = _wrap(
      WireBundleN(rv1.bits, rv2.bits, rv3.bits, rv4.bits, rv5.bits)
    )
    joinImpl.join(Seq(rv1, rv2, rv3, rv4, rv5), rv)
    rv
  }

  def apply[
      T1 <: Data,
      T2 <: Data,
      T3 <: Data,
      T4 <: Data,
      T5 <: Data,
      T6 <: Data
  ](
      rv1: Interface[T1],
      rv2: Interface[T2],
      rv3: Interface[T3],
      rv4: Interface[T4],
      rv5: Interface[T5],
      rv6: Interface[T6]
  )(implicit si: SourceInfo) = {
    val rv = _wrap(
      WireBundleN(rv1.bits, rv2.bits, rv3.bits, rv4.bits, rv5.bits, rv6.bits)
    )
    joinImpl.join(Seq(rv1, rv2, rv3, rv4, rv5, rv6), rv)
    rv
  }

  def apply[
      T1 <: Data,
      T2 <: Data,
      T3 <: Data,
      T4 <: Data,
      T5 <: Data,
      T6 <: Data,
      T7 <: Data
  ](
      rv1: Interface[T1],
      rv2: Interface[T2],
      rv3: Interface[T3],
      rv4: Interface[T4],
      rv5: Interface[T5],
      rv6: Interface[T6],
      rv7: Interface[T7]
  )(implicit si: SourceInfo) = {
    val rv = _wrap(
      WireBundleN(
        rv1.bits,
        rv2.bits,
        rv3.bits,
        rv4.bits,
        rv5.bits,
        rv6.bits,
        rv7.bits
      )
    )
    joinImpl.join(Seq(rv1, rv2, rv3, rv4, rv5, rv6, rv7), rv)
    rv
  }

  def apply[
      T1 <: Data,
      T2 <: Data,
      T3 <: Data,
      T4 <: Data,
      T5 <: Data,
      T6 <: Data,
      T7 <: Data,
      T8 <: Data
  ](
      rv1: Interface[T1],
      rv2: Interface[T2],
      rv3: Interface[T3],
      rv4: Interface[T4],
      rv5: Interface[T5],
      rv6: Interface[T6],
      rv7: Interface[T7],
      rv8: Interface[T8]
  )(implicit si: SourceInfo) = {
    val rv = _wrap(
      WireBundleN(
        rv1.bits,
        rv2.bits,
        rv3.bits,
        rv4.bits,
        rv5.bits,
        rv6.bits,
        rv7.bits,
        rv8.bits
      )
    )
    joinImpl.join(Seq(rv1, rv2, rv3, rv4, rv5, rv6, rv7, rv8), rv)
    rv
  }
}
