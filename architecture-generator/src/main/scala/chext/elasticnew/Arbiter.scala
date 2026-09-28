package chext.elasticnew

import chisel3._
import chisel3.util._
import chisel3.experimental.SourceInfo

import ConnectOp._

class Arbiter[T <: Data](
    val gen: T,
    val n: Int,
    val chooser: Chooser
) extends Module {
  require(n > 0)
  val genSelect = UInt(chisel3.util.log2Up(n).W)

  override def desiredName: String = "elasticArbiter"

  val io = IO(new Bundle {
    val sources = Vec(n, Source(gen))
    val sink = Sink(gen)
    val select = Sink(genSelect)
  })

  private val sources = io.sources

  private val sink = io.sink
  private val select = io.select

  private val regSink = RegInit(false.B)
  private val regSelect = RegInit(false.B)

  private val ready = (sink.ready || regSink) && (select.ready || regSelect)

  private val choice = chooser(VecInit(sources.map { _.valid }), ready)

  sources.zipWithIndex.foreach { case (x, i) =>
    x.ready := ready && i.U === (choice)
  }

  sink.valid := sources(choice).valid && !regSink
  select.valid := sources(choice).valid && !regSelect

  regSink := (sink.ready || regSink) && sources(choice).valid && !ready
  regSelect := (select.ready || regSelect) && sources(choice).valid && !ready

  sink.bits := sources(choice).bits
  select.bits := choice
}

object Arbiter {
  def apply[T <: Data](
      sources: Seq[Interface[T]],
      sink: Interface[T],
      chooserFn: Chooser,
      select: Option[Interface[UInt]] = None
  )(implicit si: SourceInfo): Unit = {
    val arbiter = Module(
      new Arbiter(
        chiselTypeOf(sources(0).bits),
        sources.length,
        chooserFn
      )
    )

    sources.zip(arbiter.io.sources).foreach { case (x, y) => x :=> y }
    arbiter.io.sink :=> sink

    select match {
      case None => {
        arbiter.io.select.ready := true.B
      }
      case Some(select) => arbiter.io.select :=> select
    }
  }
}
