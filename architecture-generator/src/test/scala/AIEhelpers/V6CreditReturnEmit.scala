package aiehelpers

import chisel3._
import chisel3.util.Decoupled

class V6CreditReturnHarness extends Module {
  val source = IO(Flipped(Decoupled(UInt(64.W))))
  val sink = IO(Decoupled(UInt(64.W)))
  val credits = IO(Output(Vec(2, Bool())))
  credits := VecInit(V5CreditReturn(source, sink, Seq(0, 1), reset.asBool))
}

object V6CreditReturnEmit extends App {
  _root_.circt.stage.ChiselStage.emitSystemVerilogFile(new V6CreditReturnHarness, args)
}
