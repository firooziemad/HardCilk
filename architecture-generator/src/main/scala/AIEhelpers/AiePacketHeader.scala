package aiehelpers

import chisel3._
import chisel3.util.Cat

/** AMD AIE packet header for a packet originating in programmable logic. */
private[aiehelpers] object AiePacketHeader {
  def apply(packetId: UInt): UInt = {
    require(packetId.getWidth == 5, "Packet ID must be five bits wide")
    val lower31 = Cat(0.U(3.W), "h7f".U(7.W), "h1f".U(5.W), 0.U(1.W),
      0.U(3.W), 0.U(7.W), packetId)
    Cat(!lower31.xorR, lower31)
  }
}
