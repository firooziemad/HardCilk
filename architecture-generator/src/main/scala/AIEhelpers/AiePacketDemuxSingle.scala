package aiehelpers

import chisel3._
import chisel3.util.{PriorityEncoder, Queue, log2Ceil}
import chext.amba.axi4s
import axi4s.Casts._

/** Receives compiler-tagged AIE packets and restores fixed-width logical records.
  * The application AIE kernel puts one 32-bit header in front of each TLAST-delimited
  * record. A physical beat is 32 bits, so no header/payload byte packing is needed.
  */
case class AiePacketDemuxSingle_Config(
    packetIds: Vector[Int],
    dataWidth: Int,
    fifoDepth: Int = 2
) {
  require(packetIds.length >= 2 && packetIds.length <= 32)
  require(packetIds.distinct.length == packetIds.length)
  require(packetIds.forall(id => id >= 0 && id < 32))
  require(dataWidth > 0 && dataWidth % 32 == 0)
  require(fifoDepth > 0)
  val fanOut = packetIds.length
  val wordsPerRecord = dataWidth / 32
  val inputCfg = axi4s.Config(wData = 32)
  val outputCfg = axi4s.Config(wData = dataWidth, true)
}

class AiePacketDemuxSingle_Basic(cfg: AiePacketDemuxSingle_Config) extends Module {
  import cfg._
  override def desiredName: String = s"AiePacketDemuxSingle_Basic_${fanOut}_${dataWidth}"

  val packetIn = IO(axi4s.Slave(inputCfg))
  val packetOut = IO(Vec(fanOut, axi4s.Master(outputCfg)))
  val malformed = IO(Output(Bool()))

  val queues = Seq.fill(fanOut)(Module(new Queue(UInt(dataWidth.W), fifoDepth)))
  queues.zipWithIndex.foreach { case (queue, index) =>
    queue.io.deq <> packetOut(index).asLite
    queue.io.enq.valid := false.B
    queue.io.enq.bits := 0.U
  }

  val inPacket = RegInit(false.B)
  val dropping = RegInit(false.B)
  val target = RegInit(0.U(log2Ceil(fanOut).W))
  val wordIndex = RegInit(0.U(math.max(1, log2Ceil(wordsPerRecord)).W))
  val record = RegInit(0.U(dataWidth.W))
  val bad = RegInit(false.B)
  malformed := bad

  val input = packetIn.asFull
  val headerId = input.bits.data(4, 0)
  // AIE writeHeader inserts the source tile column/row in bits [27:16].
  // Those coordinates differ from the PL-origin header used by AiePacketMuxSingle.
  // Check the fixed fields and odd parity while accepting any AIE source tile.
  val validHeader = input.bits.data(30, 28) === 0.U &&
    input.bits.data(15, 5) === 0.U && input.bits.data.xorR
  val matching = VecInit(packetIds.map(id => headerId === id.U(5.W)))
  val known = matching.asUInt.orR
  val matchedIndex = PriorityEncoder(matching)
  val finalWord = wordIndex === (wordsPerRecord - 1).U
  val chosenReady = VecInit(queues.map(_.io.enq.ready))(target)
  input.ready := !inPacket || dropping || !finalWord || chosenReady

  when(input.fire) {
    when(!inPacket) {
      // The header is routing metadata, never application data.
      when(input.bits.last.asBool || !known || input.bits.keep =/= 15.U ||
        !validHeader) {
        bad := true.B
        dropping := !input.bits.last.asBool
        inPacket := !input.bits.last.asBool
      }.otherwise {
        inPacket := true.B
        target := matchedIndex
        wordIndex := 0.U
        record := 0.U
      }
    }.elsewhen(dropping) {
      when(input.bits.last.asBool) {
        dropping := false.B
        inPacket := false.B
      }
    }.otherwise {
      val shifted = input.bits.data << (wordIndex << 5)
      val updated = record | shifted(dataWidth - 1, 0)
      when(input.bits.keep =/= 15.U || input.bits.last.asBool =/= finalWord) {
        bad := true.B
        dropping := !input.bits.last.asBool
        inPacket := !input.bits.last.asBool
        record := 0.U
      }.elsewhen(finalWord) {
        queues.zipWithIndex.foreach { case (queue, index) =>
          when(target === index.U) {
            queue.io.enq.valid := true.B
            queue.io.enq.bits := updated
          }
        }
        inPacket := false.B
        record := 0.U
      }.otherwise {
        record := updated
        wordIndex := wordIndex + 1.U
      }
    }
  }
}

object AiePacketDemuxSingleEmit extends App {
  require(args.length >= 3,
    "Usage: AiePacketDemuxSingleEmit ID0,ID1,... DATA_WIDTH TARGET_DIR")
  val ids = args(0).split(',').map(_.toInt).toVector
  val width = args(1).toInt
  _root_.circt.stage.ChiselStage.emitSystemVerilogFile(
    new AiePacketDemuxSingle_Basic(AiePacketDemuxSingle_Config(ids, width)),
    Array("--target-dir", args(2))
  )
}
