package aiehelpers

object AiePacketMuxSingleCreditEmit extends App {
  require(args.length >= 4,
    "Usage: AiePacketMuxSingleCreditEmit IDS DATA_WIDTH CREDIT_CAPACITY INITIAL_CREDITS [firtool options]")
  val packetIds = args(0).split(',').map(Integer.decode(_).intValue()).toVector
  val dataWidth = args(1).toInt
  def laneValues(text: String): Vector[Int] = {
    val values = text.split(',').map(_.toInt).toVector
    if (values.length == 1) Vector.fill(packetIds.length)(values.head) else values
  }
  val creditCapacities = laneValues(args(2))
  val initialCredits = laneValues(args(3))

  _root_.circt.stage.ChiselStage.emitSystemVerilogFile(
    new AiePacketMuxSingleCredit_Basic(
      AiePacketMuxSingle_Config(
        packetIds = packetIds,
        dataWidth = dataWidth,
        creditCapacities = creditCapacities,
        initialCredits = initialCredits
      )
    ),
    args.drop(4)
  )
}

object AiePacketMuxStreamCreditEmit extends App {
  require(args.length >= 6,
    "Usage: AiePacketMuxStreamCreditEmit IDS DATA_WIDTH OUTPUT_WIDTH SEGMENT_BEATS CREDIT_CAPACITY INITIAL_CREDITS [--logical-credit] [firtool options]")
  val packetIds = args(0).split(',').map(Integer.decode(_).intValue()).toVector
  val dataWidth = args(1).toInt
  val outputWidth = args(2).toInt
  val segmentBeats = args(3).toInt
  def laneValues(text: String): Vector[Int] = {
    val values = text.split(',').map(_.toInt).toVector
    if (values.length == 1) Vector.fill(packetIds.length)(values.head) else values
  }
  val creditCapacities = laneValues(args(4))
  val initialCredits = laneValues(args(5))
  val logicalCredit = args.lift(6).contains("--logical-credit")
  val firtoolArgs = args.drop(if (logicalCredit) 7 else 6)

  _root_.circt.stage.ChiselStage.emitSystemVerilogFile(
    new AiePacketMuxStreamCredit_Basic(
      AiePacketMuxStream_Config(
        packetIds = packetIds,
        dataWidth = dataWidth,
        outputWidth = outputWidth,
        segmentBeats = segmentBeats,
        creditCapacities = creditCapacities,
        initialCredits = initialCredits,
        creditPerSegment = !logicalCredit
      )
    ),
    firtoolArgs
  )
}

object StreamSplitterEmit extends App {
  require(args.nonEmpty,
    "Usage: StreamSplitterEmit OUTPUT_WIDTHS [firtool options]")
  val outputWidths = args(0).split(',').map(_.toInt).toVector

  _root_.circt.stage.ChiselStage.emitSystemVerilogFile(
    new StreamSplitter_Basic(StreamSplitter_Config(outputWidths)),
    args.drop(1)
  )
}
