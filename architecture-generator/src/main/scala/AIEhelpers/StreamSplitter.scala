package aiehelpers

import chisel3._
import chisel3.util.{Queue, Decoupled}
import chext.amba.axi4s
import axi4s.Casts._
import Descriptors._
import scala.collection.mutable

case class StreamSplitterDataRoute(taskName: String, peIndex: Int,
    endpointName: String, portType: String, bitWidth: Int)

case class StreamSplitterGroup(taskName: String, peIndex: Int,
    endpointName: String, aiePortType: String,
    dataRoutes: Vector[StreamSplitterDataRoute],
    creditGroups: Vector[AIEPlioPhysicalSharingGroupDescriptor]) {
  val outputWidths: Vector[Int] = dataRoutes.map(_.bitWidth) ++ creditGroups.map(_ => 32)
  val physicalPortName: String =
    s"AIEOutput_${taskName}_${peIndex}_${endpointName}_${aiePortType}"
      .replaceAll("[^A-Za-z0-9_]", "_")
}

/** Shared output plan used by PeIO, connectivity, and kernel XML. */
object StreamSplitterPlan {
  private case class Endpoint(name: String, normalizedName: String,
      nextSubPE: Option[String], taskOutputPorts: Seq[String])

  def groups(descriptor: FullSysGenDescriptor): Vector[StreamSplitterGroup] = {
    val endpoints = descriptor.taskDescriptors.map { task =>
      val ordered = orderedEndpoints(descriptor, task.name)
      val byPort = mutable.Map[String, String]()
      ordered.foreach(e => e.taskOutputPorts.foreach(p => byPort(p) = e.normalizedName))
      task.name -> (ordered.lastOption.map(_.normalizedName).getOrElse(task.name),
        byPort.toMap, ordered.dropRight(1).map(_.normalizedName).toSet)
    }.toMap
    val taskRoutes = collectTaskOutputs(descriptor).map { route =>
      val (default, byPort, _) = endpoints(route.taskName)
      route.copy(endpointName = byPort.getOrElse(route.portType, default))
    }
    val routes = taskRoutes ++ collectRwOutputs(descriptor)
    val result = routes.groupBy(r => (r.taskName, r.peIndex, r.endpointName)).toVector
      .sortBy(_._1).flatMap { case ((taskName, peIndex, endpointName), endpointRoutes) =>
        val ordered = endpointRoutes.groupBy(_.portType)
          .map { case (_, values) => values.maxBy(_.bitWidth) }.toVector
          .sortBy(r => (priority(r.portType), r.portType))
        val credits = descriptor.expandedAiePlioSharingGroups.filter(g =>
          g.direction == "plToAie" && g.taskName == taskName && g.peIndices.contains(peIndex) &&
            normalize(g.subPEName) == endpointName).sortBy(_.name).toVector
        credits.foreach(c => require(ordered.exists(_.portType == c.creditReturnPort),
          s"AIE PLIO sharing group ${c.name} returns credit on ${c.creditReturnPort}, " +
            s"but AIE endpoint $endpointName has no such output"))
        val base = if (endpoints(taskName)._3(endpointName) && ordered.size > 1) Vector(ordered)
          else if (ordered.size > 2) ordered.grouped(2).filter(_.size > 1).map(_.toVector).toVector
          else Vector.empty
        val combined = base.flatten.map(_.portType).toSet
        val creditOnly = ordered.filter(r => !combined(r.portType) &&
          credits.exists(_.creditReturnPort == r.portType)).map(r => Vector(r))
        (base ++ creditOnly).map { dataRoutes =>
          val groupCredits = credits.filter(c => dataRoutes.exists(_.portType == c.creditReturnPort))
          StreamSplitterGroup(taskName, peIndex, endpointName,
            dataRoutes.map(_.portType).mkString("_"), dataRoutes, groupCredits)
        }
      }
    // Credit-bearing direct outputs use the same explicit tagged format as
    // combined outputs. Their generated wrapper frames the ordinary records.
    result
  }

  private def collectTaskOutputs(d: FullSysGenDescriptor): Vector[StreamSplitterDataRoute] = {
    val direct = d.getSystemConnectionsDescriptor().connections.flatMap { c =>
      if (c.srcPort.parentType == "PE" && c.dstPort.parentType == "HardCilk")
        Some(StreamSplitterDataRoute(c.srcPort.parentName, c.srcPort.parentIndex,
          "", c.srcPort.portType, c.bitWidth)) else None
    }
    val generated = d.taskDescriptors.flatMap { task =>
      (0 until task.numProcessingElements).flatMap { i => Seq(
        Option.when(task.generateArgOutWriteBuffer)(StreamSplitterDataRoute(
          task.name, i, "", "argDataOut", argDataOutWidth(d, task))),
        Option.when(task.generateSpawnNextWriteBuffer)(StreamSplitterDataRoute(
          task.name, i, "", "spawnNext", spawnNextWidth(d, task)))).flatten
      }
    }
    (direct ++ generated).groupBy(r => (r.taskName, r.peIndex, r.portType))
      .map { case (_, values) => values.maxBy(_.bitWidth) }.toVector
  }

  private def collectRwOutputs(d: FullSysGenDescriptor): Vector[StreamSplitterDataRoute] = {
    val counts = d.taskDescriptors.map(t => t.name -> t.numProcessingElements).toMap
    d.subPEList.toVector.sortBy(_._1).flatMap { case (name, sub) =>
      sub.rwRequest.toVector.flatMap { req => (0 until counts(sub.peName)).map { i =>
        StreamSplitterDataRoute(sub.peName, i, normalize(name), rwPort(req), sourceWidth(req))
      }}
    }
  }

  private def orderedEndpoints(d: FullSysGenDescriptor, task: String): Vector[Endpoint] = {
    val all = d.subPEList.toVector.filter(_._2.peName == task).map { case (name, sub) =>
      Endpoint(name, normalize(name), sub.rwRequest.flatMap(_.nextsubPE), sub.taskOutputPorts)
    }
    val byName = all.map(e => e.name -> e).toMap
    val incoming = all.flatMap(_.nextSubPE).toSet
    val result = mutable.ArrayBuffer[Endpoint]()
    val visited = mutable.Set[String]()
    def walk(head: Endpoint): Unit = {
      var current = Option(head)
      while (current.nonEmpty && !visited(current.get.name)) {
        val e = current.get; result += e; visited += e.name
        current = e.nextSubPE.flatMap(byName.get)
      }
    }
    all.filterNot(e => incoming(e.name)).sortBy(_.name).foreach(walk)
    all.sortBy(_.name).filterNot(e => visited(e.name)).foreach(walk)
    result.toVector
  }

  private def rwPort(r: RWRequestDescriptor): String = (r.`type`, r.mode) match {
    case ("read", "single") => s"readSingle${r.portWidth}Out"
    case ("read", "stream") => s"readStream${r.portWidth}Out"
    case ("write", "single") => s"writeSingle${r.portWidth}Out"
    case ("write", "stream") => s"writeStream${r.portWidth}Out"
    case other => throw new IllegalArgumentException(s"Unsupported rwRequest: $other")
  }
  private def sourceWidth(r: RWRequestDescriptor): Int = {
    val n = (r.`type`, r.mode) match {
      case ("read", "single") => 64; case ("read", "stream") => 96
      case ("write", _) => 64 + r.portWidth
      case other => throw new IllegalArgumentException(s"Unsupported rwRequest: $other")
    }; n + ((64 - n % 64) % 64)
  }
  private def spawnNextWidth(d: FullSysGenDescriptor, t: TaskDescriptor): Int = {
    val data = d.spawnNextList.get(t.name).map(_.flatMap(n =>
      d.taskDescriptors.find(_.name == n).map(_.widthTask))).filter(_.nonEmpty)
      .map(_.max).getOrElse(t.widthTask)
    nextPow2(d.widthAddress + data + 32 +
      (1 + d.spawnList.getOrElse(t.name, List.empty).count(_ != t.name)) *
        (if (t.variableSpawn) 0 else 32))
  }
  private def argDataOutWidth(d: FullSysGenDescriptor, t: TaskDescriptor): Int =
    nextPow2(d.widthAddress + t.argumentSizeList.headOption.getOrElse(0) + 64)
  private def nextPow2(n: Int): Int = { var x = 1; while (x < n) x <<= 1; x }
  private def priority(p: String): Int = p match {
    case "taskOut" => 0; case "taskOutGlobal" => 1; case "argOut" => 2
    case "closureOut" => 3; case "mallocOut" => 4; case "argDataOut" => 5
    case "spawnNext" => 6; case _ => 100
  }
  private def normalize(s: String): String = s.toLowerCase.replaceAll("[^a-z0-9_]", "")
}

/**
  * Configuration for the original HardCilk/AIECilk tagged-output splitter.
  *
  * A fixed-size input record contains max(outputWidths) payload bits followed
  * by 96 compatibility bits and a trailing 32-bit output tag. The tag is the
  * zero-based output index. The selected output receives the low payload bits
  * as one ready/valid transfer.
  */
case class StreamSplitter_Config(
    outputWidths: Vector[Int],
    shouldBufferOutput: Boolean = false,
    shouldBufferInput: Boolean = true
) {
  require(outputWidths.nonEmpty, "StreamSplitter requires at least one output")
  require(outputWidths.forall(width => width > 0 && width % 8 == 0),
    "StreamSplitter output widths must be positive multiples of eight")

  val max: Int = outputWidths.max
  val inputWidth: Int = max + 128
  val inputCfg: axi4s.Config = axi4s.Config(wData = inputWidth, true)
  val outputCfgs: Vector[axi4s.Config] =
    outputWidths.map(width => axi4s.Config(wData = width, onlyRV = true))

  def moduleSuffix: String = outputWidths.mkString("_")
}

/**
  * Established elastic-bugra StreamSplitter implementation, compiled inside
  * HardCilk so descriptor-required variants are emitted with the other RTL.
  */
class StreamSplitter_Basic(cfg: StreamSplitter_Config) extends Module {
  import cfg._

  override def desiredName: String = s"StreamSplitter_Basic_${cfg.moduleSuffix}"

  val input = IO(axi4s.Slave(inputCfg))
  val outputs = outputWidths.map { width =>
    IO(axi4s.Master(axi4s.Config(wData = width, onlyRV = true)))
  }

  // Queue.enq.ready is not reset-qualified. Accepting an AIE's first grant
  // during reset silently loses it while the receiver waits for its data.
  private val ingress = Wire(Decoupled(UInt(inputWidth.W)))
  ingress.bits := input.asLite.bits
  ingress.valid := input.asLite.valid && !reset.asBool
  input.asLite.ready := ingress.ready && !reset.asBool
  private val source =
    if (shouldBufferInput) Queue(ingress, entries = 2)
    else ingress
  private val tag = source.bits(inputWidth - 1, inputWidth - 32)
  private val decoded = outputWidths.map(w => Wire(Decoupled(UInt(w.W))))
  decoded.zip(outputs).foreach { case (lane, output) =>
    val queued = if (shouldBufferOutput) Queue(lane, entries = 2) else lane
    output.asLite.bits := queued.bits
    output.asLite.valid := queued.valid && !reset.asBool
    queued.ready := output.asLite.ready && !reset.asBool
  }
  private val outputReady = VecInit(decoded.map(_.ready))
  private val selectWidth = math.max(1, chisel3.util.log2Ceil(outputWidths.length))
  private val selectedOutput = tag(selectWidth - 1, 0)

  source.ready := Mux(
    !reset.asBool && tag < outputWidths.length.U,
    outputReady(selectedOutput),
    false.B
  )
  when(source.valid) {
    assert(tag < outputWidths.length.U, "StreamSplitter tag is out of range")
  }

  decoded.zipWithIndex.foreach { case (sink, index) =>
    sink.valid := !reset.asBool && source.valid && tag === index.U
    sink.bits := source.bits(outputWidths(index) - 1, 0)
  }
}
