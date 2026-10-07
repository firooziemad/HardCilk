package aiehelpers

import chisel3._

import chext.{elasticnew => e}
import e.ConnectOp._

import chext.amba.axi4
import axi4.Ops._
import chext.amba.axi4s
import axi4s.Casts._

import Descriptors._

case class PeRwPort(
    subPEName: String,
    index: Int,
    sourceTask: axi4s.Interface,
    sinkResult: Option[axi4s.Interface],
    internalSinkResult: Option[e.Interface[axi4s.FullChannel]],
    m_axi: axi4.RawInterface
) {
  def instanceName: String = s"${subPEName}_${index}"
}

case class PeSharedHardCilkInput(
    taskName: String,
    index: Int,
    portName: String,
    logicalWidth: Int,
    sink: axi4s.Interface
)

case class PeSharedHardCilkOutput(
    taskName: String,
    index: Int,
    portName: String,
    logicalWidth: Int,
    source: axi4s.Interface
)

case class PeSharedPackedOutput(physicalPortName: String, source: axi4s.Interface)

case class PeSharedPacketInput(physicalPortName: String, sink: axi4s.Interface)

case class PeSharedPacketOutput(
    groupName: String,
    physicalPortName: String,
    source: axi4s.Interface
)

case class PeSplitPhysicalInput(
    group: StreamSplitterGroup,
    sink: axi4s.Interface
)

case class PeSplitDataOutput(
    route: StreamSplitterDataRoute,
    source: axi4s.Interface
)

class PeIO(
    subPEs: Map[String, SubPEDescriptor],
    count: Int,
    taskName: String = "",
    sharingGroups: Seq[AIEPlioPhysicalSharingGroupDescriptor] = Seq.empty,
    outputSplitterGroups: Seq[StreamSplitterGroup] = Seq.empty,
    directCreditRoutes: Seq[StreamSplitterDataRoute] = Seq.empty
) extends Module {
  require(count >= 0, "count must be non-negative")
  require(sharingGroups.forall(_.taskName == taskName),
    s"Every sharing group passed to PeIO($taskName) must belong to task '$taskName'")
  require(outputSplitterGroups.forall(_.taskName == taskName),
    s"Every output splitter passed to PeIO($taskName) must belong to task '$taskName'")

  private def isSharedReadResponse(subPEName: String, index: Int): Boolean =
    sharingGroups.exists { group =>
      group.sourceKind == AiePlioSourceKind.ReadResponse &&
      group.sourceSubPEName == subPEName &&
      group.peIndices.contains(index)
    }

  private val localCreditReturnPulses =
    scala.collection.mutable.Map[(String, Int), Bool]()

  /**
    * A HardCilk-input credit is an explicit record emitted by the AIE kernel
    * when it consumes that input. Used by memory-request and direct task
    * outputs alike; ordinary application records must not create credits.
    */
  private def connectRwRequestOutput(
      subPEName: String,
      index: Int,
      returnPort: String,
      source: axi4s.Interface,
      destination: axi4s.Interface
  ): Unit = {
    val creditGroups = sharingGroups.filter { group =>
      group.direction == "plToAie" &&
      group.subPEName == subPEName &&
      group.peIndices.contains(index) &&
      group.creditReturnPort == returnPort &&
      !outputSplitterGroups.exists(s => s.peIndex == index &&
        s.creditGroups.exists(_.name == group.name))
    }
    if (creditGroups.isEmpty) {
      source.asLite :=> destination.asLite
    } else {
      require(source.cfg.onlyRV && destination.cfg.onlyRV &&
        source.cfg.wData == destination.cfg.wData && source.cfg.wData >= 64,
        s"Explicit AIE credit return on $subPEName[$index].$returnPort requires a >=64-bit ready/valid stream")
      val pulses = V5CreditReturn(
        source.asLite, destination.asLite,
        creditGroups.map(_.creditTag), reset.asBool)
      creditGroups.zip(pulses).foreach { case (group, pulse) =>
        localCreditReturnPulses((group.name, index)) = pulse
      }
    }
  }

  private case class SplitterResult(
      physicalInput: PeSplitPhysicalInput,
      dataOutputs: Seq[PeSplitDataOutput],
      creditPulses: Seq[(AIEPlioPhysicalSharingGroupDescriptor, Int, Bool)]
  )

  private val internallySharedSplitters = sharingGroups.filter(_.sourceKind == AiePlioSourceKind.PackedOutput)
    .flatMap(g => g.peIndices.map(i => (g.taskName, i,
      g.subPEName.toLowerCase.replaceAll("[^a-z0-9_]", ""), g.portName))).toSet

  private val outputSplitters = outputSplitterGroups.map { group =>
    val cfg = StreamSplitter_Config(group.outputWidths, shouldBufferOutput = true)
    val dut = Module(new StreamSplitter_Basic(cfg))
    dut.suggestName(s"${group.endpointName}_${group.peIndex}_streamSplitter")
    val input =
      if (internallySharedSplitters((group.taskName, group.peIndex, group.endpointName, group.aiePortType)))
        Wire(axi4s.Slave(cfg.inputCfg))
      else IO(axi4s.Slave(cfg.inputCfg)).suggestName(group.physicalPortName)
    input.asLite :=> dut.input.asLite

    val dataOutputs = group.dataRoutes.zip(dut.outputs).map { case (route, source) =>
      val output = IO(axi4s.Master(axi4s.Config(
        wData = route.bitWidth, onlyRV = true)))
        .suggestName(s"${group.endpointName}_${group.peIndex}_${route.portType}_split")
      source.asLite :=> output.asLite
      PeSplitDataOutput(route, output)
    }
    val creditPulses = group.creditGroups.zip(
      dut.outputs.drop(group.dataRoutes.size)).map { case (creditGroup, source) =>
      source.asLite.ready := !reset.asBool
      (creditGroup, group.peIndex, source.asLite.fire)
    }
    SplitterResult(PeSplitPhysicalInput(group, input), dataOutputs, creditPulses)
  }

  val splitPhysicalInputs: Seq[PeSplitPhysicalInput] =
    outputSplitters.map(_.physicalInput).filterNot { split =>
      internallySharedSplitters((split.group.taskName, split.group.peIndex,
        split.group.endpointName, split.group.aiePortType))
    }
  val splitDataOutputs: Seq[PeSplitDataOutput] =
    outputSplitters.flatMap(_.dataOutputs)
  private val creditReturns = outputSplitters.flatMap(_.creditPulses)

  private def isSharedRwRequest(subPEName: String, index: Int): Boolean =
    sharingGroups.exists(group => group.sourceKind == AiePlioSourceKind.RwRequest &&
      group.subPEName == subPEName && group.peIndices.contains(index))

  val rwPorts: Seq[PeRwPort] =
    (0 until count).flatMap { index =>
      subPEs.toSeq.sortBy(_._1).flatMap { case (name, descriptor) =>
        descriptor.rwRequest.map { req =>
          val instanceName = s"${name}_${index}"

          (req.`type`, req.mode) match {
            case ("read", "single") =>
              val cfg = ReadSingle_Config(dataWidth = req.portWidth)
              val dut = Module(new ReadSingle_Basic(cfg))

              val sourceTask =
                (if (isSharedRwRequest(name, index)) Wire(axi4s.Slave(cfg.inputCfg))
                 else IO(axi4s.Slave(cfg.inputCfg)).suggestName(s"${instanceName}_sourceTask"))
              val internalSinkResult =
                e.EWire(new axi4s.FullChannel(cfg.outputCfg))
              val sinkResult =
                if (isSharedReadResponse(name, index)) {
                  None
                } else {
                  val external = IO(axi4s.Master(cfg.outputCfg))
                    .suggestName(s"${instanceName}_sinkResult")
                  internalSinkResult :=> external.asFull
                  Some(external)
                }
              val m_axi =
                IO(axi4.Master(cfg.axiCfg)).suggestName(s"${instanceName}_m_axi")

              connectRwRequestOutput(
                name, index, s"readSingle${req.portWidth}Out",
                sourceTask, dut.sourceTask)
              dut.sinkResult.asFull :=> internalSinkResult
              dut.m_axi :=> m_axi.asFull

              PeRwPort(
                name,
                index,
                sourceTask,
                sinkResult,
                Some(internalSinkResult),
                m_axi
              )

            case ("read", "stream") =>
              val cfg = ReadStream_Config(dataWidth = req.portWidth)
              val dut = Module(new ReadStreamWSplitter_Basic(cfg))

              val sourceTask =
                (if (isSharedRwRequest(name, index)) Wire(axi4s.Slave(cfg.inputCfg))
                 else IO(axi4s.Slave(cfg.inputCfg)).suggestName(s"${instanceName}_sourceTask"))
              val internalSinkResult =
                e.EWire(new axi4s.FullChannel(cfg.outputCfg))
              val sinkResult =
                if (isSharedReadResponse(name, index)) {
                  None
                } else {
                  val external = IO(axi4s.Master(cfg.outputCfg))
                    .suggestName(s"${instanceName}_sinkResult")
                  internalSinkResult :=> external.asFull
                  Some(external)
                }
              val m_axi =
                IO(axi4.Master(cfg.axiCfg)).suggestName(s"${instanceName}_m_axi")

              connectRwRequestOutput(
                name, index, s"readStream${req.portWidth}Out",
                sourceTask, dut.sourceTask)
              dut.sinkResult.asFull :=> internalSinkResult
              dut.m_axi :=> m_axi.asFull

              PeRwPort(
                name,
                index,
                sourceTask,
                sinkResult,
                Some(internalSinkResult),
                m_axi
              )

            case ("write", "single") =>
              val cfg = WriteSingle_Config(dataWidth = req.portWidth)
              val dut = Module(new WriteSingle_Basic(cfg))

              val sourceTask =
                (if (isSharedRwRequest(name, index)) Wire(axi4s.Slave(cfg.inputCfg))
                 else IO(axi4s.Slave(cfg.inputCfg)).suggestName(s"${instanceName}_sourceTask"))
              val m_axi =
                IO(axi4.Master(cfg.axiCfg)).suggestName(s"${instanceName}_m_axi")

              connectRwRequestOutput(
                name, index, s"writeSingle${req.portWidth}Out",
                sourceTask, dut.sourceTask)
              dut.m_axi :=> m_axi.asFull

              PeRwPort(name, index, sourceTask,
                None, None, m_axi)

            case other =>
              throw new Exception(s"Unsupported rwRequest for subPE $name: $other")
          }
        }
      }
    }

  val axiMasters: Seq[axi4.full.Interface] =
    rwPorts.map(_.m_axi.asFull)

  // Direct task outputs bypass StreamSplitter and memory-request helpers.
  // Decode their credit records before connecting the packetizer credit sinks.
  val directCreditOutputs = directCreditRoutes.map { route =>
    val cfg = axi4s.Config(wData = route.bitWidth, onlyRV = true)
    val input = IO(axi4s.Slave(cfg))
      .suggestName(s"directCredit_${route.peIndex}_${route.portType}_in")
    val output = IO(axi4s.Master(cfg))
      .suggestName(s"directCredit_${route.peIndex}_${route.portType}_out")
    connectRwRequestOutput(route.endpointName, route.peIndex, route.portType, input, output)
    (route, input, output)
  }

  private case class PacketizerResult(
      hardCilkInputs: Seq[PeSharedHardCilkInput],
      packetOutput: PeSharedPacketOutput,
      readResponseKeys: Set[(String, Int)]
  )

  private def connectCreditReturns(
      group: AIEPlioPhysicalSharingGroupDescriptor,
      sinks: Seq[Bool]
  ): Unit = {
    require(sinks.length == group.fanIn,
      s"AIE PLIO sharing group '${group.name}' has the wrong number of credit sinks")

    group.peIndices.zipWithIndex.foreach { case (peIndex, laneIndex) =>
      // Uniform slot credits: only receiver consumption replenishes a slot.
      {
        val splitPulse = creditReturns.collectFirst {
          case (creditGroup, creditPe, fire)
              if creditGroup.name == group.name && creditPe == peIndex => fire
        }
        val explicitPulse = localCreditReturnPulses.get((group.name, peIndex))
        val pulse = splitPulse.orElse(explicitPulse).getOrElse(throw new IllegalArgumentException(
          s"No credit-return input exists for ${group.name} lane $laneIndex"))
        sinks(laneIndex) := pulse
      }
    }
  }
  private def connectPacketMux(
      group: AIEPlioPhysicalSharingGroupDescriptor,
      inputCfg: axi4s.Config,
      packetCfg: axi4s.Config,
      packetInputs: Seq[axi4s.Interface],
      packetOutputSource: axi4s.Interface,
      streamMode: Boolean
  ): PacketizerResult = {
    val hardCilkInputs = scala.collection.mutable.ArrayBuffer[PeSharedHardCilkInput]()
    val readResponseKeys = scala.collection.mutable.Set[(String, Int)]()

    group.peIndices.zipWithIndex.foreach { case (peIndex, laneIndex) =>
      group.sourceKind match {
        case AiePlioSourceKind.HardCilkInput =>
          require(!streamMode,
            s"AIE PLIO sharing group '${group.name}': HardCilk inputs require single mode")
          val input = IO(axi4s.Slave(inputCfg))
            .suggestName(s"${group.name}_${group.portName}_${peIndex}")
          input.asLite :=> packetInputs(laneIndex).asLite
          hardCilkInputs += PeSharedHardCilkInput(
            taskName,
            peIndex,
            group.portName,
            group.logicalWidth,
            input
          )

        case AiePlioSourceKind.ReadResponse =>
          val key = (group.sourceSubPEName, peIndex)
          val response = rwPorts
            .find(port => port.subPEName == key._1 && port.index == key._2)
            .flatMap(_.internalSinkResult)
            .getOrElse(throw new IllegalArgumentException(
              s"AIE PLIO sharing group '${group.name}' cannot find read response ${key._1}[${key._2}]"
            ))

          if (streamMode) {
            response :=> packetInputs(laneIndex).asFull
          } else {
            new e.Transform(response, packetInputs(laneIndex).asLite) {
              out := in.data
            }
          }
          readResponseKeys += key

        case other =>
          throw new IllegalArgumentException(
            s"AIE PLIO sharing group '${group.name}' has unsupported source kind '$other'"
          )
      }
    }

    val output = IO(axi4s.Master(packetCfg))
      .suggestName(group.effectivePhysicalPortName)
    packetOutputSource.asFull :=> output.asFull

    PacketizerResult(
      hardCilkInputs.toSeq,
      PeSharedPacketOutput(group.name, group.effectivePhysicalPortName, output),
      readResponseKeys.toSet
    )
  }

  private val packetizers = sharingGroups.filter(_.direction == "plToAie").map { group =>
    // The AIE receive FIFO is the storage protected by these counters. One
    // credit reserves one complete fixed record or bounded stream segment.
    // Branches may have different depths to keep their receive FIFOs exclusive.
    val effectiveCreditCapacities = group.receiveCreditCapacities
    val effectiveInitialCredits = effectiveCreditCapacities

    group.mode match {
      case AiePlioSharingMode.Single =>
        val cfg = AiePacketMuxSingle_Config(
          packetIds = group.packetIds,
          dataWidth = group.logicalWidth,
          fifoDepth = group.fifoDepth,
          creditCapacities = effectiveCreditCapacities,
          initialCredits = effectiveInitialCredits,
          coalesceCreditsAtCapacity = false
        )
        val dut = Module(new AiePacketMuxSingleCredit_Basic(cfg))
        dut.suggestName(s"${group.name}_singlePacketMuxCredit")
        connectCreditReturns(group, dut.creditReturn.toSeq)
        connectPacketMux(
          group,
          cfg.inputCfg,
          cfg.outputCfg,
          dut.packetIn.toSeq,
          dut.packetOut,
          streamMode = false
        )

      case AiePlioSharingMode.Stream =>
        val cfg = AiePacketMuxStream_Config(
          packetIds = group.packetIds,
          dataWidth = group.logicalWidth,
          fifoDepth = group.fifoDepth,
          outputWidth = group.physicalPlioWidth,
          segmentBeats = group.segmentBeats,
          creditCapacities = effectiveCreditCapacities,
          initialCredits = effectiveInitialCredits,
          creditPerSegment = true
        )
        val dut = Module(new AiePacketMuxStreamCredit_Basic(cfg))
        dut.suggestName(s"${group.name}_streamPacketMuxCredit")
        connectCreditReturns(group, dut.creditReturn.toSeq)
        connectPacketMux(
          group,
          cfg.inputCfg,
          cfg.outputCfg,
          dut.packetIn.toSeq,
          dut.packetOut,
          streamMode = true
        )

      case other =>
        throw new IllegalArgumentException(
          s"AIE PLIO sharing group ${group.name} requested unknown mode $other"
        )
    }
  }

  val sharedHardCilkInputs: Seq[PeSharedHardCilkInput] =
    packetizers.flatMap(_.hardCilkInputs)

  val sharedReadResponseKeys: Set[(String, Int)] =
    packetizers.flatMap(_.readResponseKeys).toSet

  val sharedPacketOutputs: Seq[PeSharedPacketOutput] =
    packetizers.map(_.packetOutput)

  private val outputDemuxes = sharingGroups.filter(_.direction == "aieToPl").map { group =>
    val cfg = AiePacketDemuxSingle_Config(group.packetIds, group.logicalWidth, group.fifoDepth)
    val dut = Module(new AiePacketDemuxSingle_Basic(cfg))
    dut.suggestName(s"${group.name}_singlePacketDemux")
    val physical = IO(axi4s.Slave(cfg.inputCfg)).suggestName(group.effectivePhysicalPortName)
    physical.asFull :=> dut.packetIn.asFull
    val packed = scala.collection.mutable.ArrayBuffer[PeSharedPackedOutput]()
    val logical = group.peIndices.zipWithIndex.flatMap { case (peIndex, lane) =>
      group.sourceKind match {
        case AiePlioSourceKind.HardCilkOutput =>
          val source = IO(axi4s.Master(cfg.outputCfg))
            .suggestName(s"${group.name}_${group.portName}_${peIndex}")
          dut.packetOut(lane).asLite :=> source.asLite
          Some(PeSharedHardCilkOutput(taskName, peIndex, group.portName,
            group.logicalWidth, source))
        case AiePlioSourceKind.PackedOutput =>
          outputSplitters.find(split => split.physicalInput.group.taskName == taskName &&
            split.physicalInput.group.peIndex == peIndex &&
            split.physicalInput.group.endpointName == group.subPEName.toLowerCase.replaceAll("[^a-z0-9_]", "") &&
            split.physicalInput.group.aiePortType == group.portName) match {
            case Some(split) =>
              require(split.physicalInput.sink.cfg.wData == group.logicalWidth,
                s"Output group ${group.name} width does not match tagged splitter input")
              dut.packetOut(lane).asLite :=> split.physicalInput.sink.asLite
            case None =>
              val name = s"${group.name}_packed_$peIndex"
              val source = IO(axi4s.Master(cfg.outputCfg)).suggestName(name)
              dut.packetOut(lane).asLite :=> source.asLite
              packed += PeSharedPackedOutput(name, source)
          }
          None
        case AiePlioSourceKind.RwRequest =>
          val request = rwPorts.find(p => p.subPEName == group.subPEName && p.index == peIndex)
            .getOrElse(throw new IllegalArgumentException(
              s"AIE PLIO group '${group.name}' has no read/write helper for PE $peIndex"))
          dut.packetOut(lane).asLite :=> request.sourceTask.asLite
          None
        case other =>
          throw new IllegalArgumentException(
            s"AIE PLIO group '${group.name}' has unsupported output source kind '$other'")
      }
    }
    (PeSharedPacketInput(group.effectivePhysicalPortName, physical), logical, packed.toSeq)
  }

  val sharedPacketInputs: Seq[PeSharedPacketInput] = outputDemuxes.map(_._1)
  val sharedHardCilkOutputs: Seq[PeSharedHardCilkOutput] = outputDemuxes.flatMap(_._2)
  val sharedPackedOutputs: Seq[PeSharedPackedOutput] = outputDemuxes.flatMap(_._3)
  val sharedRwRequestKeys: Set[(String, Int)] = sharingGroups
    .filter(_.sourceKind == AiePlioSourceKind.RwRequest)
    .flatMap(group => group.peIndices.map(index => (group.subPEName, index))).toSet
}
