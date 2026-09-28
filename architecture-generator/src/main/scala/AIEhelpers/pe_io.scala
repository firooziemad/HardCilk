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

case class PeSharedPacketOutput(
    groupName: String,
    physicalPortName: String,
    source: axi4s.Interface
)

class PeIO(
    subPEs: Map[String, SubPEDescriptor],
    count: Int,
    taskName: String = "",
    sharingGroups: Seq[AIEPlioPhysicalSharingGroupDescriptor] = Seq.empty
) extends Module {
  require(count >= 0, "count must be non-negative")
  require(sharingGroups.forall(_.taskName == taskName),
    s"Every sharing group passed to PeIO($taskName) must belong to task '$taskName'")

  private def isSharedReadResponse(subPEName: String, index: Int): Boolean =
    sharingGroups.exists { group =>
      group.sourceKind == AiePlioSourceKind.ReadResponse &&
      group.sourceSubPEName == subPEName &&
      group.peIndices.contains(index)
    }

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
                IO(axi4s.Slave(cfg.inputCfg)).suggestName(s"${instanceName}_sourceTask")
              val internalSinkResult =
                e.EWire(new axi4s.FullChannel(cfg.outputCfg))
              val m_axi =
                IO(axi4.Master(cfg.axiCfg)).suggestName(s"${instanceName}_m_axi")

              sourceTask.asLite :=> dut.sourceTask.asLite
              dut.sinkResult.asFull :=> internalSinkResult
              dut.m_axi :=> m_axi.asFull

              val sinkResult =
                if (isSharedReadResponse(name, index)) {
                  None
                } else {
                  val external = IO(axi4s.Master(cfg.outputCfg))
                    .suggestName(s"${instanceName}_sinkResult")
                  internalSinkResult :=> external.asFull
                  Some(external)
                }

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
                IO(axi4s.Slave(cfg.inputCfg)).suggestName(s"${instanceName}_sourceTask")
              val internalSinkResult =
                e.EWire(new axi4s.FullChannel(cfg.outputCfg))
              val m_axi =
                IO(axi4.Master(cfg.axiCfg)).suggestName(s"${instanceName}_m_axi")

              sourceTask.asLite :=> dut.sourceTask.asLite
              dut.sinkResult.asFull :=> internalSinkResult
              dut.m_axi :=> m_axi.asFull

              val sinkResult =
                if (isSharedReadResponse(name, index)) {
                  None
                } else {
                  val external = IO(axi4s.Master(cfg.outputCfg))
                    .suggestName(s"${instanceName}_sinkResult")
                  internalSinkResult :=> external.asFull
                  Some(external)
                }

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
                IO(axi4s.Slave(cfg.inputCfg)).suggestName(s"${instanceName}_sourceTask")
              val m_axi =
                IO(axi4.Master(cfg.axiCfg)).suggestName(s"${instanceName}_m_axi")

              sourceTask.asLite :=> dut.sourceTask.asLite
              dut.m_axi :=> m_axi.asFull

              PeRwPort(name, index, sourceTask, None, None, m_axi)

            case other =>
              throw new Exception(s"Unsupported rwRequest for subPE $name: $other")
          }
        }
      }
    }

  val axiMasters: Seq[axi4.full.Interface] =
    rwPorts.map(_.m_axi.asFull)

  private case class PacketizerResult(
      hardCilkInputs: Seq[PeSharedHardCilkInput],
      packetOutput: PeSharedPacketOutput,
      readResponseKeys: Set[(String, Int)]
  )

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

  private val packetizers = sharingGroups.map { group =>
    group.mode match {
      case AiePlioSharingMode.Single =>
        val cfg = AiePacketMuxSingle_Config(
          group.packetIds,
          group.logicalWidth,
          group.fifoDepth
        )
        val dut = Module(new AiePacketMuxSingle_Basic(cfg))
        dut.suggestName(s"${group.name}_singlePacketMux")
        connectPacketMux(
          group,
          cfg.inputCfg,
          cfg.packetCfg,
          dut.packetIn.toSeq,
          dut.packetOut,
          streamMode = false
        )

      case AiePlioSharingMode.Stream =>
        val cfg = AiePacketMuxStream_Config(
          packetIds = group.packetIds,
          dataWidth = group.logicalWidth,
          fifoDepth = group.fifoDepth,
          outputWidth = group.physicalPlioWidth
        )
        val dut = Module(new AiePacketMuxStream_Basic(cfg))
        dut.suggestName(s"${group.name}_streamPacketMux")
        connectPacketMux(
          group,
          cfg.inputCfg,
          cfg.packetCfg,
          dut.packetIn.toSeq,
          dut.packetOut,
          streamMode = true
        )

      case other =>
        throw new IllegalArgumentException(
          s"AIE PLIO sharing group '${group.name}' requested unknown mode '$other'"
        )
    }
  }

  val sharedHardCilkInputs: Seq[PeSharedHardCilkInput] =
    packetizers.flatMap(_.hardCilkInputs)

  val sharedReadResponseKeys: Set[(String, Int)] =
    packetizers.flatMap(_.readResponseKeys).toSet

  val sharedPacketOutputs: Seq[PeSharedPacketOutput] =
    packetizers.map(_.packetOutput)
}
