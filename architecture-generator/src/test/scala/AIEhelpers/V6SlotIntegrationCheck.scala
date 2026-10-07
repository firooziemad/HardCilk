package aiehelpers

import Descriptors._
import Descriptors.DescriptorJSON._
import circt.stage.ChiselStage

/** Elaborate PeIO credit wiring for every shared task; no synthesis or simulation. */
object V6SlotIntegrationCheck extends App {
  args.foreach { path =>
    val d = parseJsonFile[FullSysGenDescriptor](path)
    d.validate()
    val connections = d.getSystemConnectionsDescriptor().connections
    d.taskDescriptors.foreach { task =>
      val groups = d.expandedAiePlioSharingGroups.filter(_.taskName == task.name)
      if (groups.nonEmpty) {
        // Synthetic IDs are for elaboration only; builds still require compiler IDs.
        groups.foreach(g => g.resolvePacketIds(0 until g.fanIn))
        val splits = StreamSplitterPlan.groups(d).filter(_.taskName == task.name)
        val direct = groups.filter(g => !g.creditReturnPort.startsWith("read") &&
          !g.creditReturnPort.startsWith("write")).flatMap { g =>
          g.peIndices.filterNot(i => splits.exists(s => s.peIndex == i &&
            s.creditGroups.exists(_.name == g.name))).map { i =>
            val widths = connections.filter(c => c.srcPort.parentType == "PE" &&
              c.srcPort.parentName == task.name && c.srcPort.parentIndex == i &&
              c.srcPort.portType == g.creditReturnPort && c.dstPort.parentType == "HardCilk").map(_.bitWidth)
            require(widths.nonEmpty)
            StreamSplitterDataRoute(task.name, i, g.subPEName, g.creditReturnPort, widths.max)
          }
        }.distinct
        val chirrtl = ChiselStage.emitCHIRRTL(new PeIO(
          d.subPEList.filter(_._2.peName == task.name), task.numProcessingElements,
          task.name, groups, splits, direct))
        require(chirrtl.contains("PeIO"))
        println(s"PASS V6 slots PeIO elaboration: ${d.name}/${task.name}, ${groups.size} groups")
      }
    }
  }
}
