package SoftwareUtil.aie

import Descriptors._
import aiehelpers.{V5CreditProtocol, StreamSplitterPlan}
import java.io.PrintWriter
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets
import scala.collection.mutable

// TODO: PLIO width is not properly handled
object ProjectHeaderTemplate {

  private case class UnitDef(
      unitName: String,
      functionName: String,
      taskName: String,
      peIndex: Int,
      peCount: Int,
      taskInputPorts: Seq[String],
      taskOutputPorts: Seq[String],
      rwInputPorts: Seq[String],
      rwOutputPorts: Seq[String],
      prevChainUnitName: Option[String],
      nextChainUnitName: Option[String]
  )

  private case class PortDef(
      varName: String,
      plioName: String,
      direction: String,
      bits: Int
  )

  private case class HelperSubPE(
      name: String,
      normalizedName: String,
      req: Option[RWRequestDescriptor],
      taskInputPorts: Seq[String],
      taskOutputPorts: Seq[String]
  )

  private case class StreamSplitterGroupDef(
      groupName: String,
      taskName: String,
      peIndex: Int,
      unitName: String,
      outputPortTypes: Seq[String],
      outputWidths: Seq[Int]
  )

  private case class CreditBinding(
      group: AIEPlioPhysicalSharingGroupDescriptor,
      laneIndex: Int,
      unitName: String,
      aliasName: String
  )

  def generateProjectHeader(descriptor: FullSysGenDescriptor, projectFolder: String): Unit = {
    val units = buildUnits(descriptor)
    val portBitWidthsByTaskPePort = buildHardCilkPortBitWidthsByTaskPePort(descriptor)
    val logicalPorts = buildPortDefs(descriptor, units, portBitWidthsByTaskPePort)
    val sharingGroups = descriptor.expandedAiePlioSharingGroups
    require(!sys.env.contains("HC_EXP1_ROUTE_CASE") &&
      !sys.env.get("HC_AIE_EXPLICIT_DMA").contains("1"),
      "V6 slot-credit mode does not use legacy routing overrides")
    val creditGroups = sharingGroups.filter(_.direction == "plToAie")
    val creditBindings = buildCreditBindings(units, creditGroups)
    generateManualCreditConfig(projectFolder, units, descriptor, creditBindings)
    val sharedMembers = sharingGroups.flatMap(_.members).toSet
    val portDirection = logicalPorts.map(p => p.varName -> p.direction).toMap
    sharingGroups.foreach { group =>
      val requiredDirection = if (group.direction == "plToAie") "input" else "output"
      val invalid = group.members.filter(m => portDirection.get(m) != Some(requiredDirection))
      require(invalid.isEmpty,
        s"AIE PLIO sharing group '${group.name}' requires generated $requiredDirection PLIOs: ${invalid.mkString(", ")}")
    }
    val inputGroups = sharingGroups.filter(_.direction == "plToAie")
    val outputGroups = sharingGroups.filter(_.direction == "aieToPl")

    val physicalPorts = sharingGroups.map { group =>
      PortDef(
        varName = group.effectivePhysicalPortName,
        plioName = s"PLIO_${group.effectivePhysicalPortName}",
        direction = if (group.direction == "plToAie") "input" else "output",
        // Vitis 2022.2 can acknowledge a packet which starts and ends in one
        // partially-filled wide PLIO beat without delivering its payload past
        // the packet splitter. This is observable for a 64-bit fixed record:
        // the PL emits header+payload as 96 valid bits on a 128-bit PLIO, the
        // shim accepts TKEEP=0x0fff/TLAST, but the destination kernel remains
        // blocked. A 32-bit packet PLIO gives the header and every payload
        // word an unambiguous full-width beat. The PL-side packet mux may
        // remain wider; v++ inserts the width converter at the PL/AIE boundary.
        bits = group.physicalPlioWidth
      )
    }
    val ports = (logicalPorts.filterNot(port => sharedMembers.contains(port.varName)) ++ physicalPorts)
      .sortBy(_.varName)

    val lines =
      Seq(
        "#include <adf.h>",
        "#include \"kernels.h\""
      ) ++ Seq(
        "",
        "using namespace adf;",
        "",
        "class simpleGraph : public adf::graph {",
        "private:"
      ) ++
        buildKernelDeclarations(units) ++
        Seq("public:") ++
        buildPortDeclarations(ports) ++
		buildPacketSplitterDeclarations(inputGroups) ++
        outputGroups.map(g => s"  pktmerge<${g.fanIn}> ${g.name}_merge;") ++
        Seq("  simpleGraph(){") ++
        Seq("    // PLIOs") ++
        buildPlioCreations(ports) ++
		buildPacketSplitterCreates(inputGroups) ++
        outputGroups.map(g => s"    ${g.name}_merge = pktmerge<${g.fanIn}>::create();") ++
        Seq("", "    // Kernels") ++
        buildKernelCreates(units, creditBindings) ++
        Seq("", "    // Connections") ++
        buildConnections(units, descriptor) ++
        Seq("  }", "};")

    val writer = new PrintWriter(s"$projectFolder/project.h")
    try {
      writer.write(lines.mkString("\n") + "\n")
    } finally {
      writer.close()
    }
    if (outputGroups.nonEmpty) {
      val packetRoute = raw".*connect< pktstream > net[0-9]+ \(([A-Za-z0-9_]+)_kernel\.out\[([0-9]+)\], ([A-Za-z0-9_]+)_merge\.in\[([0-9]+)\]\);".r
      val widths = outputGroups.map(g => g.name -> (g.logicalWidth / 32)).toMap
      val functions = units.map(u => u.unitName -> u.functionName).toMap
      val routes = lines.collect { case packetRoute(unit, port, group, _) =>
        require(functions.contains(unit) && widths.contains(group),
          s"Unknown direct packet route $unit -> $group")
        s"$unit\t${functions(unit)}\t$port\t${widths(group)}"
      }
      require(routes.length == outputGroups.map(_.fanIn).sum,
        s"Expected ${outputGroups.map(_.fanIn).sum} direct packet routes, found ${routes.length}")
      Files.writeString(Paths.get(projectFolder, "direct_packet_outputs.tsv"),
        routes.mkString("\n") + "\n", StandardCharsets.UTF_8)
    }
  }

  private def buildUnits(descriptor: FullSysGenDescriptor): Seq[UnitDef] = {
    val hardCilkPortsByTaskPe = buildHardCilkPortsByTaskPe(descriptor)
    val units = mutable.ArrayBuffer[UnitDef]()

    descriptor.taskDescriptors.sortBy(_.name).foreach { task =>
      val orderedSubPEs = getOrderedSubPEsForTask(descriptor, task.name)

      (0 until task.numProcessingElements).foreach { peIndex =>
        val peSuffix = suffixForPe(peIndex, task.numProcessingElements)
        val hardPorts = hardCilkPortsByTaskPe.getOrElse((task.name, peIndex), (Seq.empty[String], Seq.empty[String]))
        val hardInputs = hardPorts._1
        val hardOutputs = hardPorts._2

        if (orderedSubPEs.nonEmpty) {
          val incomingReadPortsBySubPE = buildIncomingReadPorts(orderedSubPEs)
          val byName = orderedSubPEs.map(s => s.name -> s).toMap
          val prevByName = mutable.Map[String, String]()

          orderedSubPEs.foreach { subPE =>
            subPE.req.flatMap(_.nextsubPE).foreach { nextSubPE =>
              if (byName.contains(nextSubPE)) {
                prevByName(nextSubPE) = subPE.name
              }
            }
          }

          orderedSubPEs.zipWithIndex.foreach { case (subPE, chainIndex) =>
            val isLast = chainIndex == orderedSubPEs.length - 1
            val taskInputOwnerByPort = buildTaskInputOwnerByPort(hardInputs, orderedSubPEs)
            val taskOutputOwnerByPort = buildTaskOutputOwnerByPort(hardOutputs, orderedSubPEs)

            val taskInputs = {
              val base = Seq("taskIn")
              val exportedForThisSubPE = hardInputs.filter { port =>
                port != "taskIn" && taskInputOwnerByPort.get(port).contains(subPE.name)
              }
              uniquePreserveOrder(base ++ exportedForThisSubPE)
            }

            val taskOutputs = {
              val forwarded = if (!isLast) Seq("taskOutInternal") else Seq.empty
              val exported = hardOutputs.filter { port =>
                port != "taskOut" && taskOutputOwnerByPort.get(port).contains(subPE.name)
              } ++ {
                if (taskOutputOwnerByPort.get("taskOut").contains(subPE.name)) Seq("taskOut") else Seq.empty
              }
              uniquePreserveOrder(forwarded ++ exported)
            }

            val rwIn = incomingReadPortsBySubPE.getOrElse(subPE.name, Seq.empty)
            val rwOut = rwOutputPorts(subPE.req)
            val prevUnitName = prevByName.get(subPE.name).flatMap(byName.get).map(s => s"${s.normalizedName}$peSuffix")
            val nextUnitName = subPE.req.flatMap(_.nextsubPE).flatMap(byName.get).map(s => s"${s.normalizedName}$peSuffix")

            units += UnitDef(
              unitName = s"${subPE.normalizedName}$peSuffix",
              functionName = subPE.normalizedName,
              taskName = task.name,
              peIndex = peIndex,
              peCount = task.numProcessingElements,
              taskInputPorts = taskInputs,
              taskOutputPorts = taskOutputs,
              rwInputPorts = rwIn,
              rwOutputPorts = rwOut,
              prevChainUnitName = prevUnitName,
              nextChainUnitName = nextUnitName
            )
          }
        } else {
          val taskInputs = uniquePreserveOrder(Seq("taskIn") ++ hardInputs.filterNot(_ == "taskIn"))
          val taskOutputs = uniquePreserveOrder(hardOutputs)

          units += UnitDef(
            unitName = s"${task.name}$peSuffix",
            functionName = task.name,
            taskName = task.name,
            peIndex = peIndex,
            peCount = task.numProcessingElements,
            taskInputPorts = taskInputs,
            taskOutputPorts = taskOutputs,
            rwInputPorts = Seq.empty,
            rwOutputPorts = Seq.empty,
            prevChainUnitName = None,
            nextChainUnitName = None
          )
        }
      }
    }

    units.toSeq
  }

  private def buildHardCilkPortsByTaskPe(
      descriptor: FullSysGenDescriptor
  ): Map[(String, Int), (Seq[String], Seq[String])] = {
    val inPorts = mutable.Map[(String, Int), mutable.ArrayBuffer[String]]()
    val outPorts = mutable.Map[(String, Int), mutable.ArrayBuffer[String]]()

    descriptor.getSystemConnectionsDescriptor().connections.foreach { connection =>
      val src = connection.srcPort
      val dst = connection.dstPort

      if (src.parentType == "HardCilk" && dst.parentType == "PE") {
        val key = (dst.parentName, dst.parentIndex)
        val buf = inPorts.getOrElseUpdate(key, mutable.ArrayBuffer[String]())
        buf += dst.portType
      } else if (src.parentType == "PE" && dst.parentType == "HardCilk") {
        val key = (src.parentName, src.parentIndex)
        val buf = outPorts.getOrElseUpdate(key, mutable.ArrayBuffer[String]())
        buf += src.portType
      }
    }

    // Write-buffer exports are not always present in the static PE<->HardCilk connection list.
    // Add them explicitly so project PLIOs include argDataOut/spawnNext when generated.
    descriptor.taskDescriptors.foreach { task =>
      (0 until task.numProcessingElements).foreach { peIndex =>
        val key = (task.name, peIndex)
        val buf = outPorts.getOrElseUpdate(key, mutable.ArrayBuffer[String]())
        if (task.generateArgOutWriteBuffer) {
          buf += "argDataOut"
        }
        if (task.generateSpawnNextWriteBuffer) {
          buf += "spawnNext"
        }
      }
    }

    (inPorts.keySet ++ outPorts.keySet).map { key =>
      val in = uniquePreserveOrder(inPorts.getOrElse(key, mutable.ArrayBuffer.empty[String]).toSeq)
      val out = uniquePreserveOrder(outPorts.getOrElse(key, mutable.ArrayBuffer.empty[String]).toSeq)
      key -> (in, out)
    }.toMap
  }

  private def buildHardCilkPortBitWidthsByTaskPePort(
      descriptor: FullSysGenDescriptor
  ): Map[(String, Int, String), Int] = {
    val widths = mutable.Map[(String, Int, String), Int]()

    descriptor.getSystemConnectionsDescriptor().connections.foreach { connection =>
      val src = connection.srcPort
      val dst = connection.dstPort

      if (src.parentType == "HardCilk" && dst.parentType == "PE") {
        widths((dst.parentName, dst.parentIndex, dst.portType)) = connection.bitWidth
      } else if (src.parentType == "PE" && dst.parentType == "HardCilk") {
        widths((src.parentName, src.parentIndex, src.portType)) = connection.bitWidth
      }
    }

    descriptor.taskDescriptors.foreach { task =>
      val spawnNextWidth = getSpawnNextBundleWidthBits(descriptor, task)
      val argDataOutWidth = getArgDataOutBundleWidthBits(descriptor, task)
      (0 until task.numProcessingElements).foreach { peIndex =>
        if (task.generateArgOutWriteBuffer) {
          widths((task.name, peIndex, "argDataOut")) = argDataOutWidth
        }
        if (task.generateSpawnNextWriteBuffer) {
          widths((task.name, peIndex, "spawnNext")) = spawnNextWidth
        }
      }
    }

    widths.toMap
  }

  private def getOrderedSubPEsForTask(descriptor: FullSysGenDescriptor, taskName: String): Seq[HelperSubPE] = {
    val subpes = descriptor.subPEList
      .toSeq
      .filter { case (_, sub) => sub.peName == taskName }
      .map { case (name, sub) =>
        HelperSubPE(
          name = name,
          normalizedName = normalizeName(name),
          req = sub.rwRequest
          ,
          taskInputPorts = sub.taskInputPorts,
          taskOutputPorts = sub.taskOutputPorts
        )
      }

    if (subpes.isEmpty) {
      Seq.empty
    } else {
      val byName = subpes.map(s => s.name -> s).toMap
      val incomingTargets = subpes.flatMap(_.req.flatMap(_.nextsubPE)).toSet
      val heads = subpes.filterNot(s => incomingTargets.contains(s.name)).sortBy(_.name)

      val ordered = mutable.ArrayBuffer[HelperSubPE]()
      val visited = mutable.Set[String]()

      def walk(start: HelperSubPE): Unit = {
        var current = Option(start)
        while (current.nonEmpty && !visited.contains(current.get.name)) {
          val value = current.get
          ordered += value
          visited += value.name
          current = value.req.flatMap(_.nextsubPE).flatMap(byName.get)
        }
      }

      heads.foreach(walk)
      subpes.sortBy(_.name).filterNot(s => visited.contains(s.name)).foreach(walk)
      ordered.toSeq
    }
  }

  private def buildIncomingReadPorts(orderedSubPEs: Seq[HelperSubPE]): Map[String, Seq[String]] = {
    val incoming = mutable.Map[String, mutable.ArrayBuffer[String]]()

    orderedSubPEs.foreach { sub =>
      sub.req.foreach { req =>
        if (req.`type` == "read") {
          req.nextsubPE.foreach { next =>
            val portName = rwInPortName(req)
            val buf = incoming.getOrElseUpdate(next, mutable.ArrayBuffer[String]())
            buf += portName
          }
        }
      }
    }

    incoming.map { case (name, buf) =>
      name -> uniquePreserveOrder(buf.toSeq)
    }.toMap
  }

  private def buildTaskInputOwnerByPort(
      hardInputs: Seq[String],
      orderedSubPEs: Seq[HelperSubPE]
  ): Map[String, String] = {
    if (orderedSubPEs.isEmpty) {
      Map.empty
    } else {
      val defaultOwner = orderedSubPEs.head.name
      val owners = mutable.Map[String, String](hardInputs.map(_ -> defaultOwner): _*)
      orderedSubPEs.foreach { sub =>
        sub.taskInputPorts.foreach { port =>
          if (hardInputs.contains(port)) {
            owners(port) = sub.name
          }
        }
      }
      owners.toMap
    }
  }

  private def buildTaskOutputOwnerByPort(
      hardOutputs: Seq[String],
      orderedSubPEs: Seq[HelperSubPE]
  ): Map[String, String] = {
    if (orderedSubPEs.isEmpty) {
      Map.empty
    } else {
      val defaultOwner = orderedSubPEs.last.name
      val owners = mutable.Map[String, String](hardOutputs.map(_ -> defaultOwner): _*)
      orderedSubPEs.foreach { sub =>
        sub.taskOutputPorts.foreach { port =>
          if (hardOutputs.contains(port)) {
            owners(port) = sub.name
          }
        }
      }
      owners.toMap
    }
  }

  private def rwOutputPorts(req: Option[RWRequestDescriptor]): Seq[String] = {
    req.toSeq.flatMap { request =>
      request.`type` match {
        case "read"  => Seq(rwOutPortName("read", request.mode, request.portWidth))
        case "write" => Seq(rwOutPortName("write", request.mode, request.portWidth))
        case _        => Seq.empty
      }
    }
  }

  private def rwInPortName(req: RWRequestDescriptor): String =
    s"read${modeCap(req.mode)}${req.portWidth}In"

  private def rwOutPortName(requestType: String, mode: String, portWidth: Int): String =
    s"${requestType}${modeCap(mode)}${portWidth}Out"

  private def modeCap(mode: String): String =
    if (mode == "single") "Single" else "Stream"

  private def buildPortDefs(
      descriptor: FullSysGenDescriptor,
      units: Seq[UnitDef],
      portBitWidthsByTaskPePort: Map[(String, Int, String), Int]
  ): Seq[PortDef] = {
    val taskByName = descriptor.taskDescriptors.map(t => t.name -> t).toMap
    val allPorts = mutable.ArrayBuffer[PortDef]()

    units.foreach { unit =>
      val task = taskByName(unit.taskName)
      val inputPorts = uniquePreserveOrder(
        orderTaskInputPorts(unit.taskInputPorts) ++ unit.rwInputPorts.sorted)
      val taskOutputPorts = orderTaskOutputPorts(unit.taskOutputPorts)
      val rwOutputPorts = unit.rwOutputPorts.sorted
      val outputPorts = uniquePreserveOrder(taskOutputPorts ++ rwOutputPorts)
      val groupedOutputGroups = buildStreamSplitterGroups(descriptor)
        .filter(group => group.taskName == unit.taskName && group.peIndex == unit.peIndex &&
          group.unitName == unit.unitName)
        .map(_.outputPortTypes.filter(outputPorts.contains))
        .filter(_.nonEmpty)
      val groupedOutputSet = groupedOutputGroups.flatten.toSet

      inputPorts.foreach { p =>
        val isInternalChainInput = p == "taskIn" && unit.prevChainUnitName.nonEmpty
        if (!isInternalChainInput) {
          val varName = s"${unit.unitName}_$p"
          allPorts += PortDef(
            varName = varName,
            plioName = s"PLIO_$varName",
            direction = "input",
            bits = resolvePortWidthBits(
              task = task,
              peIndex = unit.peIndex,
              portName = p,
              isInput = true,
              portBitWidthsByTaskPePort = portBitWidthsByTaskPePort
            )
          )
        }
      }

      outputPorts.foreach { p =>
        val isInternalChainOutput = p == "taskOutInternal" && unit.nextChainUnitName.nonEmpty
        val isGroupedOutput = groupedOutputSet.contains(p)
        if (!isInternalChainOutput && !isGroupedOutput) {
          val varName = s"${unit.unitName}_$p"
          allPorts += PortDef(
            varName = varName,
            plioName = s"PLIO_$varName",
            direction = "output",
            bits = resolvePortWidthBits(
              task = task,
              peIndex = unit.peIndex,
              portName = p,
              isInput = false,
              portBitWidthsByTaskPePort = portBitWidthsByTaskPePort
            )
          )
        }
      }

      groupedOutputGroups.foreach { groupedOutputPorts =>
        val varName = packedOutputVarName(unit, groupedOutputPorts)
        val packedBits = toSupportedPlioBits(groupedOutputPorts.map { port =>
          resolvePortWidthBits(
            task = task,
            peIndex = unit.peIndex,
            portName = port,
            isInput = false,
            portBitWidthsByTaskPePort = portBitWidthsByTaskPePort
          )
        }.sum)

        allPorts += PortDef(
          varName = varName,
          plioName = s"PLIO_$varName",
          direction = "output",
          bits = packedBits
        )
      }
    }

    uniquePortDefs(allPorts.toSeq).sortBy(_.varName)
  }

  private def uniquePortDefs(ports: Seq[PortDef]): Seq[PortDef] = {
    val seen = mutable.Set[String]()
    ports.filter { p =>
      if (seen.contains(p.varName)) false
      else {
        seen += p.varName
        true
      }
    }
  }

  private def buildKernelDeclarations(units: Seq[UnitDef]): Seq[String] =
    units.map(u => s"  kernel ${u.unitName}_kernel;")

  private def buildPortDeclarations(ports: Seq[PortDef]): Seq[String] =
    ports.map { p =>
      val kind = if (p.direction == "input") "input_plio" else "output_plio"
      s"  $kind ${p.varName};"
    }

  private def buildPacketSplitterDeclarations(groups: Seq[AIEPlioPhysicalSharingGroupDescriptor]): Seq[String] =
    groups.map(group => s"  pktsplit<${group.fanIn}> ${group.name}_split;")

  private def buildPlioCreations(ports: Seq[PortDef]): Seq[String] =
    ports.map { p =>
      val creator = if (p.direction == "input") "input_plio::create" else "output_plio::create"
      s"    ${p.varName} = $creator(\"${p.plioName}\", adf::plio_${p.bits}_bits, \"data/${p.varName}.txt\");"
    }

  private def buildPacketSplitterCreates(groups: Seq[AIEPlioPhysicalSharingGroupDescriptor]): Seq[String] =
    groups.map(group => s"    ${group.name}_split = pktsplit<${group.fanIn}>::create();")

  private def buildKernelCreates(
      units: Seq[UnitDef], bindings: Seq[CreditBinding]): Seq[String] = {
    val bindingByUnit = bindings.groupBy(_.unitName).view.mapValues(_.head).toMap
    units.flatMap { u =>
      val functionName = u.functionName
      Seq(
        s"    ${u.unitName}_kernel = kernel::create($functionName);",
        s"    source(${u.unitName}_kernel) = \"kernels/$functionName.cc\";",
        s"    runtime<ratio>(${u.unitName}_kernel) = 1;"
      )
    }
  }

  private def buildConnections(units: Seq[UnitDef], descriptor: FullSysGenDescriptor): Seq[String] = {
    val lines = mutable.ArrayBuffer[String]()
    var net = 0
    val unitsByName = units.map(u => u.unitName -> u).toMap
	val sharedMemberRoutes = descriptor.expandedAiePlioSharingGroups.filter(_.direction == "plToAie").flatMap { group =>
	  group.members.zipWithIndex.map { case (member, branchIndex) =>
		member -> (group, branchIndex)
	  }
	}.toMap

	descriptor.expandedAiePlioSharingGroups.filter(_.direction == "plToAie").zipWithIndex.foreach { case (group, groupIndex) =>
	  lines += s"    connect< pktstream > sharedPhysicalNet$groupIndex (${group.effectivePhysicalPortName}.out[0], ${group.name}_split.in[0]);"
	}

    val outputRoutes = descriptor.expandedAiePlioSharingGroups.filter(_.direction == "aieToPl")
      .flatMap(g => g.members.zipWithIndex.map { case (member, index) => member -> (g, index) }).toMap
    descriptor.expandedAiePlioSharingGroups.filter(_.direction == "aieToPl").zipWithIndex.foreach { case (group, index) =>
      lines += s"    connect< pktstream > sharedOutputNet$index (${group.name}_merge.out[0], ${group.effectivePhysicalPortName}.in[0]);"
    }

    units.foreach { u =>
      val inputPorts = uniquePreserveOrder(
        orderTaskInputPorts(u.taskInputPorts) ++ u.rwInputPorts.sorted)
      val taskOutputPorts = orderTaskOutputPorts(u.taskOutputPorts)
      val rwOutputPorts = u.rwOutputPorts.sorted
      val outputPorts = uniquePreserveOrder(taskOutputPorts ++ rwOutputPorts)
      val groupedOutputGroups = buildStreamSplitterGroups(descriptor)
        .filter(group => group.taskName == u.taskName && group.peIndex == u.peIndex &&
          group.unitName == u.unitName)
        .map(_.outputPortTypes.filter(outputPorts.contains))
        .filter(_.nonEmpty)
      val groupedOutputSet = groupedOutputGroups.flatten.toSet
      val groupedOutputByPort = groupedOutputGroups.flatMap(group => group.map(_ -> group)).toMap

      // Build consolidated output list treating groups as single entries
      val consolidatedOutputs = mutable.ArrayBuffer[String]()
      val portToConsolidatedIndex = mutable.Map[String, Int]()
      var consolidatedIdx = 0

      outputPorts.foreach { port =>
        if (!groupedOutputSet.contains(port)) {
          // Not in a group, add directly
          consolidatedOutputs += port
          portToConsolidatedIndex(port) = consolidatedIdx
          consolidatedIdx += 1
        } else {
          // In a per-unit group, map all grouped ports to the same consolidated index.
          val group = groupedOutputByPort(port)
          val groupRepr = group.head
          if (!portToConsolidatedIndex.contains(groupRepr)) {
            consolidatedOutputs += groupRepr
            portToConsolidatedIndex(groupRepr) = consolidatedIdx
            group.foreach { p =>
              portToConsolidatedIndex(p) = consolidatedIdx
            }
            consolidatedIdx += 1
          }
        }
      }

      inputPorts.zipWithIndex.foreach { case (port, inIdx) =>
        if (port == "taskIn" && u.prevChainUnitName.nonEmpty) {
          val prevUnit = unitsByName(u.prevChainUnitName.get)
          val prevOutputPorts = uniquePreserveOrder(
            orderTaskOutputPorts(prevUnit.taskOutputPorts) ++ prevUnit.rwOutputPorts.sorted)
          val prevTaskOutIdx = {
            val internalIdx = prevOutputPorts.indexOf("taskOutInternal")
            if (internalIdx >= 0) internalIdx else prevOutputPorts.indexOf("taskOut")
          }
          if (prevTaskOutIdx >= 0) {
            lines += s"    connect< stream > net$net (${prevUnit.unitName}_kernel.out[$prevTaskOutIdx], ${u.unitName}_kernel.in[$inIdx]);"
            net += 1
          }
        } else {
          val plioVar = s"${u.unitName}_$port"
          sharedMemberRoutes.get(plioVar) match {
            case Some((group, branchIndex)) =>
              lines += s"    connect< pktstream, stream > net$net (${group.name}_split.out[$branchIndex], ${u.unitName}_kernel.in[$inIdx]);"
              // Equal depth requests on packet-split branches can be merged by
              // aiecompiler. Distinct depths keep each receive slot on its
              // own FIFO; the routed-FIFO gate verifies the actual resources.
              lines += s"    fifo_depth(net$net) = ${group.receiveFifoDepths(branchIndex)};"
              lines += s"    location<fifo>(net$net) = {ss_fifo()};"
            case None =>
              lines += s"    connect< stream > net$net (${plioVar}.out[0], ${u.unitName}_kernel.in[$inIdx]);"
          }
          net += 1
        }
      }

      outputPorts.foreach { port =>
        val isInternalChainOutput = port == "taskOutInternal" && u.nextChainUnitName.nonEmpty
        val isGroupedOutput = groupedOutputSet.contains(port)
        if (!isInternalChainOutput && !isGroupedOutput) {
          val plioVar = s"${u.unitName}_$port"
          val consolidatedIdx = portToConsolidatedIndex(port)
          val route = outputRoutes.get(plioVar)
          val dest = route.map { case (group, branch) => s"${group.name}_merge.in[$branch]" }.getOrElse(s"${plioVar}.in[0]")
          val kind = if (route.nonEmpty) "pktstream" else "stream"
          lines += s"    connect< $kind > net$net (${u.unitName}_kernel.out[$consolidatedIdx], $dest);"
          net += 1
        }
      }

      // Handle per-unit grouped PLIO output.
      groupedOutputGroups.foreach { groupedOutputPorts =>
        val groupRepr = groupedOutputPorts.head
        val packedVarName = packedOutputVarName(u, groupedOutputPorts)
        portToConsolidatedIndex.get(groupRepr).foreach { consolidatedIdx =>
          val route = outputRoutes.get(packedVarName)
          val dest = route.map { case (group, branch) => s"${group.name}_merge.in[$branch]" }.getOrElse(s"${packedVarName}.in[0]")
          val kind = if (route.nonEmpty) "pktstream" else "stream"
          lines += s"    connect< $kind > net$net (${u.unitName}_kernel.out[$consolidatedIdx], $dest);"
          net += 1
        }
      }
    }

    lines.toSeq
  }

  private def orderTaskInputPorts(ports: Seq[String]): Seq[String] = {
    val order = Seq("taskIn", "taskInGlobal", "argIn", "closureIn", "mallocIn")
    orderByReferenceThenName(ports, order)
  }

  private def orderTaskOutputPorts(ports: Seq[String]): Seq[String] = {
    val order = Seq("taskOutInternal", "taskOut", "taskOutGlobal", "argOut", "closureOut", "mallocOut", "argDataOut", "spawnNext")
    orderByReferenceThenName(ports, order)
  }

  private def orderByReferenceThenName(ports: Seq[String], order: Seq[String]): Seq[String] = {
    val unique = uniquePreserveOrder(ports)
    val orderMap = order.zipWithIndex.toMap
    unique.sortBy(p => (orderMap.getOrElse(p, Int.MaxValue), p))
  }

  private def uniquePreserveOrder(values: Seq[String]): Seq[String] = {
    val seen = mutable.Set[String]()
    values.filter { v =>
      if (seen.contains(v)) false
      else {
        seen += v
        true
      }
    }
  }

  private def suffixForPe(peIndex: Int, peCount: Int): String =
    if (peCount > 1) s"_$peIndex" else ""

  private def normalizeName(value: String): String =
    value.toLowerCase.replaceAll("[^a-z0-9_]", "")

  private def packedOutputVarName(unit: UnitDef, groupedOutputPorts: Seq[String]): String =
    s"${unit.unitName}_${groupedOutputPorts.mkString("_")}"

  private def buildStreamSplitterGroups(descriptor: FullSysGenDescriptor): Seq[StreamSplitterGroupDef] = {
    StreamSplitterPlan.groups(descriptor).map { group =>
      val task = descriptor.taskDescriptors.find(_.name == group.taskName).get
      StreamSplitterGroupDef(
        groupName = s"StreamSplitter_${group.outputWidths.mkString("_")}",
        taskName = group.taskName,
        peIndex = group.peIndex,
        unitName = group.endpointName + suffixForPe(group.peIndex, task.numProcessingElements),
        outputPortTypes = group.dataRoutes.map(_.portType),
        outputWidths = group.dataRoutes.map(_.bitWidth))
    }
  }

  private def getSpawnNextDataWidthBits(descriptor: FullSysGenDescriptor, task: TaskDescriptor): Int = {
    descriptor.spawnNextList
      .get(task.name)
      .map(_.flatMap(target => descriptor.taskDescriptors.find(_.name == target).map(_.widthTask)))
      .filter(_.nonEmpty)
      .map(_.max)
      .getOrElse(task.widthTask)
  }

  private def getSpawnNextBundleWidthBits(descriptor: FullSysGenDescriptor, task: TaskDescriptor): Int = {
    val wAddr = descriptor.widthAddress
    val wData = getSpawnNextDataWidthBits(descriptor, task)
    val wAllow = if (task.variableSpawn) 0 else 32
    val nAllow = 1 + descriptor.spawnList.getOrElse(task.name, List.empty).count(_ != task.name)
    val totalSize = wAddr + wData + 32 + nAllow * wAllow
    nextPow2(totalSize)
  }

  private def getArgDataOutDataWidthBits(descriptor: FullSysGenDescriptor, task: TaskDescriptor): Int = {
    if (task.generateArgOutWriteBuffer) {
      task.argumentSizeList.headOption.getOrElse(0)
    } else {
      0
    }
  }

  private def getArgDataOutBundleWidthBits(descriptor: FullSysGenDescriptor, task: TaskDescriptor): Int = {
    val wAddr = descriptor.widthAddress
    val wData = getArgDataOutDataWidthBits(descriptor, task)
    val wAllow = 32
    val nAllow = 1
    val totalSize = wAddr + wData + 32 + nAllow * wAllow
    nextPow2(totalSize)
  }

  private def nextPow2(value: Int): Int = {
    var x = 1
    while (x < value) x = x << 1
    x
  }

  private def resolvePortWidthBits(
      task: TaskDescriptor,
      peIndex: Int,
      portName: String,
      isInput: Boolean,
      portBitWidthsByTaskPePort: Map[(String, Int, String), Int]
  ): Int = {
    // Explicit rules for subPE helper ports.
    if (portName.startsWith("readSingle") && portName.endsWith("Out")) {
      toSupportedPlioBits(64)
    } else if (portName.startsWith("readStream") && portName.endsWith("Out")) {
      toSupportedPlioBits(128)
    } else if (portName.startsWith("writeSingle") && portName.endsWith("Out")) {
      toSupportedPlioBits(128)
    } else if (portName.startsWith("writeStream") && portName.endsWith("Out")) {
      val dataSize = extractWidthOrDefault(portName, 64)
      val bits = if (dataSize < 64) 64 else 128
      toSupportedPlioBits(bits)
    } else if (portName.startsWith("readSingle") && portName.endsWith("In")) {
      val dataSize = extractWidthOrDefault(portName, 64)
      toSupportedPlioBits(dataSize)
    } else if (portName.startsWith("readStream") && portName.endsWith("In")) {
      val dataSize = extractWidthOrDefault(portName, 128)
      toSupportedPlioBits(dataSize)
    } else {
      val metadataWidth = portBitWidthsByTaskPePort.get((task.name, peIndex, portName))
      val fallbackWidth = portName match {
        case "taskIn" | "taskOut" | "taskOutInternal" | "taskInGlobal" | "taskOutGlobal" => task.widthTask
        case "mallocIn" | "mallocOut"                                   => if (task.widthMalloc > 0) task.widthMalloc else 64
        case "closureIn" | "closureOut"                                 => widthFromSideConfigOrDefault(task, "allocator", 64)
        case "argIn" | "argOut"                                         => widthFromArgumentMetadataOrDefault(task, 64)
        case "argDataOut" | "spawnNext"                                 => task.widthTask
        case _                                                              => if (isInput) task.widthTask else task.widthTask
      }
      toSupportedPlioBits(metadataWidth.getOrElse(fallbackWidth))
    }
  }

  private def widthFromArgumentMetadataOrDefault(task: TaskDescriptor, default: Int): Int = {
    val sideWidth = task.sidesConfigs.find(_.sideType == "argumentNotifier").map(_.portWidth)
    val argumentListWidth = task.argumentSizeList.headOption
    sideWidth.orElse(argumentListWidth).getOrElse(default)
  }

  private def widthFromSideConfigOrDefault(task: TaskDescriptor, sideType: String, default: Int): Int =
    task.sidesConfigs.find(_.sideType == sideType).map(_.portWidth).getOrElse(default)

  private def toSupportedPlioBits(rawWidth: Int): Int = {
    val capped = math.max(1, math.min(rawWidth, 128))
    if (capped <= 32) 32
    else if (capped <= 64) 64
    else 128
  }

  private def extractWidthOrDefault(portName: String, default: Int): Int = {
    val digits = """(\d+)""".r.findAllIn(portName).toList
    digits.lastOption.map(_.toInt).getOrElse(default)
  }
  private def buildCreditBindings(
      units: Seq[UnitDef],
      groups: Seq[AIEPlioPhysicalSharingGroupDescriptor]
  ): Seq[CreditBinding] = {
    val bindings = groups.flatMap { group =>
      group.members.zipWithIndex.map { case (member, laneIndex) =>
        val matches = units.filter(unit => member == s"${unit.unitName}_${group.portName}")
        require(matches.size == 1,
          s"V6 credit group '${group.name}' member '$member' resolves to ${matches.size} AIE kernels")
        val unit = matches.head
        require(unit.taskOutputPorts.contains(group.creditReturnPort) ||
          unit.rwOutputPorts.contains(group.creditReturnPort),
          s"V6 credit group '${group.name}' requires output '${group.creditReturnPort}', but '${unit.functionName}' does not export it")
        CreditBinding(group, laneIndex, unit.unitName, s"v6slot_credit_${unit.unitName}")
      }
    }
    bindings.groupBy(_.unitName).foreach { case (unitName, perUnit) =>
      val ports = perUnit.map(_.group.portName)
      require(ports.distinct.size == ports.size,
        s"V6 AIE kernel '$unitName' has duplicate credit bindings for ${ports.diff(ports.distinct).distinct.mkString(", ")}")
    }
    bindings
  }

  private def groupedKernelOutputGroups(
      unit: UnitDef,
      descriptor: FullSysGenDescriptor
  ): Seq[Seq[String]] = {
    val outputPorts = uniquePreserveOrder(
      orderTaskOutputPorts(unit.taskOutputPorts) ++ unit.rwOutputPorts.sorted)
    buildStreamSplitterGroups(descriptor)
      .filter(group => group.taskName == unit.taskName &&
        group.peIndex == unit.peIndex && group.unitName == unit.unitName)
      .map(_.outputPortTypes.filter(outputPorts.contains))
      .filter(_.nonEmpty)
  }

  private def kernelOutputCount(
      unit: UnitDef,
      descriptor: FullSysGenDescriptor
  ): Int = {
    val outputPorts = uniquePreserveOrder(
      orderTaskOutputPorts(unit.taskOutputPorts) ++ unit.rwOutputPorts.sorted)
    val groups = groupedKernelOutputGroups(unit, descriptor)
    val grouped = groups.flatten.toSet
    outputPorts.count(port => !grouped.contains(port)) + groups.size
  }

  private def kernelInputCount(unit: UnitDef): Int =
    uniquePreserveOrder(
      orderTaskInputPorts(unit.taskInputPorts) ++ unit.rwInputPorts.sorted).size

  private def generateManualCreditConfig(
      projectFolder: String, units: Seq[UnitDef],
      descriptor: FullSysGenDescriptor, bindings: Seq[CreditBinding]
  ): Unit = {
    val byName = units.map(u => u.unitName -> u).toMap
    val kernels = units.map { unit =>
      val name = unit.unitName
      val ports = bindings.filter(_.unitName == name)
      val inputPorts = uniquePreserveOrder(orderTaskInputPorts(unit.taskInputPorts) ++ unit.rwInputPorts.sorted)
      val outputPorts = uniquePreserveOrder(orderTaskOutputPorts(unit.taskOutputPorts) ++ unit.rwOutputPorts.sorted)
      val groups = groupedKernelOutputGroups(unit, descriptor)
      val representative = outputPorts.map(p => p -> groups.find(_.contains(p)).map(_.head).getOrElse(p)).toMap
      val outputs = uniquePreserveOrder(outputPorts.map(representative))
      val receives = ports.sortBy(_.group.portName).map { binding =>
        val group = binding.group
        val split = StreamSplitterPlan.groups(descriptor).find(g =>
          g.taskName == unit.taskName && g.peIndex == unit.peIndex &&
          g.endpointName == unit.functionName && g.creditGroups.exists(_.name == group.name))
        val task = descriptor.taskDescriptors.find(_.name == unit.taskName).get
        val words = split.map(g => (g.outputWidths.max + 31) / 32 + 4).getOrElse {
          val bits = resolvePortWidthBits(task, unit.peIndex, group.creditReturnPort,
            false, buildHardCilkPortBitWidthsByTaskPePort(descriptor))
          require(bits >= 64 && bits % 32 == 0, "V6 slot credit return needs a whole >=64-bit record")
          bits / 32
        }
        val tag = split.map(g => g.dataRoutes.size + g.creditGroups.indexWhere(_.name == group.name))
          .getOrElse(group.creditTag)
        V9CreditConfig.Receive(inputPorts.indexOf(group.portName),
          outputs.indexOf(representative(group.creditReturnPort)),
          group.logicalWidth / 32 * (if (group.mode == AiePlioSharingMode.Stream) group.segmentBeats else 1),
          words, tag, split.nonEmpty, group.mode == AiePlioSharingMode.Stream)
      }
      val encodes = StreamSplitterPlan.groups(descriptor).filter(g =>
        g.taskName == unit.taskName && g.peIndex == unit.peIndex &&
          g.endpointName == unit.functionName && g.dataRoutes.size == 1 && g.creditGroups.nonEmpty
      ).map { g =>
        val route = g.dataRoutes.head
        require(route.bitWidth % 32 == 0)
        V9CreditConfig.Encode(outputs.indexOf(representative(route.portType)), route.bitWidth / 32)
      }
      V9CreditConfig.Kernel(unit.unitName, unit.functionName,
        inputPorts, outputs, receives, encodes)
    }
    V9CreditConfig.write(projectFolder, kernels)
  }
}
