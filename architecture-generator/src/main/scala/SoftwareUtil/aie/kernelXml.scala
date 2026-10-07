package SoftwareUtil.aie

import Descriptors._
import java.io.PrintWriter
import scala.collection.mutable
import aiehelpers.StreamSplitterPlan

object KernelXmlTemplate {

  private case class TopStreamPortDef(
      bindPortName: String,
      mode: String,
      bitWidth: Int
  )

    private case class TopMgmtArgDef(
      argName: String,
      offset: Int
    )

      private case class TaskOutputDef(
        taskName: String,
        peIndex: Int,
        portType: String,
        bitWidth: Int
      )

        private case class RoutedOutputDef(
          taskName: String,
          peIndex: Int,
          endpointName: String,
          portType: String,
          bitWidth: Int
        )

        private case class OrderedSubPEEndpointDef(
          name: String,
          normalizedName: String,
          nextSubPE: Option[String],
          taskOutputPorts: Seq[String]
        )

        private case class TaskAieEndpointDef(
          defaultOutputEndpoint: String,
          outputByPort: Map[String, String]
        )

      private case class StreamSplitterXmlDef(
        kernelName: String,
        outputWidths: Seq[Int]
      )

  def generateHelperKernelXmls(descriptor: FullSysGenDescriptor, projectFolder: String): Unit = {
    val xmlFolder = new java.io.File(s"$projectFolder/scripts/xml")
    xmlFolder.mkdirs()

    val topFile = new java.io.File(xmlFolder, s"${descriptor.name}.xml")
    val topWriter = new PrintWriter(topFile)
    try {
      topWriter.write(renderTopKernelXml(descriptor) + "\n")
    } finally {
      topWriter.close()
    }

  }

  /**
    * Splitter variants required by the generated graph. RTL and XML generation
    * share this calculation so kernel names and interfaces cannot drift.
    */
  def requiredStreamSplitterWidths(
      descriptor: FullSysGenDescriptor
  ): Seq[Vector[Int]] =
    StreamSplitterPlan.groups(descriptor)
      .map(_.outputWidths)
      .distinct

  private def sourceTaskDataWidth(req: RWRequestDescriptor): Int = {
    val addressWidth = 64
    (req.`type`, req.mode) match {
      case ("read", "single") => addressWidth
      case ("read", "stream") => paddedTo64(addressWidth + 32)
      case ("write", "single") => paddedTo64(addressWidth + req.portWidth)
      case ("write", "stream") => paddedTo64(addressWidth + req.portWidth)
      case _ =>
        throw new IllegalArgumentException(s"Unsupported rwRequest combination: type=${req.`type`} mode=${req.mode}")
    }
  }

  private def buildStreamSplitterXmlDefs(descriptor: FullSysGenDescriptor): Seq[StreamSplitterXmlDef] = {
    val endpointByTask = buildAieEndpointByTask(descriptor)
    val nonLastEndpointsByTask = getNonLastEndpointsByTask(descriptor)
    val routedOutputs = collectTaskRoutedOutputs(descriptor, endpointByTask) ++ collectRWRoutedOutputs(descriptor)
    val outputsByTaskPeEndpoint = routedOutputs.groupBy(out => (out.taskName, out.peIndex, out.endpointName))

    outputsByTaskPeEndpoint
      .toSeq
      .sortBy { case ((taskName, peIndex, endpointName), _) => (taskName, peIndex, endpointName) }
      .flatMap { case ((taskName, peIndex, endpointName), outputs) =>
        val isNonLastEndpoint = nonLastEndpointsByTask.getOrElse(taskName, Set.empty).contains(endpointName)
        val ordered = outputs
          .groupBy(_.portType)
          .map(_._2.head)
          .toSeq
          .sortBy(out => (outputPortPriority(out.portType), out.portType))


        val endpointCredits = descriptor.expandedAiePlioSharingGroups
          .filter(group => group.sourceKind == AiePlioSourceKind.HardCilkInput &&
            group.taskName == taskName && group.peIndices.contains(peIndex) &&
            normalizeName(group.subPEName) == endpointName)
          .sortBy(_.name)
        endpointCredits.foreach { credit =>
          require(ordered.exists(_.portType == credit.creditReturnPort),
            s"AIE PLIO sharing group ${credit.name} returns credit on ${credit.creditReturnPort}, " +
              s"but AIE endpoint $endpointName has no such output")
        }

        val baseGroups =
          if (isNonLastEndpoint && ordered.size > 1) Seq(ordered)
          else if (ordered.size > 2) ordered.grouped(2).filter(_.size > 1).toSeq
          else Seq.empty
        val groupedPorts = baseGroups.flatten.map(_.portType).toSet
        val creditedSingletons = endpointCredits.map(_.creditReturnPort).distinct
          .filterNot(groupedPorts).map(port => Seq(ordered.find(_.portType == port).get))

        (baseGroups ++ creditedSingletons).map { dataGroup =>
          val creditCount = endpointCredits.count(credit =>
            dataGroup.exists(_.portType == credit.creditReturnPort))
          val widths = dataGroup.map(_.bitWidth) ++ Seq.fill(creditCount)(32)
          StreamSplitterXmlDef(
            kernelName = s"StreamSplitter_${widths.mkString("_")}",
            outputWidths = widths
          )
        }
      }
      .distinctBy(_.kernelName)
  }

  private def collectTaskRoutedOutputs(
      descriptor: FullSysGenDescriptor,
      endpointByTask: Map[String, TaskAieEndpointDef]
  ): Seq[RoutedOutputDef] = {
    collectTaskOutputs(descriptor).map { out =>
      val endpoint = endpointByTask.get(out.taskName).map(ep => ep.outputByPort.getOrElse(out.portType, ep.defaultOutputEndpoint)).getOrElse(out.taskName)
      RoutedOutputDef(
        taskName = out.taskName,
        peIndex = out.peIndex,
        endpointName = endpoint,
        portType = out.portType,
        bitWidth = out.bitWidth
      )
    }
  }

  private def collectRWRoutedOutputs(descriptor: FullSysGenDescriptor): Seq[RoutedOutputDef] = {
    val taskPeCountByName = descriptor.taskDescriptors.map(t => t.name -> t.numProcessingElements).toMap

    descriptor.subPEList.toSeq.sortBy(_._1).flatMap { case (subPEName, sub) =>
      sub.rwRequest.toSeq.flatMap { req =>
        taskPeCountByName.get(sub.peName).toSeq.flatMap { peCount =>
          val portType = rwOutputPortName(req)
          val width = sourceTaskDataWidth(req)
          (0 until peCount).map { peIndex =>
            RoutedOutputDef(
              taskName = sub.peName,
              peIndex = peIndex,
              endpointName = normalizeName(subPEName),
              portType = portType,
              bitWidth = width
            )
          }
        }
      }
    }
  }

  private def rwOutputPortName(req: RWRequestDescriptor): String = {
    (req.`type`, req.mode) match {
      case ("read", "single") => s"readSingle${req.portWidth}Out"
      case ("read", "stream") => s"readStream${req.portWidth}Out"
      case ("write", "single") => s"writeSingle${req.portWidth}Out"
      case ("write", "stream") => s"writeStream${req.portWidth}Out"
      case _ => throw new IllegalArgumentException(s"Unsupported rwRequest combination: type=${req.`type`} mode=${req.mode}")
    }
  }

  private def buildAieEndpointByTask(descriptor: FullSysGenDescriptor): Map[String, TaskAieEndpointDef] = {
    descriptor.taskDescriptors.map { task =>
      val orderedSubPEs = getOrderedSubPEEndpointsForTask(descriptor, task.name)
      if (orderedSubPEs.nonEmpty) {
        val outputByPort = mutable.Map[String, String]()
        orderedSubPEs.foreach { sub =>
          sub.taskOutputPorts.foreach { port =>
            outputByPort(port) = sub.normalizedName
          }
        }
        task.name -> TaskAieEndpointDef(
          defaultOutputEndpoint = orderedSubPEs.last.normalizedName,
          outputByPort = outputByPort.toMap
        )
      } else {
        task.name -> TaskAieEndpointDef(task.name, Map.empty)
      }
    }.toMap
  }

  private def getNonLastEndpointsByTask(descriptor: FullSysGenDescriptor): Map[String, Set[String]] = {
    descriptor.taskDescriptors.map { task =>
      val orderedSubPEs = getOrderedSubPEEndpointsForTask(descriptor, task.name)
      val nonLast = if (orderedSubPEs.length > 1) orderedSubPEs.dropRight(1).map(_.normalizedName).toSet else Set.empty[String]
      task.name -> nonLast
    }.toMap
  }

  private def getOrderedSubPEEndpointsForTask(descriptor: FullSysGenDescriptor, taskName: String): Seq[OrderedSubPEEndpointDef] = {
    val subpes = descriptor.subPEList
      .toSeq
      .filter { case (_, sub) => sub.peName == taskName }
      .map { case (name, sub) =>
        OrderedSubPEEndpointDef(
          name = name,
          normalizedName = normalizeName(name),
          nextSubPE = sub.rwRequest.flatMap(_.nextsubPE),
          taskOutputPorts = sub.taskOutputPorts
        )
      }

    if (subpes.isEmpty) {
      Seq.empty
    } else {
      val byName = subpes.map(s => s.name -> s).toMap
      val incomingTargets = subpes.flatMap(_.nextSubPE).toSet
      val heads = subpes.filterNot(s => incomingTargets.contains(s.name)).sortBy(_.name)

      val ordered = mutable.ArrayBuffer[OrderedSubPEEndpointDef]()
      val visited = mutable.Set[String]()

      def walk(start: OrderedSubPEEndpointDef): Unit = {
        var current = Option(start)
        while (current.nonEmpty && !visited.contains(current.get.name)) {
          val value = current.get
          ordered += value
          visited += value.name
          current = value.nextSubPE.flatMap(byName.get)
        }
      }

      heads.foreach(walk)
      subpes.sortBy(_.name).filterNot(s => visited.contains(s.name)).foreach(walk)
      ordered.toSeq
    }
  }

  private def normalizeName(value: String): String =
    value.toLowerCase.replaceAll("[^a-z0-9_]", "")

  private def renderStreamSplitterXml(splitter: StreamSplitterXmlDef): String = {
    val inputWidth = splitter.outputWidths.max + 128
    val outputPortLines = splitter.outputWidths.zipWithIndex.map { case (width, idx) =>
      s"      <port name=\"outputs_$idx\" mode=\"read_only\" dataWidth=\"$width\" portType=\"stream\"/>"
    }
    val outputArgLines = splitter.outputWidths.zipWithIndex.map { case (_, idx) =>
      val id = idx + 1
      s"      <arg name=\"outputs_$idx\" addressQualifier=\"4\" id=\"$id\" port=\"outputs_$idx\" size=\"0x4\" offset=\"0x0\" hostOffset=\"0x0\" hostSize=\"0x4\" type=\"stream\"/>"
    }

    Seq(
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
      "<root versionMajor=\"1\" versionMinor=\"9\">",
      s"  <kernel name=\"${splitter.kernelName}\" language=\"ip\" vlnv=\"epfl.ch:hardcilk:${splitter.kernelName}:1.0\" attributes=\"\" preferredWorkGroupSizeMultiple=\"0\" workGroupSize=\"1\" hwControlProtocol=\"user_managed\">",
      "    <ports>",
      s"      <port name=\"input\" mode=\"write_only\" dataWidth=\"$inputWidth\" portType=\"stream\"/>",
      outputPortLines.mkString("\n"),
      "    </ports>",
      "    <args>",
      "      <arg name=\"input\" addressQualifier=\"4\" id=\"0\" port=\"input\" size=\"0x4\" offset=\"0x0\" hostOffset=\"0x0\" hostSize=\"0x4\" type=\"stream\"/>",
      outputArgLines.mkString("\n"),
      "    </args>",
      "  </kernel>",
      "</root>"
    ).mkString("\n")
  }

  private def renderTopKernelXml(descriptor: FullSysGenDescriptor): String = {
    val mAxiPortCount = getHardCilkAxiPortCount(descriptor)
    val mAxiDataWidth = 256
    val streamPorts = buildTopStreamPortDefs(descriptor)
    val mgmtArgs = buildTopMgmtArgDefs(descriptor)

    val mAxiPortLines = (0 until mAxiPortCount).map { i =>
      s"      <port name=\"m_axi_${f"$i%02d"}\" mode=\"master\" range=\"0x3FFFFFFFF\" dataWidth=\"$mAxiDataWidth\" portType=\"addressable\" base=\"0x0\"/>"
    }

    val streamPortLines = streamPorts.map { p =>
      s"      <port name=\"${p.bindPortName}\" mode=\"${p.mode}\" dataWidth=\"${p.bitWidth}\" portType=\"stream\"/>"
    }

    val mAxiArgLines = (0 until mAxiPortCount).map { i =>
      val id = mgmtArgs.length + i
      val offset = 0x10 + ((mgmtArgs.length + i) * 0x8)
      s"      <arg name=\"mem_$i\" addressQualifier=\"1\" id=\"$id\" port=\"m_axi_${f"$i%02d"}\" size=\"0x8\" offset=\"0x${offset.toHexString}\" hostOffset=\"0x0\" hostSize=\"0x8\" type=\"void*\"/>"
    }

    val streamArgLines = streamPorts.zipWithIndex.map { case (p, idx) =>
      val id = mgmtArgs.length + mAxiPortCount + idx
      s"      <arg name=\"${p.bindPortName}\" addressQualifier=\"4\" id=\"$id\" port=\"${p.bindPortName}\" size=\"0x4\" offset=\"0x0\" hostOffset=\"0x0\" hostSize=\"0x4\" type=\"stream\"/>"
    }

    val mgmtArgLines = mgmtArgs.zipWithIndex.map { case (a, id) =>
      s"      <arg name=\"${a.argName}\" addressQualifier=\"0\" id=\"$id\" port=\"s_axil_mgmt_hardcilk\" size=\"0x8\" offset=\"0x${a.offset.toHexString}\" hostOffset=\"0x0\" hostSize=\"0x8\" type=\"ap_uint&lt;64>\"/>"
    }

    val portSectionLines =
      mAxiPortLines ++
        Seq("      <port name=\"s_axil_mgmt_hardcilk\" mode=\"slave\" range=\"0x1000\" dataWidth=\"32\" portType=\"addressable\" base=\"0x0\"/>") ++
        Seq("      <!-- AXI-Stream ports for AIE connectivity -->") ++
        streamPortLines

    val argSectionLines =
      mgmtArgLines ++
        mAxiArgLines ++
        Seq("      <!-- Stream port args -->") ++
        streamArgLines

    Seq(
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
      "<root versionMajor=\"1\" versionMinor=\"9\">",
      s"  <kernel name=\"${descriptor.name}\" language=\"ip\" vlnv=\"epfl.ch:hardcilk:${descriptor.name}:1.0\" attributes=\"\" preferredWorkGroupSizeMultiple=\"0\" workGroupSize=\"1\" hwControlProtocol=\"user_managed\">",
      "    <ports>",
      portSectionLines.mkString("\n"),
      "    </ports>",
      "    <args>",
      argSectionLines.mkString("\n"),
      "    </args>",
      "  </kernel>",
      "</root>"
    ).mkString("\n")
  }

  private def buildTopStreamPortDefs(descriptor: FullSysGenDescriptor): Seq[TopStreamPortDef] = {
    val ports = mutable.ArrayBuffer[TopStreamPortDef]()
	val splitterGroups = StreamSplitterPlan.groups(descriptor)
	val splitRoutes = splitterGroups.flatMap(_.dataRoutes)
	  .map(route => (route.taskName, route.peIndex, route.portType)).toSet
	val splitRwRoutes = splitterGroups.flatMap(_.dataRoutes)
	  .map(route => (route.taskName, route.peIndex, route.endpointName, route.portType)).toSet
	val sharedHardCilkOutputs = descriptor.expandedAiePlioSharingGroups
      .filter(_.sourceKind == AiePlioSourceKind.HardCilkOutput)
      .flatMap(g => g.peIndices.map(i => (g.taskName, i, g.portName))).toSet
    val sharedRwRequests = descriptor.expandedAiePlioSharingGroups
      .filter(_.sourceKind == AiePlioSourceKind.RwRequest)
      .flatMap(g => g.peIndices.map(i => (g.subPEName, i))).toSet
	val sharedHardCilkInputs = descriptor.expandedAiePlioSharingGroups
	  .filter(_.sourceKind == AiePlioSourceKind.HardCilkInput)
	  .flatMap(group =>
	    group.peIndices.map(peIndex => (group.taskName, peIndex, group.portName))
	).toSet

    descriptor.getSystemConnectionsDescriptor().connections.foreach { connection =>
      val src = connection.srcPort
      val dst = connection.dstPort

      if (src.parentType == "HardCilk" && dst.parentType == "PE" &&
	          !sharedHardCilkInputs.contains((dst.parentName, dst.parentIndex, dst.portType))) {
        ports += TopStreamPortDef(
          bindPortName = s"BindTo_PE_${dst.parentName}_${dst.parentIndex}_${dst.portType}",
          mode = "write_only",
          bitWidth = connection.bitWidth
        )
      } else if (src.parentType == "PE" && dst.parentType == "HardCilk" &&
          !splitRoutes((src.parentName, src.parentIndex, src.portType)) &&
          !sharedHardCilkOutputs((src.parentName, src.parentIndex, src.portType))) {
        ports += TopStreamPortDef(
          bindPortName = s"BindTo_PE_${src.parentName}_${src.parentIndex}_${src.portType}",
          mode = "read_only",
          bitWidth = connection.bitWidth
        )
      }
    }

    descriptor.taskDescriptors.foreach { task =>
      val spawnNextWidth = getSpawnNextBundleWidthBits(descriptor, task)
      val argDataOutWidth = getArgDataOutBundleWidthBits(descriptor, task)
      (0 until task.numProcessingElements).foreach { peIndex =>
        if (task.generateArgOutWriteBuffer &&
            !splitRoutes((task.name, peIndex, "argDataOut")) &&
            !sharedHardCilkOutputs((task.name, peIndex, "argDataOut"))) {
          ports += TopStreamPortDef(
            bindPortName = s"BindTo_PE_${task.name}_${peIndex}_argDataOut",
            mode = "read_only",
            bitWidth = argDataOutWidth
          )
        }
        if (task.generateSpawnNextWriteBuffer &&
            !splitRoutes((task.name, peIndex, "spawnNext")) &&
            !sharedHardCilkOutputs((task.name, peIndex, "spawnNext"))) {
          ports += TopStreamPortDef(
            bindPortName = s"BindTo_PE_${task.name}_${peIndex}_spawnNext",
            mode = "read_only",
            bitWidth = spawnNextWidth
          )
        }
      }
    }

    descriptor.subPEList.toSeq.sortBy(_._1).foreach { case (subPEName, sub) =>
      sub.rwRequest.foreach { req =>
        descriptor.taskDescriptors.find(_.name == sub.peName).foreach { task =>
          (0 until task.numProcessingElements).foreach { peIndex =>
            val rwPortType = rwOutputPortName(req)
            // portType alone is not unique within a task: several helper
            // kernels can all emit (for example) readSingle64Out. Only
            // suppress this helper's XO port when this exact AIE endpoint is
            // routed through a PL-side StreamSplitter.
            if (!splitRwRoutes((sub.peName, peIndex, normalizeName(subPEName), rwPortType)) &&
                !sharedRwRequests((subPEName, peIndex))) {
              ports += TopStreamPortDef(
                bindPortName = rwTopPortName(subPEName, peIndex, "sourceTask"),
                mode = "write_only",
                bitWidth = sourceTaskDataWidth(req)
              )
            }

            if (req.`type` == "read") {
              val responseIsShared = descriptor.expandedAiePlioSharingGroups.exists { group =>
                group.sourceKind == AiePlioSourceKind.ReadResponse &&
                group.sourceSubPEName == subPEName &&
                group.peIndices.contains(peIndex)
              }
              if (!responseIsShared) {
                ports += TopStreamPortDef(
                  bindPortName = rwTopPortName(subPEName, peIndex, "sinkResult"),
                  mode = "read_only",
                  bitWidth = req.portWidth
                )
              }
            }
          }
        }
      }
    }

    descriptor.expandedAiePlioSharingGroups.foreach { group =>
      if (group.sourceKind == AiePlioSourceKind.PackedOutput &&
          !splitterGroups.exists(s => s.taskName == group.taskName &&
            group.peIndices.contains(s.peIndex) && s.aiePortType == group.portName &&
            s.endpointName == group.subPEName.toLowerCase.replaceAll("[^a-z0-9_]", ""))) {
        group.peIndices.foreach { peIndex =>
          ports += TopStreamPortDef(s"${group.name}_packed_$peIndex", "read_only", group.logicalWidth)
        }
      }
      ports += TopStreamPortDef(
        bindPortName = group.effectivePhysicalPortName,
        mode = if (group.direction == "plToAie") "write_only" else "read_only",
        bitWidth = group.packetPortWidth
      )
    }

    splitterGroups.filterNot { split =>
      descriptor.expandedAiePlioSharingGroups.exists(g =>
        g.sourceKind == AiePlioSourceKind.PackedOutput &&
          g.taskName == split.taskName && g.peIndices.contains(split.peIndex) &&
          g.portName == split.aiePortType &&
          g.subPEName.toLowerCase.replaceAll("[^a-z0-9_]", "") == split.endpointName)
    }.foreach { group =>
      ports += TopStreamPortDef(
        bindPortName = group.physicalPortName,
        mode = "read_only",
        bitWidth = group.outputWidths.max + 128
      )
    }

    ports
      .groupBy(_.bindPortName)
      .map { case (_, defs) => defs.maxBy(_.bitWidth) }
      .toSeq
      .sortBy(_.bindPortName)
  }

  private def buildTopMgmtArgDefs(descriptor: FullSysGenDescriptor): Seq[TopMgmtArgDef] = {
    val schedulerRegs = Seq("rPause", "rAddr", "maxLen", "fifoTailReg", "fifoHeadReg", "procInterrupt", "currLen")
    val allocatorRegs = Seq("rPause", "rAddr", "avaialbleSize")

    descriptor.taskDescriptors.flatMap { task =>
      val schedulerArgs = task.mgmtBaseAddresses.schedulerServersBaseAddresses.zipWithIndex.flatMap { case (baseAddr, idx) =>
        schedulerRegs.zipWithIndex.map { case (regName, regIdx) =>
          TopMgmtArgDef(
            argName = s"${task.name}_scheduler_${idx}_$regName",
            offset = baseAddr + (regIdx * 8)
          )
        }
      }

      val allocatorArgs = task.mgmtBaseAddresses.allocationServersBaseAddresses.zipWithIndex.flatMap { case (baseAddr, idx) =>
        allocatorRegs.zipWithIndex.map { case (regName, regIdx) =>
          TopMgmtArgDef(
            argName = s"${task.name}_allocator_${idx}_$regName",
            offset = baseAddr + (regIdx * 8)
          )
        }
      }

      val memoryAllocatorArgs = task.mgmtBaseAddresses.memoryAllocatorServersBaseAddresses.zipWithIndex.flatMap { case (baseAddr, idx) =>
        allocatorRegs.zipWithIndex.map { case (regName, regIdx) =>
          TopMgmtArgDef(
            argName = s"${task.name}_memoryAllocator_${idx}_$regName",
            offset = baseAddr + (regIdx * 8)
          )
        }
      }

      schedulerArgs ++ allocatorArgs ++ memoryAllocatorArgs
    }
  }

  private def collectTaskOutputs(descriptor: FullSysGenDescriptor): Seq[TaskOutputDef] = {
    val directOutputs = descriptor.getSystemConnectionsDescriptor().connections.flatMap { connection =>
      val src = connection.srcPort
      val dst = connection.dstPort
      if (src.parentType == "PE" && dst.parentType == "HardCilk") {
        Some(TaskOutputDef(src.parentName, src.parentIndex, src.portType, connection.bitWidth))
      } else {
        None
      }
    }

    val writeBufferOutputs = descriptor.taskDescriptors.flatMap { task =>
      val spawnNextWidth = getSpawnNextBundleWidthBits(descriptor, task)
      val argDataOutWidth = getArgDataOutBundleWidthBits(descriptor, task)
      (0 until task.numProcessingElements).flatMap { peIndex =>
        val argDataOut =
          if (task.generateArgOutWriteBuffer) Some(TaskOutputDef(task.name, peIndex, "argDataOut", argDataOutWidth)) else None
        val spawnNext =
          if (task.generateSpawnNextWriteBuffer) Some(TaskOutputDef(task.name, peIndex, "spawnNext", spawnNextWidth)) else None
        Seq(argDataOut, spawnNext).flatten
      }
    }

    (directOutputs ++ writeBufferOutputs)
      .groupBy(out => (out.taskName, out.peIndex, out.portType))
      .map { case (_, defs) => defs.maxBy(_.bitWidth) }
      .toSeq
  }

  // Mirror Verilog logic in HLSHelpers + Util.WriteBuffer.WriteBundle sizing.
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

  private def outputPortPriority(portType: String): Int = {
    portType match {
      case "taskOut" => 0
      case "taskOutGlobal" => 1
      case "argOut" => 2
      case "closureOut" => 3
      case "mallocOut" => 4
      case "argDataOut" => 5
      case "spawnNext" => 6
      case _ => 100
    }
  }

  private def getHardCilkAxiPortCount(descriptor: FullSysGenDescriptor): Int = {
    val numHBMPorts = if (descriptor.maximumAXIPorts > 0) descriptor.maximumAXIPorts else 6

    val interfacesPE = getEstimatedPEInterfacesCount(descriptor)
    val interfacesScheduler = descriptor.taskDescriptors.map(task => task.getNumServers("scheduler") + task.spawnServersCount).sum
    val interfacesClosureAllocator = descriptor.taskDescriptors
      .filter(task => descriptor.getPortCount("spawnNext", task.name) > 0)
      .map(_.getNumServers("allocator"))
      .sum
    val interfacesArgumentNotifier = descriptor.taskDescriptors
      .filter(task => descriptor.getPortCount("sendArgument", task.name) > 0)
      .map(_.getNumServers("argumentNotifier") * 2)
      .sum
    val interfacesMemoryAllocator = descriptor.taskDescriptors
      .filter(task => descriptor.getPortCount("mallocIn", task.name) > 0)
      .map(_.getNumServers("memoryAllocator"))
      .sum
    val interfacesRemoteMemAccess = descriptor.taskDescriptors
      .count(task => task.generateArgOutWriteBuffer && (descriptor.mFPGASimulation || descriptor.mFPGASynth))

    printf("Estimated AXI interfaces needed: PE=%d, Scheduler=%d, ClosureAllocator=%d, ArgumentNotifier=%d, MemoryAllocator=%d, RemoteMemAccess=%d\n",
      interfacesPE, interfacesScheduler, interfacesClosureAllocator, interfacesArgumentNotifier, interfacesMemoryAllocator, interfacesRemoteMemAccess)
    val totalPorts =
      interfacesPE + interfacesMemoryAllocator + interfacesScheduler + interfacesClosureAllocator + interfacesArgumentNotifier + interfacesRemoteMemAccess

    if (totalPorts <= 0) {
      0
    } else {
      val numPortsPerMux = totalPorts.toDouble / numHBMPorts.toDouble
      val peMux = math.max(1, math.ceil(interfacesPE.toDouble / numPortsPerMux).toInt)
      val serverMux = math.max(0, numHBMPorts - peMux)

      val pePortsPerMux = if (peMux > 0 && interfacesPE > 0) interfacesPE.toDouble / peMux else 1.0
      val nonEmptyHBM = mutable.Set[Int]()

      if (interfacesPE > 0) {
        (0 until interfacesPE).foreach { idx =>
          val bucket = (idx.toDouble / pePortsPerMux).toInt
          if (bucket >= 0 && bucket < numHBMPorts) {
            nonEmptyHBM += bucket
          }
        }
      }

      val serverInterfaces = interfacesMemoryAllocator + interfacesScheduler + interfacesClosureAllocator + interfacesArgumentNotifier + interfacesRemoteMemAccess
      val serverPortsPerMuxClamped = if (serverInterfaces > 0 && serverMux > 0) serverInterfaces.toDouble / serverMux else 1.0

      if (serverInterfaces > 0 && serverMux > 0) {
        (0 until serverInterfaces).foreach { idx =>
          val bucket = peMux + (idx.toDouble / serverPortsPerMuxClamped).toInt
          if (bucket >= 0 && bucket < numHBMPorts) {
            nonEmptyHBM += bucket
          }
        }
      }

      nonEmptyHBM.size
    }
  }

  private def getEstimatedPEInterfacesCount(descriptor: FullSysGenDescriptor): Int = {
    descriptor.taskDescriptors.map { task =>
      val hasPEModule = task.peHDLPath.nonEmpty

      val peCoreAxi = if (task.hasAXI && hasPEModule) task.numProcessingElements else 0
      val peSpawnNextAxi =
        if ((descriptor.getPortCount("spawnNext", task.name) > 0 || task.generateSpawnNextWriteBuffer) && hasPEModule)
          task.numProcessingElements
        else
          0
      val peArgOutAxi =
        if ((descriptor.getPortCount("sendArgument", task.name) > 0 || task.generateArgOutWriteBuffer) && hasPEModule)
          task.numProcessingElements
        else
          0

      val wbSpawnNextAxi = if (task.generateSpawnNextWriteBuffer && !hasPEModule) task.numProcessingElements else 0
      val wbArgDataAxi = if (task.generateArgOutWriteBuffer && !hasPEModule) task.numProcessingElements else 0
      val peIORwAxi =
        if (!hasPEModule) {
          descriptor.subPEList.values.count(sub => sub.peName == task.name && sub.rwRequest.nonEmpty) * task.numProcessingElements
        } else {
          0
        }

      peCoreAxi + peSpawnNextAxi + peArgOutAxi + wbSpawnNextAxi + wbArgDataAxi + peIORwAxi
    }.sum
  }

  private def paddedTo64(width: Int): Int =
    width + ((64 - (width % 64)) % 64)

  private def rwTopPortName(subPEName: String, peIndex: Int, suffix: String): String =
    s"${subPEName}_${peIndex}_$suffix"
}
