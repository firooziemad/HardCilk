
package HardCilk

import chisel3._
import Descriptors._
import Scheduler._
import Allocator._
import ArgumentNotifier._
import HLSHelpers._
import scala.collection.mutable.ArrayBuffer
import aiehelpers._

// All the AXI-related imports needed by the HBM logic
import chext.amba.axi4
import axi4.Ops._
import chext.amba.axi4s
import axi4s.Casts._
import chext.{elasticnew => e}
import e.ConnectOp._
import AXIHelpers._
import Util.AddressTransformConfig
import io.circe.generic.auto._
import Util.WriteBuffer
import Util.AxiPageBoundarySplitter_Basic
import Util.AxiPageBoundarySplitter_Config
import Util.RemoteStreamToMem
import scala.collection.mutable

/**
 * A trait that encapsulates the HBM AXI interconnect generation logic.
 * It is intended to be mixed into the top-level CleanHardCilk module.
 * * It requires the class mixing it in to provide concrete implementations
 * for all the abstract 'val's and 'var's defined below.
 */
trait HasHBMInterconnect extends Module {

  // --- Abstract members to be provided by CleanHardCilk ---
  // These are "inputs" that the trait needs from the main class.
  val fullSysGenDescriptor: FullSysGenDescriptor
  val reduceAxi: Int
  val addressTransformFlag: Boolean
  val cfgAxi4HBM: axi4.Config
  val cfgXDMA: axi4.Config
  val interfaceBuffer: ArrayBuffer[hdlinfo.Interface]
  val axiOuts: ArrayBuffer[axi4.RawInterface]
  val axiXDMA: ArrayBuffer[axi4.RawInterface]

  // This is an "output" var that this trait will update
  var numHbmPortExports: Int

  private val peIOByTaskName = mutable.Map[String, PeIO]()
  private val sharedHardCilkInputSinks =
    mutable.Map[(String, Int, String), (Int, axi4s.Interface)]()
  private val attachedSharedHardCilkInputs = mutable.Set[(String, Int, String)]()
  private val sharedHardCilkOutputSources = mutable.Map[(String, Int, String), (Int, axi4s.Interface)]()
  private val attachedSharedHardCilkOutputs = mutable.Set[(String, Int, String)]()
  private val splitAieOutputSources =
    mutable.Map[(String, Int, String, String), axi4s.Interface]()
  private val attachedSplitAieOutputs = mutable.Set[(String, Int, String, String)]()

  private def getOrCreatePeIO(task: TaskDescriptor): PeIO = {
    peIOByTaskName.getOrElseUpdate(task.name, {
      val subPEs = fullSysGenDescriptor.subPEList.filter(_._2.peName == task.name)
      val sharingGroups = fullSysGenDescriptor.expandedAiePlioSharingGroups
        .filter(_.taskName == task.name)
      val outputSplitterGroups = StreamSplitterPlan.groups(fullSysGenDescriptor)
        .filter(_.taskName == task.name)
      val directCreditRoutes = sharingGroups.filter(g => g.direction == "plToAie" &&
        !g.creditReturnPort.startsWith("read") && !g.creditReturnPort.startsWith("write"))
        .flatMap { g => g.peIndices.filterNot(i => outputSplitterGroups.exists(s =>
          s.peIndex == i && s.creditGroups.exists(_.name == g.name))).map { i =>
          val connections = fullSysGenDescriptor.getSystemConnectionsDescriptor().connections
            .filter(c => c.srcPort.parentType == "PE" &&
              c.srcPort.parentName == task.name && c.srcPort.parentIndex == i &&
              c.srcPort.portType == g.creditReturnPort && c.dstPort.parentType == "HardCilk")
          require(connections.nonEmpty,
            s"Missing direct credit output ${task.name}[$i].${g.creditReturnPort}")
          StreamSplitterDataRoute(task.name, i, g.subPEName, g.creditReturnPort,
            connections.map(_.bitWidth).max)
        }}.distinct
      Module(new PeIO(
        subPEs,
        task.numProcessingElements,
        task.name,
        sharingGroups,
        outputSplitterGroups,
        directCreditRoutes
      ))
    })
  }

  /**
   * Instantiates the PeIO containers that own configured AIE packetizers and
   * exports their physical packet streams from the single HardCilk top.
   * This is called before missing PE ports are attached.
   */
  def prepareAiePlioSharing(): Unit = {
    // Tagged outputs also exist without shared forward inputs (e.g. Exp3).
    // Export their splitter ingress before attaching any logical PE outputs.
    (fullSysGenDescriptor.expandedAiePlioSharingGroups.map(_.taskName) ++
      StreamSplitterPlan.groups(fullSysGenDescriptor).map(_.taskName))
      .distinct
      .foreach { taskName =>
        val task = fullSysGenDescriptor.taskDescriptors
          .find(_.name == taskName)
          .getOrElse(throw new IllegalArgumentException(
            s"AIE PLIO sharing references unknown task '$taskName'"
          ))
        val peIO = getOrCreatePeIO(task)

        peIO.splitPhysicalInputs.foreach { splitInput =>
          val port = IO(chiselTypeOf(splitInput.sink))
            .suggestName(splitInput.group.physicalPortName)
          port.asLite :=> splitInput.sink.asLite
        }

        peIO.splitDataOutputs.foreach { output =>
          val key = (output.route.taskName, output.route.peIndex,
            output.route.endpointName, output.route.portType)
          require(!splitAieOutputSources.contains(key),
            s"AIE output ${key._1}[${key._2}].${key._3} belongs to more than one splitter")
          splitAieOutputSources(key) = output.source
        }

        peIO.directCreditOutputs.foreach { case (route, input, output) =>
          val physical = IO(chiselTypeOf(input))
            .suggestName(s"BindTo_PE_${route.taskName}_${route.peIndex}_${route.portType}")
          physical.asLite :=> input.asLite
          val key = (route.taskName, route.peIndex, route.endpointName, route.portType)
          require(!splitAieOutputSources.contains(key), s"Duplicate direct credit endpoint $key")
          splitAieOutputSources(key) = output
        }

        peIO.sharedHardCilkInputs.foreach { sharedInput =>
          val key = (sharedInput.taskName, sharedInput.index, sharedInput.portName)
          require(!sharedHardCilkInputSinks.contains(key),
            s"HardCilk input ${sharedInput.taskName}[${sharedInput.index}].${sharedInput.portName} belongs to more than one packetizer")
          sharedHardCilkInputSinks(key) = (sharedInput.logicalWidth, sharedInput.sink)
        }

        peIO.sharedHardCilkOutputs.foreach { sharedOutput =>
          val key = (sharedOutput.taskName, sharedOutput.index, sharedOutput.portName)
          require(!sharedHardCilkOutputSources.contains(key),
            s"HardCilk output $key belongs to more than one packet decoder")
          sharedHardCilkOutputSources(key) = (sharedOutput.logicalWidth, sharedOutput.source)
        }
        peIO.sharedPacketInputs.foreach { packetInput =>
          val physical = IO(chiselTypeOf(packetInput.sink))
            .suggestName(packetInput.physicalPortName)
          physical.asFull :=> packetInput.sink.asFull
        }
        peIO.sharedPackedOutputs.foreach { packed =>
          val physical = IO(chiselTypeOf(packed.source))
            .suggestName(packed.physicalPortName)
          packed.source.asLite :=> physical.asLite
        }

        peIO.sharedPacketOutputs.foreach { sharedOutput =>
          val physical = IO(chiselTypeOf(sharedOutput.source))
            .suggestName(sharedOutput.physicalPortName)
          sharedOutput.source.asFull :=> physical.asFull
        }

      }
  }

  def attachAieSharedTaskInput(
      taskName: String,
      peIndex: Int,
      portType: String,
      hardCilkPort: Data
  ): Boolean = {
    val key = (taskName, peIndex, portType)
    if (!sharedHardCilkInputSinks.contains(key)) {
      false
    } else {
      require(!attachedSharedHardCilkInputs.contains(key),
        s"HardCilk input $taskName[$peIndex].$portType was attached more than once")
      val stream = hardCilkPort.asInstanceOf[axi4s.Interface]
      val (expectedWidth, sink) = sharedHardCilkInputSinks(key)
      require(stream.cfg.wData == expectedWidth && stream.cfg.onlyRV,
        s"HardCilk input $taskName[$peIndex].$portType must be a $expectedWidth-bit ready/valid AXI stream")
      stream.asLite :=> sink.asLite
      attachedSharedHardCilkInputs += key
      true
    }
  }

  def attachAieSharedTaskOutput(
      taskName: String, peIndex: Int, portType: String, hardCilkPort: Data
  ): Boolean = {
    val key = (taskName, peIndex, portType)
    sharedHardCilkOutputSources.get(key) match {
      case None => false
      case Some((expectedWidth, source)) =>
        require(!attachedSharedHardCilkOutputs.contains(key),
          s"HardCilk output $key was attached more than once")
        val stream = hardCilkPort.asInstanceOf[axi4s.Interface]
        require(stream.cfg.wData == expectedWidth && stream.cfg.onlyRV,
          s"HardCilk output $key must be a $expectedWidth-bit ready/valid AXI stream")
        source.asLite :=> stream.asLite
        attachedSharedHardCilkOutputs += key
        true
    }
  }

  def validateAieSharedTaskInputs(): Unit = {
    val missing = sharedHardCilkInputSinks.keySet.toSet -- attachedSharedHardCilkInputs
    require(missing.isEmpty,
      s"Configured AIE packet-mux HardCilk inputs were not found: ${missing.toSeq.sorted.mkString(", ")}")
    val missingOutputs = sharedHardCilkOutputSources.keySet.toSet -- attachedSharedHardCilkOutputs
    require(missingOutputs.isEmpty,
      s"Configured AIE packet-demux HardCilk outputs were not found: ${missingOutputs.toSeq.sorted.mkString(", ")}")

  }

  def attachSplitAieOutput(
      taskName: String,
      peIndex: Int,
      portType: String,
      hardCilkPort: Data,
      endpointName: Option[String] = None
  ): Boolean = {
    val candidates = splitAieOutputSources.filter { case ((task, index, endpoint, port), _) =>
      task == taskName && index == peIndex && port == portType &&
        endpointName.forall(_ == endpoint)
    }.toSeq
    require(candidates.size <= 1,
      s"AIE output $taskName[$peIndex].$portType is ambiguous; specify its endpoint")
    candidates.headOption match {
      case None => false
      case Some((key, source)) =>
        require(!attachedSplitAieOutputs(key),
          s"AIE output $taskName[$peIndex].$portType was attached more than once")
        source.asLite :=> hardCilkPort.asInstanceOf[axi4s.Interface].asLite
        attachedSplitAieOutputs += key
        true
    }
  }

  def validateSplitAieOutputs(): Unit = {
    val missing = splitAieOutputSources.keySet.toSet -- attachedSplitAieOutputs
    require(missing.isEmpty,
      s"Configured internal AIE splitter outputs were not attached: ${missing.toSeq.sorted.mkString(", ")}")
  }

  /**
   * This method is now part of the trait. It contains the exact logic
   * moved from CleanHardCilk.scala.
   */
  def buildAndConnectHBM(
      peMap: Map[String, Seq[VitisWriteBufferModule]],
      schedulerMap: Map[String, Scheduler],
      closureAllocatorMap: Map[String, Allocator],
      argumentNotifierMap: Map[String, ArgumentNotifier],
      memoryAllocatorMap: Map[String, Allocator],
      spawnNextWBMap: Map[String, Seq[WriteBuffer]],
      sendArgumentWBMap: Map[String, Seq[WriteBuffer]],
      remoteMemAccessMap: Map[String, RemoteStreamToMem]
  ): Unit = {

    // [This is the code block from CleanHardCilk.scala, line 316 to 512]

    val interfacesPE = new ArrayBuffer[axi4.full.Interface]()


    peMap.foreach { case (taskName, peArray) =>
      val task = fullSysGenDescriptor.taskDescriptors.find(_.name == taskName).get
      peArray.foreach { pe =>
        pe.io.elements.get("m_axi_spawnNext").foreach(p => interfacesPE.addOne(p.asInstanceOf[axi4.RawInterface].asFull))
        pe.io.elements.get("m_axi_argOut").foreach(p => interfacesPE.addOne(p.asInstanceOf[axi4.RawInterface].asFull))
        if (task.hasAXI) {
          interfacesPE.addOne(pe.getPort("m_axi_gmem").asInstanceOf[axi4.RawInterface].asFull)
        }
      }
    }
    // for all tasks that do not have hdlpath for their PEs
    // we create and export pe_io interfaces associated with the task if they exist
    val aiePEs = fullSysGenDescriptor.taskDescriptors.filter(_.peHDLPath.isEmpty);
    for (task <- aiePEs) {
      val subPEs = fullSysGenDescriptor.subPEList.filter(_._2.peName == task.name)
      if (subPEs.nonEmpty) {
        val peIO = getOrCreatePeIO(task)
        peIO.axiMasters.foreach { port =>
          interfacesPE.addOne(port)
        }
        peIO.rwPorts.foreach { rwPort =>
          println(s"[HBMInterconnect] Adding PE IO ports for task ${rwPort.subPEName}_${rwPort.index}")
          val request = subPEs(rwPort.subPEName).rwRequest.get
          val portType = (request.`type`, request.mode) match {
            case ("read", "single") => s"readSingle${request.portWidth}Out"
            case ("read", "stream") => s"readStream${request.portWidth}Out"
            case ("write", "single") => s"writeSingle${request.portWidth}Out"
            case ("write", "stream") => s"writeStream${request.portWidth}Out"
          }
          val endpointName = rwPort.subPEName.toLowerCase.replaceAll("[^a-z0-9_]", "")
          if (!peIO.sharedRwRequestKeys.contains((rwPort.subPEName, rwPort.index)) &&
              !attachSplitAieOutput(task.name, rwPort.index, portType,
                rwPort.sourceTask, Some(endpointName))) {
            val sourceTask = IO(chiselTypeOf(rwPort.sourceTask))
              .suggestName(s"${rwPort.instanceName}_sourceTask")
            sourceTask.asLite :=> rwPort.sourceTask.asLite
          }

          rwPort.sinkResult
            .filterNot(_ => peIO.sharedReadResponseKeys.contains((rwPort.subPEName, rwPort.index)))
            .foreach { sink =>
            val sinkResult =
              IO(chiselTypeOf(sink)).suggestName(s"${rwPort.instanceName}_sinkResult")
            sink.asFull :=> sinkResult.asFull
          }
        }
      }
    }

    spawnNextWBMap.foreach {
      case (taskName, wbArray) =>
        wbArray.foreach { wb =>
          interfacesPE.addOne(wb.m_axi.asInstanceOf[axi4.RawInterface].asFull)
        }
    }

    sendArgumentWBMap.foreach {
      case (taskName, wbArray) =>
        wbArray.foreach { wb =>
          interfacesPE.addOne(wb.m_axi.asInstanceOf[axi4.RawInterface].asFull)
        }
    }

    val interfacesScheduler = schedulerMap.values.flatMap(_.io_internal.vss_axi_full).to(ArrayBuffer)
    schedulerMap.values.foreach { s =>
      if (s.spawnerServerAXI.isDefined) {
        interfacesScheduler.addAll(s.spawnerServerAXI.get)
      }
    }

    val interfacesClosureAllocator = closureAllocatorMap.values.flatMap(_.io_internal.vcas_axi_full).to(ArrayBuffer)
    val interfacesArgumentNotifier = argumentNotifierMap.values.flatMap(_.axi_full_argRoute).to(ArrayBuffer)
    val interfacesMemoryAllocator = memoryAllocatorMap.values.flatMap(_.io_internal.vcas_axi_full).to(ArrayBuffer)

    val interfacesRemoteMemAccess = remoteMemAccessMap.values.flatMap(v => Seq(v.io.m_axi_mem)).to(ArrayBuffer)

    val numHBMPorts = reduceAxi
    val hbmSlaves =
      scala.collection.mutable.Map[Int, ArrayBuffer[axi4.full.Interface]]()
    for (i <- 0 until numHBMPorts) {
      hbmSlaves += (i -> new ArrayBuffer[axi4.full.Interface]())
    }

    val totalPorts =
      interfacesPE.length + interfacesMemoryAllocator.length + interfacesScheduler.length + interfacesClosureAllocator.length + interfacesArgumentNotifier.length + interfacesRemoteMemAccess.length

    // log the number of total ports
    println(s"[HBM:Interconnect:92] Total ports: $totalPorts")

    // log the interfaces from each module
    println(s"[HBM:Interconnect:95] PE interfaces: ${interfacesPE.length}")
    println(s"[HBM:Interconnect:96] Scheduler interfaces: ${interfacesScheduler.length}")
    println(s"[HBM:Interconnect:97] Closure Allocator interfaces: ${interfacesClosureAllocator.length}")
    println(s"[HBM:Interconnect:98] Argument Notifier interfaces: ${interfacesArgumentNotifier.length}")
    println(s"[HBM:Interconnect:99] Memory Allocator interfaces: ${interfacesMemoryAllocator.length}")


    if (totalPorts > 0) {
        val numPortsPerMux = totalPorts.toDouble / numHBMPorts.toDouble
        // Reserve an exported memory port for scheduler/notifier traffic.
        // At high PE counts ceil(PE / average) can otherwise consume every
        // port, silently dropping all server interfaces from hbmSlaves.
        val hasServers = totalPorts > interfacesPE.length
        require(numHBMPorts >= (if (interfacesPE.nonEmpty && hasServers) 2 else 1),
          "Separate PE/server memory muxes require at least two AXI ports")
        val peMux = if (interfacesPE.isEmpty) 0 else math.min(
          numHBMPorts - (if (hasServers) 1 else 0),
          math.max(1, math.ceil(1.0 * interfacesPE.length / numPortsPerMux).toInt))
        val serverMux = numHBMPorts - peMux

        val pePortsPerMux = if (peMux > 0 && interfacesPE.length > 0) 1.0 * interfacesPE.length / peMux else 1.0

        interfacesPE.zipWithIndex
          .groupBy(x => (x._2.toDouble / pePortsPerMux).toInt)
          .foreach(x => {
            if (hbmSlaves.contains(x._1)) hbmSlaves(x._1).addAll(x._2.map(_._1))
          })

        val serverInterfaces = interfacesMemoryAllocator ++ interfacesScheduler ++ interfacesClosureAllocator ++ interfacesArgumentNotifier ++ interfacesRemoteMemAccess

        val serverPortsPerMuxClamped = if (serverInterfaces.length > 0 && serverMux > 0) (1.0 * serverInterfaces.length / serverMux) else 1.0

        serverInterfaces.zipWithIndex
          .groupBy(x => peMux + (x._2.toDouble / serverPortsPerMuxClamped).toInt)
          .foreach(x => {
             if (hbmSlaves.contains(x._1)) hbmSlaves(x._1).addAll(x._2.map(_._1))
          })
    }


    require(hbmSlaves.values.map(_.size).sum == totalPorts,
      "Every memory interface must be assigned to an exported AXI port")

    if (false){//!isSimulation) {
      val xdma_axi = IO(axi4.Slave(cfgXDMA)).suggestName("s_axi_xdma")
      hbmSlaves(numHBMPorts - 1).addOne(
        axi4.full.SlaveBuffer(xdma_axi.asFull, axi4.BufferConfig.all(8))
      )
      interfaceBuffer.addOne(
        hdlinfo.Interface(
          "s_axi_xdma", hdlinfo.InterfaceRole.slave, hdlinfo.InterfaceKind("axi4"),
          "clock", "reset", Map("config" -> hdlinfo.TypedObject(cfgXDMA))
        )
      )
      axiXDMA.addOne(xdma_axi)
    }

    val axi3CompatFlag = false
    numHbmPortExports = hbmSlaves.filter(_._2.length > 0).size
    hbmSlaves.filter(_._2.length > 0).zipWithIndex.map {
      case (hbmSlaves_i, i) => {
        val interfaceCount = hbmSlaves_i._2.length
        val hbmSlave = hbmSlaves_i._2

        if (
          interfaceCount == 1 && hbmSlave.head.cfg.axi3Compat && hbmSlave.head.cfg.wData == 256
        ) {
          val axiOut =
            IO(axi4.Master(hbmSlave.head.cfg)).suggestName(f"m_axi_${i}%02d")
          val axpbs = Module(new AxiPageBoundarySplitter_Basic(new AxiPageBoundarySplitter_Config(
            addressWidth = hbmSlave.head.cfg.wAddr,
            dataWidth = hbmSlave.head.cfg.wData,
            alignmentBits = 12, // 4 KB alignment
            idWidth = hbmSlave.head.cfg.wId,
            numberOfOutstanding = 16
          )))
          axpbs.s_axi :=> hbmSlave.head
          axiOut.asFull :=> axpbs.m_axi.asFull
          interfaceBuffer.addOne(
            hdlinfo.Interface(
              f"m_axi_${i}%02d", hdlinfo.InterfaceRole.master, hdlinfo.InterfaceKind("axi4"),
              "clock", "reset", Map("config" -> hdlinfo.TypedObject(axiOut.cfg))
            )
          )
          axiOuts.addOne(axiOut)
        } else if (interfaceCount > 1) {
          val mux = Module(
            new axi4.full.components.Mux(
              new axi4.full.components.MuxConfig(
                axiSlaveCfg = cfgAxi4HBM.copy(axi3Compat = axi3CompatFlag, wId = 2),
                numSlaves = hbmSlave.length
              )
            )
          )

          mux.s_axi.zip(hbmSlave).foreach { case (muxPort, slavePort) =>
            val protocolConverter = Module(
              new axi4.full.components.ProtocolConverter(
                new axi4.full.components.ProtocolConverterConfig(
                  axiSlaveCfg = slavePort.cfg.copy(wUserAR = 0, wUserR = 0, wUserAW = 0, wUserW = 0, wUserB = 0),
                  axiMasterCfg = muxPort.cfg
                )
              )
            )
            axi4.full.SlaveBuffer(AxiUserYanker(slavePort), axi4.BufferConfig.all(8)) :=> protocolConverter.s_axi

            // if the slave cfg has data width smaller than the axi master config instantiate a Widen
            if(slavePort.cfg.wData < muxPort.cfg.wData){
              val widen_mod = Module(new chext.amba.axi4.full.components.Widen(chext.amba.axi4.full.components.WidenConfig(muxPort.cfg)))
              protocolConverter.m_axi :=> widen_mod.s_axi
              widen_mod.m_axi :=> muxPort
            } else{
              protocolConverter.m_axi :=> muxPort
            }
          }

          val axiOut = IO(axi4.Master(mux.m_axi.cfg)).suggestName(f"m_axi_${i}%02d")

          if (addressTransformFlag) {
            val addressTransform = Module(new Util.AddressTransform(
              AddressTransformConfig(
                axiCfg = axiOut.cfg,
                transform = Seq(33, 23, 22, 21, 20, 28, 27, 26, 25, 24, 32, 31, 30, 29, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0).reverse
              )
            ))
            val axpbs = Module(new AxiPageBoundarySplitter_Basic(new AxiPageBoundarySplitter_Config(
              addressWidth = axiOut.cfg.wAddr,
              dataWidth = axiOut.cfg.wData,
              alignmentBits = 12, // 4 KB alignment
              idWidth = axiOut.cfg.wId,
              numberOfOutstanding = 16
            )))
            mux.m_axi :=> addressTransform.s_axi
            addressTransform.m_axi :=> axpbs.s_axi.asFull
            axpbs.m_axi :=> axiOut.asFull
          } else {
            val axpbs = Module(new AxiPageBoundarySplitter_Basic(new AxiPageBoundarySplitter_Config(
              addressWidth = axiOut.cfg.wAddr,
              dataWidth = axiOut.cfg.wData,
              alignmentBits = 12, // 4 KB alignment
              idWidth = axiOut.cfg.wId,
              numberOfOutstanding = 16
            )))
            mux.m_axi :=> axpbs.s_axi.asFull
            axpbs.m_axi :=> axiOut.asFull
          }

          interfaceBuffer.addOne(
            hdlinfo.Interface(
              f"m_axi_${i}%02d", hdlinfo.InterfaceRole.master, hdlinfo.InterfaceKind("axi4"),
              "clock", "reset", Map("config" -> hdlinfo.TypedObject(axiOut.cfg))
            )
          )
          axiOuts.addOne(axiOut)
        } else {
          val outputCfg = cfgAxi4HBM.copy(axi3Compat = axi3CompatFlag, wId = 2)
          val axiOut =
            IO(axi4.Master(outputCfg)).suggestName(f"m_axi_${i}%02d")
          val protocolConverter = Module(
            new axi4.full.components.ProtocolConverter(
              new axi4.full.components.ProtocolConverterConfig(
                axiSlaveCfg = hbmSlave.head.cfg.copy(wUserAR = 0, wUserR = 0, wUserAW = 0, wUserW = 0, wUserB = 0),
                axiMasterCfg = outputCfg
              )
            )
          )
          axi4.full.SlaveBuffer(AxiUserYanker(hbmSlave.head), axi4.BufferConfig.all(2)) :=> protocolConverter.s_axi

          if (addressTransformFlag) {
             val addressTransform = Module(new Util.AddressTransform(
              AddressTransformConfig(
                axiCfg = axiOut.cfg,
                transform = Seq(33, 23, 22, 21, 20, 28, 27, 26, 25, 24, 32, 31, 30, 29, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0).reverse
              )
            ))
            // #TODO add the widen here as well.
            val axpbs = Module(new AxiPageBoundarySplitter_Basic(new AxiPageBoundarySplitter_Config(
              addressWidth = axiOut.cfg.wAddr,
              dataWidth = axiOut.cfg.wData,
              alignmentBits = 12, // 4 KB alignment
              idWidth = axiOut.cfg.wId,
              numberOfOutstanding = 16
            )))
            protocolConverter.m_axi :=> addressTransform.s_axi
            addressTransform.m_axi :=> axpbs.s_axi.asFull
            axpbs.m_axi :=> axiOut.asFull
          } else {
            // Add the Widen for V80
            if(protocolConverter.s_axi.cfg.wData < axiOut.cfg.wData){
              val widen_mod = Module(new chext.amba.axi4.full.components.Widen(chext.amba.axi4.full.components.WidenConfig(axiOut.cfg)))
              val axpbs = Module(new AxiPageBoundarySplitter_Basic(new AxiPageBoundarySplitter_Config(
                addressWidth = axiOut.cfg.wAddr,
                dataWidth = axiOut.cfg.wData,
                alignmentBits = 12, // 4 KB alignment
                idWidth = axiOut.cfg.wId,
                numberOfOutstanding = 16
              )))
              protocolConverter.m_axi :=> widen_mod.s_axi
              widen_mod.m_axi :=> axpbs.s_axi.asFull
              axpbs.m_axi :=> axiOut.asFull
            } else{
              val axpbs = Module(new AxiPageBoundarySplitter_Basic(new AxiPageBoundarySplitter_Config(
                addressWidth = axiOut.cfg.wAddr,
                dataWidth = axiOut.cfg.wData,
                alignmentBits = 12, // 4 KB alignment
                idWidth = axiOut.cfg.wId,
                numberOfOutstanding = 16
              )))
              protocolConverter.m_axi :=> axpbs.s_axi.asFull
              axpbs.m_axi :=> axiOut.asFull
            }

          }

          interfaceBuffer.addOne(
            hdlinfo.Interface(
              f"m_axi_${i}%02d", hdlinfo.InterfaceRole.master, hdlinfo.InterfaceKind("axi4"),
              "clock", "reset", Map("config" -> hdlinfo.TypedObject(axiOut.cfg))
            )
          )
          axiOuts.addOne(axiOut)
        }
      }
    }
  }
}
