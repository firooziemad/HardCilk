package Descriptors


import chisel3.util.isPow2
import scala.collection.mutable
import org.slf4j.{LoggerFactory, Logger} // For logging warnings
import scala.collection.mutable.ListBuffer

// --- Helper Objects ---

// Use a common logger for all descriptor warnings
object DescriptorLogger {
  val logger: Logger = LoggerFactory.getLogger(this.getClass)
}

case class MemSystemDescriptor(
    var schedulerServersBaseAddresses: Seq[Int] = Seq.empty,
    var allocationServersBaseAddresses: Seq[Int] = Seq.empty,
    var memoryAllocatorServersBaseAddresses: Seq[Int] = Seq.empty
)

case class PortDescriptor(
    parentName: String,
    parentType: String,
    parentIndex: Int = 0, // Defaulted
    portType: String,
    portIndex: Int = 0  // Defaulted
) {
  def validate(): Unit = {
    require(parentType == "HardCilk" || parentType == "PE" || parentType == "mem", s"Invalid parentType: $parentType")
    require(parentIndex >= 0, "parentIndex must be >= 0")
    require(
      Set("taskIn", "taskOut", "taskInGlobal", "taskOutGlobal", "argIn", "argOut",
          "closureIn", "closureOut", "mallocIn", "mallocOut").contains(portType),
      s"Invalid portType: $portType"
    )
    require(portIndex >= 0, "portIndex must be >= 0")
  }

  def getFormatedPortName(descriptor: FullSysGenDescriptor): String = {
    // ... (this logic remains the same)
    if (parentType == "PE") {
      val portTypeMap = Map("argOut" -> "addrOut", "closureIn" -> "contIn")
      f"${parentName}_${parentIndex}/${portTypeMap.getOrElse(portType, portType)}"
    } else if (parentType == "HardCilk") {
      val side =
        if (portType == "taskIn" || portType == "taskOut") "scheduler"
        else if (portType == "closureOut") "closureAllocator"
        else if (portType == "argIn") "argumentNotifier"
        else "memoryAllocator"
      f"${descriptor.name}_0/${parentName}_${side}_${portType}_${portIndex}"
    } else {
      "err"
    }
  }
}

case class ConnectionDescriptor(
    srcPort: PortDescriptor,
    dstPort: PortDescriptor,
    bitWidth: Int = 0, // Defaulted
    connectionType: String = "AXIS" // Defaulted
) {
  def validate(): Unit = {
    srcPort.validate()
    dstPort.validate()
  }
}

case class SystemConnections(
    connections: List[ConnectionDescriptor]
)

// ... (MemStats, InterconnectDescriptor remain the same) ...
case class InterconnectDescriptor(
    count: Int,
    ratio: Int
)

case class MemStats(
    totalAXIPorts: Int,
    interconnectDescriptors: List[InterconnectDescriptor]
)

case class RWRequestDescriptor(
    `type`: String,
    mode: String,
    portWidth: Int,
    nextsubPE: Option[String] = None
) {
  def validate(subPEName: String): Unit = {
    require(Set("read", "write").contains(`type`),
      s"subPE '$subPEName': rwRequest.type must be one of read|write")
    require(Set("single", "stream").contains(mode),
      s"subPE '$subPEName': rwRequest.mode must be one of single|stream")
    require(portWidth > 0,
      s"subPE '$subPEName': rwRequest.portWidth must be > 0")

    if (`type` == "read") {
      require(nextsubPE.nonEmpty,
        s"subPE '$subPEName': rwRequest.nextsubPE must exist when type is read")
    } else {
      require(nextsubPE.isEmpty,
        s"subPE '$subPEName': rwRequest.nextsubPE must not exist when type is write")
    }
  }
}

case class SubPEDescriptor(
    peName: String,
    rwRequest: Option[RWRequestDescriptor] = None,
    taskInputPorts: List[String] = List.empty,
    taskOutputPorts: List[String] = List.empty
) {
  def validate(subPEName: String): Unit = {
    val allowedTaskInputPorts = Set("taskInGlobal", "argIn", "closureIn", "mallocIn", "taskIn")
    val allowedTaskOutputPorts = Set("taskOutGlobal", "argOut", "closureOut", "mallocOut", "argDataOut", "spawnNext", "taskOut")

    require(peName.nonEmpty, s"subPE '$subPEName': peName must not be empty")
    rwRequest.foreach(_.validate(subPEName))

    require(taskInputPorts.distinct.length == taskInputPorts.length,
      s"subPE '$subPEName': taskInputPorts contains duplicate entries")
    require(taskOutputPorts.distinct.length == taskOutputPorts.length,
      s"subPE '$subPEName': taskOutputPorts contains duplicate entries")

    taskInputPorts.foreach { port =>
      require(allowedTaskInputPorts.contains(port),
        s"subPE '$subPEName': invalid taskInputPorts entry '$port'. Allowed: ${allowedTaskInputPorts.toList.sorted.mkString(",")}")
    }
    taskOutputPorts.foreach { port =>
      require(allowedTaskOutputPorts.contains(port),
        s"subPE '$subPEName': invalid taskOutputPorts entry '$port'. Allowed: ${allowedTaskOutputPorts.toList.sorted.mkString(",")}")
    }
  }
}

// --- SideConfig with default handling ---
case class SideConfig(
    sideType: String,
    numVirtualServers: Int = 0,
    capacityVirtualQueue: Int = 0,
    capacityPhysicalQueue: Int = 0,
    portWidth: Int = 32,
    virtualEntrtyWidth: Int = 0,
    numSpawnerServer: Int = 0
) {
  def validate(): Unit = {
    require(Set("scheduler", "allocator", "argumentNotifier", "memoryAllocator").contains(sideType),
      s"Invalid sideType: $sideType")

    if (portWidth == 32) { // '32' is the default
      DescriptorLogger.logger.warn(
        s"Task side '$sideType' is using default portWidth=32. " +
        "Ensure this is intended or specify 'portWidth' in the JSON."
      )
    }
  }
}

// --- TaskDescriptor with validation ---
case class TaskDescriptor(
    name: String,
    peVersion: String = "1.0",
    peHDLPath: String = "",
    isRoot: Boolean,
    isCont: Boolean,
    dynamicMemAlloc: Boolean,
    numProcessingElements: Int,
    widthTask: Int,
    widthMalloc: Int = 0, // Defaulted
    variableSpawn: Boolean = false, // Defaulted
    sidesConfigs: List[SideConfig],
    var mgmtBaseAddresses: MemSystemDescriptor = MemSystemDescriptor(),
    spawnServersCount: Int = 0, // Defaulted
    hasAXI: Boolean = true,
    isAIE: Boolean = false,
    generateSpawnNextWriteBuffer: Boolean = false,
    generateArgOutWriteBuffer: Boolean = false,
    argumentSizeList: List[Int] = List(),
    taskId: Int = 0 // Defaulted
) {
  // Helper methods are fine to keep here
  def getNumServers(sideType: String): Int = { //
    sidesConfigs.find(_.sideType == sideType).map(_.numVirtualServers).getOrElse(0)
  }
  def getCapacityVirtualQueue(sideType: String): Int = { //
    sidesConfigs.find(_.sideType == sideType).map(_.capacityVirtualQueue).getOrElse(0)
  }
  def getCapacityPhysicalQueue(sideType: String): Int = { //
    sidesConfigs.find(_.sideType == sideType).map(_.capacityPhysicalQueue).getOrElse(0)
  }
  // ... (other get... methods) ...

  def validate(): Unit = {
    sidesConfigs.foreach(_.validate())

    require(numProcessingElements > 0, s"Task '$name': numProcessingElements must be > 0")
    require(isPow2(widthTask) && widthTask <= 1024, s"Task '$name': widthTask must be power of 2 and <= 1024")

    if (peHDLPath.nonEmpty) {
      require(new java.io.File(peHDLPath).exists, s"Task '$name': peHDLPath not found at '$peHDLPath'")
    } else {
      DescriptorLogger.logger.warn(s"Task '$name' has no 'peHDLPath'. Ports will be exported.")
    }

    require(getNumServers("scheduler") > 0, s"Task '$name': must have > 0 scheduler servers")
    // ... (all other 'asserts' converted to 'require') ...

    require(dynamicMemAlloc && widthMalloc > 0 || !dynamicMemAlloc && widthMalloc == 0,
      s"Task '$name': dynamicMemAlloc requires widthMalloc > 0")

    if (isCont) {
      //require(getNumServers("allocator") > 0, s"Task '$name' (Cont): must have > 0 allocator servers")
      require(getNumServers("argumentNotifier") > 0, s"Task '$name' (Cont): must have > 0 argumentNotifier servers")
    }

    if (dynamicMemAlloc) {
       require(getNumServers("memoryAllocator") > 0, s"Task '$name' (DynMem): must have > 0 memoryAllocator servers")
    }

    if(generateArgOutWriteBuffer) {
      require(!argumentSizeList.isEmpty, s"Task '$name': argumentSizeList must not be empty!")
      require(argumentSizeList.head > 0, s"Task '$name': argumentWidth must be > 0 to has a write buffer!")
    }


  }
}

case class AIEPlioSharingMemberDescriptor(
    subPEName: String,
    portName: String,
    share: Int
) {
  def validate(groupLabel: String): Unit = {
    require(subPEName.matches("[A-Za-z_][A-Za-z0-9_]*"),
      s"AIE PLIO sharing group '$groupLabel': invalid subPEName '$subPEName'")
    require(portName.matches("[A-Za-z_][A-Za-z0-9_]*"),
      s"AIE PLIO sharing group '$groupLabel': invalid portName '$portName'")
    require(Set(2, 4).contains(share),
      s"AIE PLIO sharing group '$groupLabel': V9 verified FIFO-backed sharing supports only share=2 or share=4")
  }
}

/**
  * Compact user-facing description of one physical PLIO.
  *
  * A group currently contains one homogeneous logical endpoint and packetizes
  * `share` consecutive, previously-unassigned PE instances of that endpoint.
  * `single` mode synthesizes TLAST per transfer; `stream` mode preserves the
  * input TLAST and locks arbitration through the complete packet.
  */
case class AIEPlioSharingGroupDescriptor(
    direction: String,
    mode: String,
    members: List[AIEPlioSharingMemberDescriptor],
    physicalPlioWidth: Int = 128,
    fifoDepth: Int = 2,
    creditCapacity: Int = 1,
    initialCredits: Int = 1,
    creditReturnPort: String = "argOut",
    creditTiming: String = AiePlioCreditTiming.BeforeReceive,
    aieFifoDepth: Int = 0,
    segmentBeats: Int = 0,
    lengthDriven: Boolean = false,
    logicalWidth: Int = 0
) {
  def label(index: Int): String = s"group_$index"

  def validate(index: Int): Unit = {
    val groupLabel = label(index)
    require(Set("plToAie", "aieToPl").contains(direction),
      s"AIE PLIO sharing group '$groupLabel': direction must be plToAie or aieToPl")
    require(Set(AiePlioSharingMode.Single, AiePlioSharingMode.Stream).contains(mode),
      s"AIE PLIO sharing group '$groupLabel': mode must be 'single' or 'stream'")
    require(members.nonEmpty,
      s"AIE PLIO sharing group '$groupLabel': members must not be empty")
    require(members.length == 1,
      s"AIE PLIO sharing group '$groupLabel': packet muxes currently support exactly one homogeneous member")
    members.foreach { member =>
      if (direction == "plToAie") member.validate(groupLabel)
      else {
        require(member.subPEName.matches("[A-Za-z_][A-Za-z0-9_]*") &&
          member.portName.matches("[A-Za-z_][A-Za-z0-9_]*") &&
          member.share >= 2 && member.share <= 32,
          s"AIE PLIO sharing group '$groupLabel': invalid output member or share")
      }
    }
    require(Set(32, 64, 128).contains(physicalPlioWidth),
      s"AIE PLIO sharing group '$groupLabel': physicalPlioWidth must be 32, 64, or 128")
    require(fifoDepth > 0,
      s"AIE PLIO sharing group '$groupLabel': fifoDepth must be positive")
    require(logicalWidth >= 0 && logicalWidth % 32 == 0,
      s"AIE PLIO sharing group '$groupLabel': optional logicalWidth must be a multiple of 32")
    if (direction == "aieToPl") {
      require(mode == AiePlioSharingMode.Single && physicalPlioWidth == 32,
        s"AIE PLIO sharing group '$groupLabel': direct output sharing requires single mode and a 32-bit PLIO")
      return
    }
    require(Set(AiePlioCreditTiming.BeforeReceive, AiePlioCreditTiming.AfterConsume,
      AiePlioCreditTiming.IterationEnd).contains(creditTiming),
      s"AIE PLIO sharing group '$groupLabel': invalid creditTiming")
    require(creditCapacity > 0,
      s"AIE PLIO sharing group '$groupLabel': creditCapacity must be positive")
    require(initialCredits >= 0 && initialCredits <= creditCapacity,
      s"AIE PLIO sharing group '$groupLabel': initialCredits must be in [0, creditCapacity]")
    require(aieFifoDepth >= 0,
      s"AIE PLIO sharing group '$groupLabel': aieFifoDepth must be non-negative")
    require(segmentBeats >= 0,
      s"AIE PLIO sharing group '$groupLabel': segmentBeats must be non-negative")
    require(creditReturnPort.matches("[A-Za-z_][A-Za-z0-9_]*"),
      s"AIE PLIO sharing group '$groupLabel': invalid creditReturnPort '$creditReturnPort'")
    if (mode == AiePlioSharingMode.Stream) {
      require(segmentBeats > 0 && lengthDriven,
        s"AIE PLIO sharing group '$groupLabel': V5 stream mode requires segmentBeats > 0 and lengthDriven=true")
    }
  }
}

object AiePlioSharingMode {
  val Single = "single"
  val Stream = "stream"
}


object AiePlioCreditTiming {
  val BeforeReceive = "beforeReceive"
  val AfterConsume = "afterConsume"
  val IterationEnd = "iterationEnd"
}

object AiePlioSourceKind {
  val HardCilkInput = "hardCilkInput"
  val ReadResponse = "readResponse"
  val HardCilkOutput = "hardCilkOutput"
  val RwRequest = "rwRequest"
  val PackedOutput = "packedOutput"
}

/** Fully expanded physical group consumed by RTL and AIE generators. */
case class AIEPlioPhysicalSharingGroupDescriptor(
    name: String,
    direction: String,
    mode: String,
    taskName: String,
    subPEName: String,
    portName: String,
    peIndices: List[Int],
    members: List[String],
    physicalPortName: String,
    physicalPlioWidth: Int,
    fifoDepth: Int,
    logicalWidth: Int,
    sourceKind: String,
    sourceSubPEName: String,
    creditCapacity: Int,
    initialCredits: Int,
    creditReturnPort: String,
    creditTiming: String,
    aieFifoDepth: Int,
    segmentBeats: Int,
    lengthDriven: Boolean,
    creditTag: Int
) {
  @transient private var resolvedPacketIds: Vector[Int] = Vector.empty

  def fanIn: Int = peIndices.length

  /** Number of 32-bit payload words reserved by one admission credit. */
  def receiveUnitWords: Int = {
    require(logicalWidth > 0 && logicalWidth % 32 == 0,
      s"AIE PLIO sharing group '$name': logical width must be a positive multiple of 32")
    val beats = if (mode == AiePlioSharingMode.Stream) segmentBeats else 1
    (logicalWidth / 32) * beats
  }

  /** Exact requested FIFO depth for every packet-split destination branch. */
  def receiveFifoDepths: Vector[Int] =
    Vector.tabulate(fanIn)(lane => aieFifoDepth + lane)

  /** Complete fixed records or bounded segments held by each receive FIFO. */
  def receiveCreditCapacities: Vector[Int] =
    receiveFifoDepths.map(_ / receiveUnitWords)

  def effectivePhysicalPortName: String =
    if (physicalPortName.nonEmpty) physicalPortName else name
  def creditReturnPhysicalPortName(peIndex: Int): String = {
    require(peIndices.contains(peIndex),
      s"AIE PLIO sharing group '$name' does not contain PE $peIndex")
    s"${name}_credit_$peIndex"
  }
  def segmented: Boolean = segmentBeats > 0

  def packetPortWidth: Int =
    mode match {
      case AiePlioSharingMode.Single if direction == "aieToPl" => physicalPlioWidth
      case AiePlioSharingMode.Single => logicalWidth + 32
      case AiePlioSharingMode.Stream => physicalPlioWidth
      case other => throw new IllegalArgumentException(
        s"AIE PLIO sharing group '$name': unsupported mode '$other'"
      )
    }

  def resolvePacketIds(ids: Seq[Int]): Unit = {
    val resolved = ids.toVector
    require(resolved.length == peIndices.length,
      s"AIE PLIO sharing group '$name': compiler report returned ${resolved.length} packet IDs for ${peIndices.length} branches")
    require(resolved.distinct.length == resolved.length,
      s"AIE PLIO sharing group '$name': compiler-assigned packet IDs must be unique")
    require(resolved.forall(id => id >= 0 && id < 32),
      s"AIE PLIO sharing group '$name': compiler-assigned packet IDs must be in [0, 31]")
    resolvedPacketIds = resolved
  }

  def packetIds: Vector[Int] = {
    require(resolvedPacketIds.nonEmpty,
      s"AIE PLIO sharing group '$name': packet IDs have not been resolved from the AIE compiler report")
    resolvedPacketIds
  }

  def validate(descriptor: FullSysGenDescriptor): Unit = {
    require(name.matches("[A-Za-z_][A-Za-z0-9_]*"),
      s"AIE PLIO sharing group '$name': invalid group name")
    require(effectivePhysicalPortName.matches("[A-Za-z_][A-Za-z0-9_]*"),
      s"AIE PLIO sharing group '$name': invalid physicalPortName")
    require(Set("plToAie", "aieToPl").contains(direction),
      s"AIE PLIO sharing group '$name': unsupported direction '$direction'")
    require(if (direction == "plToAie") Set(2, 4).contains(fanIn) else fanIn >= 2 && fanIn <= 32,
      s"AIE PLIO sharing group '$name': unsupported fan-in $fanIn")
    require(members.length == fanIn,
      s"AIE PLIO sharing group '$name': peIndices and members must have the same length")
    require(peIndices.distinct.length == peIndices.length,
      s"AIE PLIO sharing group '$name': peIndices must be unique")
    require(members.distinct.length == members.length,
      s"AIE PLIO sharing group '$name': members must be unique")
    require(Set(32, 64, 128).contains(physicalPlioWidth),
      s"AIE PLIO sharing group '$name': physicalPlioWidth must be 32, 64, or 128")
    require(fifoDepth > 0,
      s"AIE PLIO sharing group '$name': fifoDepth must be positive")
    require(Set(AiePlioSharingMode.Single, AiePlioSharingMode.Stream).contains(mode),
      s"AIE PLIO sharing group '$name': mode must be 'single' or 'stream'")
    if (direction == "aieToPl") {
      require(mode == AiePlioSharingMode.Single && physicalPlioWidth == 32,
        s"AIE PLIO sharing group '$name': direct output sharing requires single mode and a 32-bit PLIO")
      require(logicalWidth > 0 && logicalWidth % 32 == 0,
        s"AIE PLIO sharing group '$name': output record width must be a positive multiple of 32")
      require(Set(AiePlioSourceKind.HardCilkOutput, AiePlioSourceKind.RwRequest,
        AiePlioSourceKind.PackedOutput).contains(sourceKind),
        s"AIE PLIO sharing group '$name': unsupported output source '$sourceKind'")
      val task = descriptor.taskDescriptors.find(_.name == taskName).getOrElse(
        throw new IllegalArgumentException(s"AIE PLIO sharing group '$name': unknown task '$taskName'"))
      require(peIndices.forall(i => i >= 0 && i < task.numProcessingElements),
        s"AIE PLIO sharing group '$name': PE index out of range")
      return
    }
    require(Set(AiePlioCreditTiming.BeforeReceive, AiePlioCreditTiming.AfterConsume,
      AiePlioCreditTiming.IterationEnd).contains(creditTiming),
      s"AIE PLIO sharing group '$name': unsupported V5 creditTiming '$creditTiming'")
    require(creditCapacity > 0 && initialCredits >= 0 && initialCredits <= creditCapacity,
      s"AIE PLIO sharing group '$name': invalid V5 credit bounds")
    require(aieFifoDepth >= 0 && segmentBeats >= 0,
      s"AIE PLIO sharing group '$name': FIFO depth and segment size must be non-negative")
    require(creditReturnPort.matches("[A-Za-z_][A-Za-z0-9_]*"),
        s"AIE PLIO sharing group '$name': invalid creditReturnPort '$creditReturnPort'")
    if (mode == AiePlioSharingMode.Stream) {
    require(creditTag >= 0,
      s"AIE PLIO sharing group '$name': creditTag must be non-negative")
      require(segmentBeats > 0 && lengthDriven,
        s"AIE PLIO sharing group '$name': V5 stream mode requires segmentBeats > 0 and lengthDriven=true")
    }
    require(logicalWidth > 0 && logicalWidth % 8 == 0,
      s"AIE PLIO sharing group '$name': logical width must be a positive multiple of eight")
    require(creditTiming == AiePlioCreditTiming.AfterConsume,
      s"AIE PLIO sharing group '$name': V9 FIFO-backed credits must return after consumption")
    require(creditCapacity == 1 && initialCredits == 1,
      s"AIE PLIO sharing group '$name': creditCapacity and initialCredits are compatibility markers fixed at 1; V9 derives actual per-lane credits from receive FIFO capacity")
    val unitBytes = (logicalWidth / 8) * (if (mode == AiePlioSharingMode.Stream) segmentBeats else 1)
    require(logicalWidth % 32 == 0 && unitBytes > 0 && unitBytes <= 4096,
      s"AIE PLIO sharing group '$name': slot must contain 1..1024 complete 32-bit words")
    require(aieFifoDepth > 0 && aieFifoDepth + fanIn - 1 < 40,
      s"AIE PLIO sharing group '$name': distinct per-branch stream-switch FIFO depths must remain in [1,39]")
    require(receiveCreditCapacities.forall(_ > 0),
      s"AIE PLIO sharing group '$name': every receive FIFO must hold at least one complete record or segment")
    if (mode == AiePlioSharingMode.Stream) {
      require(logicalWidth >= 8 && isPow2(logicalWidth),
        s"AIE PLIO sharing group '$name': stream mode requires a byte-aligned power-of-two logical width")
    }
    require(Set(AiePlioSourceKind.HardCilkInput, AiePlioSourceKind.ReadResponse).contains(sourceKind),
      s"AIE PLIO sharing group '$name': unsupported logical source kind '$sourceKind'")
    require(subPEName.nonEmpty && portName.nonEmpty,
      s"AIE PLIO sharing group '$name': resolved endpoint names must not be empty")
    if (sourceKind == AiePlioSourceKind.ReadResponse) {
      require(sourceSubPEName.nonEmpty,
        s"AIE PLIO sharing group '$name': a read response must identify its PL-side read helper")
    }

    val task = descriptor.taskDescriptors.find(_.name == taskName).getOrElse(
      throw new IllegalArgumentException(
        s"AIE PLIO sharing group '$name': unknown task '$taskName'"
      )
    )
    require(peIndices.forall(index => index >= 0 && index < task.numProcessingElements),
      s"AIE PLIO sharing group '$name': a PE index is outside task '$taskName' range [0, ${task.numProcessingElements - 1}]")

  }
}

// --- FullSysGenDescriptor with validation ---
case class FullSysGenDescriptor(
    name: String,
    widthAddress: Int,
    widthContCounter: Int,
    taskDescriptors: List[TaskDescriptor],
    spawnList: Map[String, List[String]],
    spawnNextList: Map[String, List[String]],
    sendArgumentList: Map[String, List[String]],
    mallocList: Map[String, List[String]] = Map.empty,
    subPEList: Map[String, SubPEDescriptor] = Map.empty,
    // cfgAxiHardCilk: chext.amba.axi4.Config = chext.amba.axi4.Config(), // This class is not defined, commenting out
    targetFrequency: Int = 250,
    memorySizeSim: Int = 1,
    fpgaModel: String = "ALVEO_U55C",
    isVitisProject: Boolean = false,
    keepAXI4Interfaces: Boolean = false,
    mFPGASynth: Boolean = false,
    mFPGASimulation: Boolean = false,
    maximumAXIPorts: Int = 32,
    hasAXIDMAInput: Boolean = false,
    transformAXI: Boolean = false,
    transformPattern: List[Int] = List(),
    widthAXIAddress: Int = 34,
    fpgaCountSim: Int = 1,
    aiePlioSharingGroups: List[AIEPlioSharingGroupDescriptor] = List.empty
) {
  /**
    * Expand the compact JSON groups once so packet-ID resolution and every
    * downstream generator operate on the same mutable physical descriptors.
    */
  lazy val expandedAiePlioSharingGroups: List[AIEPlioPhysicalSharingGroupDescriptor] = {
    val nextPeIndex = mutable.Map.empty[(String, String), Int].withDefaultValue(0)

    aiePlioSharingGroups.zipWithIndex.map { case (group, groupIndex) =>
      group.validate(groupIndex)
      val member = group.members.head
      val groupLabel = group.label(groupIndex)
      val sharingMode = group.mode
      val subPE = subPEList.getOrElse(member.subPEName,
        throw new IllegalArgumentException(
          s"AIE PLIO sharing group '$groupLabel': unknown subPE '${member.subPEName}'"))

      val task = taskDescriptors.find(_.name == subPE.peName).getOrElse(
        throw new IllegalArgumentException(
          s"AIE PLIO sharing group '$groupLabel': subPE '${member.subPEName}' references unknown task '${subPE.peName}'"))

      val isHardCilkInput = group.direction == "plToAie" &&
        (member.portName == "taskIn" || subPE.taskInputPorts.contains(member.portName))

      val incomingReadResponses = subPEList.toSeq.flatMap {
        case (sourceSubPEName, sourceSubPE) if sourceSubPE.peName == task.name =>
          sourceSubPE.rwRequest.toSeq.collect {
            case req if req.`type` == "read" && req.nextsubPE.contains(member.subPEName) =>
              val mode = if (req.mode == "single") "Single" else "Stream"
              (sourceSubPEName, req, s"read${mode}${req.portWidth}In")
          }
        case _ => Seq.empty
      }.filter(_._3 == member.portName && group.direction == "plToAie")

      val outputConnections = getSystemConnectionsDescriptor().connections.collect {
        case connection if group.direction == "aieToPl" &&
          connection.srcPort.parentType == "PE" &&
          connection.srcPort.parentName == task.name &&
          connection.srcPort.portType == member.portName => connection.bitWidth
      }
      val generatedOutputWidth = if (group.direction == "aieToPl") {
        if (member.portName == "argDataOut" && task.generateArgOutWriteBuffer)
          Some(nextPow2(widthAddress + task.argumentSizeList.headOption.getOrElse(0) + 64))
        else if (member.portName == "spawnNext" && task.generateSpawnNextWriteBuffer) {
          val targetWidth = spawnNextList.getOrElse(task.name, List.empty)
            .flatMap(target => taskDescriptors.find(_.name == target).map(_.widthTask))
            .foldLeft(task.widthTask)(math.max)
          val allowWidth = if (task.variableSpawn) 0 else
            32 * (1 + spawnList.getOrElse(task.name, List.empty).count(_ != task.name))
          Some(nextPow2(widthAddress + targetWidth + 32 + allowWidth))
        } else None
      } else None
      val rwOutput = subPE.rwRequest.filter(_ => group.direction == "aieToPl")
        .filter { req =>
          member.portName == s"${req.`type`}${if (req.mode == "single") "Single" else "Stream"}${req.portWidth}Out"
        }
      val taggedCreditOutput = group.direction == "aieToPl" &&
        aiePlioSharingGroups.exists(input => input.direction == "plToAie" &&
          input.members.head.subPEName == member.subPEName &&
          input.creditReturnPort == member.portName)
      val packedOutput = group.direction == "aieToPl" &&
        (taggedCreditOutput || (group.logicalWidth > 0 && outputConnections.isEmpty &&
          generatedOutputWidth.isEmpty && rwOutput.isEmpty))
      require(isHardCilkInput || incomingReadResponses.nonEmpty || outputConnections.nonEmpty ||
        generatedOutputWidth.nonEmpty || rwOutput.nonEmpty || packedOutput,
        s"AIE PLIO sharing group '$groupLabel': '${member.subPEName}.${member.portName}' is not a generated logical endpoint")
      require(!(isHardCilkInput && incomingReadResponses.nonEmpty),
        s"AIE PLIO sharing group '$groupLabel': '${member.subPEName}.${member.portName}' resolves ambiguously")
      require(incomingReadResponses.lengthCompare(1) <= 0,
        s"AIE PLIO sharing group '$groupLabel': '${member.subPEName}.${member.portName}' has more than one PL-side read-response producer")

      val (logicalWidth, sourceKind, sourceSubPEName) =
        if (group.direction == "aieToPl") {
          require(sharingMode == AiePlioSharingMode.Single,
            s"AIE PLIO sharing group '$groupLabel': AIE-to-PL record outputs require mode 'single'")
          require(group.physicalPlioWidth == 32,
            s"AIE PLIO sharing group '$groupLabel': AIE-to-PL output PLIO width must be 32")
          val rwWidth = rwOutput.map { req =>
            val raw = if (req.`type` == "read" && req.mode == "single") 64
              else if (req.`type` == "read") 96
              else 64 + req.portWidth
            ((raw + 63) / 64) * 64
          }
          val untaggedWidth = outputConnections.maxOption.orElse(generatedOutputWidth)
            .orElse(rwWidth).orElse(if (packedOutput) Some(group.logicalWidth) else None)
          val width = (if (taggedCreditOutput) untaggedWidth.map(_ + 128)
            else untaggedWidth).get
          require(width > 0 && width % 32 == 0,
            s"AIE PLIO sharing group '$groupLabel': output width must be a positive multiple of 32")
          require(!taggedCreditOutput || untaggedWidth.nonEmpty &&
            width == untaggedWidth.get + 128,
            s"AIE PLIO sharing group '$groupLabel': tagged output must use payload width plus 128 bits")
          require(group.logicalWidth == 0 || group.logicalWidth == width,
            s"AIE PLIO sharing group '$groupLabel': explicit logicalWidth ${group.logicalWidth} does not match generated width $width")
          (width, if (packedOutput) AiePlioSourceKind.PackedOutput
            else if (rwOutput.nonEmpty) AiePlioSourceKind.RwRequest
            else AiePlioSourceKind.HardCilkOutput, member.subPEName)
        } else if (isHardCilkInput) {
          if (member.portName == "taskIn") {
            val hasInternalPredecessor = subPEList.values.exists { candidate =>
              candidate.peName == task.name &&
                candidate.rwRequest.flatMap(_.nextsubPE).contains(member.subPEName)
            }
            require(!hasInternalPredecessor,
              s"AIE PLIO sharing group '$groupLabel': '${member.subPEName}.taskIn' is internal to the AIE sub-PE chain and has no PLIO")
          }

          val width =
            if (member.portName == "taskIn") task.widthTask
            else getSystemConnectionsDescriptor().connections.collectFirst {
              case connection
                  if connection.dstPort.parentType == "PE" &&
                    connection.dstPort.parentName == task.name &&
                    connection.dstPort.portType == member.portName =>
                connection.bitWidth
            }.getOrElse(throw new IllegalArgumentException(
              s"AIE PLIO sharing group '$groupLabel': cannot derive the width of HardCilk input '${member.portName}'"
            ))

          require(sharingMode == AiePlioSharingMode.Single,
            s"AIE PLIO sharing group '$groupLabel': HardCilk input '${member.portName}' requires mode 'single'")
          (width, AiePlioSourceKind.HardCilkInput, "")
        } else {
          val (producerName, request, _) = incomingReadResponses.head
          val requiredMode =
            if (request.mode == "stream") AiePlioSharingMode.Stream
            else AiePlioSharingMode.Single

          require(sharingMode == requiredMode,
            s"AIE PLIO sharing group '$groupLabel': '${member.subPEName}.${member.portName}' requires mode '$requiredMode'")
          (request.portWidth, AiePlioSourceKind.ReadResponse, producerName)
        }

      val key = (member.subPEName, member.portName)
      val firstPeIndex = nextPeIndex(key)
      val endPeIndex = firstPeIndex + member.share
      require(endPeIndex <= task.numProcessingElements,
        s"AIE PLIO sharing group '$groupLabel': ${member.subPEName}.${member.portName} requests ${member.share} lanes starting at PE $firstPeIndex, but task '${task.name}' has only ${task.numProcessingElements} PEs")
      nextPeIndex(key) = endPeIndex

      // Match the AIE project's established endpoint naming convention while
      // retaining the canonical subPE key for descriptor and RTL lookups.
      val generatedSubPEName = member.subPEName.toLowerCase.replaceAll("[^a-z0-9_]", "")
      val peIndices = (firstPeIndex until endPeIndex).toList
      val logicalMembers = peIndices.map { index =>
        val peSuffix = if (task.numProcessingElements > 1) s"_$index" else ""
        s"${generatedSubPEName}${peSuffix}_${member.portName}"
      }
      val generatedName =
        s"${generatedSubPEName}_${member.portName}_${sharingMode}_g$groupIndex"
      val physicalGroup = AIEPlioPhysicalSharingGroupDescriptor(
        name = generatedName,
        direction = group.direction,
        mode = sharingMode,
        taskName = task.name,
        subPEName = member.subPEName,
        portName = member.portName,
        peIndices = peIndices,
        members = logicalMembers,
        physicalPortName = generatedName,
        physicalPlioWidth = group.physicalPlioWidth,
        fifoDepth = group.fifoDepth,
        logicalWidth = logicalWidth,
        sourceKind = sourceKind,
        sourceSubPEName = sourceSubPEName,
        creditCapacity = group.creditCapacity,
        initialCredits = group.initialCredits,
        creditReturnPort = group.creditReturnPort,
        creditTiming = group.creditTiming,
        aieFifoDepth = group.aieFifoDepth,
        segmentBeats = group.segmentBeats,
        lengthDriven = group.lengthDriven,
        creditTag = groupIndex
      )
      physicalGroup.validate(this)
      physicalGroup
    }
  }

  private def nextPow2(value: Int): Int = {
    var result = 1
    while (result < value) result = result << 1
    result
  }

  // --- All helper logic is kept here ---
  // Assign base addresses
  var j = 0
  val base = if (isVitisProject) 0x10 else 0x0


  taskDescriptors.foreach(task => {
    task.mgmtBaseAddresses = MemSystemDescriptor()
    val numSchedulerServers = task.getNumServers("scheduler")
    for (i <- j until j + numSchedulerServers) {
      task.mgmtBaseAddresses.schedulerServersBaseAddresses = task.mgmtBaseAddresses.schedulerServersBaseAddresses :+ ((i << 6) + base)
    }
    j += numSchedulerServers
    println("J value after scheduler: " + j)

    if(task.spawnServersCount > 0) {
      for (i <- j until j + task.spawnServersCount ) {
        task.mgmtBaseAddresses.schedulerServersBaseAddresses = task.mgmtBaseAddresses.schedulerServersBaseAddresses :+ ((i << 6) + base)
      }
      j += task.spawnServersCount
    }
    println("J value after spawner servers: " + j)


    if (task.isCont) {
      val numAllocationServers = task.getNumServers("allocator")
      for (i <- j until j + numAllocationServers) {
        task.mgmtBaseAddresses.allocationServersBaseAddresses =
          task.mgmtBaseAddresses.allocationServersBaseAddresses :+ ((i << 6) + base)
      }
      j += numAllocationServers
    }
    println("J value after allocator: " + j)


    if (task.dynamicMemAlloc) {
      val numMemoryAllocatorServers = task.getNumServers("memoryAllocator")
      for (i <- j until j + numMemoryAllocatorServers) {
        task.mgmtBaseAddresses.memoryAllocatorServersBaseAddresses =
          task.mgmtBaseAddresses.memoryAllocatorServersBaseAddresses :+ ((i << 6) + base)
      }
      j += numMemoryAllocatorServers
    }
    println("J value after memory allocator: " + j)
  })

  // For each task log base addresses
  taskDescriptors.foreach(
    task =>
      println(f"Task: ${task.name}:  task.mgmtBaseAddresses: ${task.mgmtBaseAddresses}")
    )

  def getMfpgaBaseAddress(): Int = {
    (j << 6) + base
  }

  // Helper functions
  def selfSpawnedCount(task_name: String): Int = {
    spawnList.get(task_name) match {
      case Some(spawnedTasks) =>
        if (spawnedTasks.contains(task_name))
          taskDescriptors.find(_.name == task_name).map(_.numProcessingElements).getOrElse(0)
        else 0
      case None => 0
    }
  }

  def getPortCount(port_type: String, task_name: String): Int = {
    // Get the correct map based on the port_type
    val map = port_type match {
      case "spawn"        => spawnList
      case "spawnNext"    => spawnNextList
      case "sendArgument" => sendArgumentList
      case "mallocIn"     => mallocList
      case _              => throw new IllegalArgumentException(s"Invalid port type: $port_type")
    }

    // Get the total number of processing elements that needs that type of port
    val totalProcessingElements = map
      .filter { case (_, mapped_list) => mapped_list.contains(task_name) }
      .keys
      .flatMap(taskName => taskDescriptors.find(_.name == taskName))

    var sum = 0
    totalProcessingElements.foreach { task =>
      sum += task.numProcessingElements
    }

    // if the port_type is spawn, decrement the return value by the value returned by selfSpawnCount
    val finalCount = if (port_type == "spawn") sum - selfSpawnedCount(task_name) else sum

    finalCount
  }

  def getSystemConnectionsDescriptor(): SystemConnections = {
    // mutable map of aggregators from string to int initialized to zero
    val aggregatorMapSendArg = mutable.Map[String, Int]().withDefaultValue(0)
    val aggregatorMapSpawnNext = mutable.Map[String, Int]().withDefaultValue(0)
    val aggregatorMapMalloc = mutable.Map[String, Int]().withDefaultValue(0)

    val connections = taskDescriptors.flatMap { task =>
      val spawnedTasks = spawnList.getOrElse(task.name, List())
      val argumentTasks = sendArgumentList.getOrElse(task.name, List())
      val mallocTasks = mallocList.getOrElse(task.name, List())
      val spawnNextTasks = spawnNextList.getOrElse(task.name, List())

      val taskConnections = (0 until task.numProcessingElements).map { i =>
        ConnectionDescriptor(
          PortDescriptor(f"${task.name}", "HardCilk", 0, "taskOut", i),
          PortDescriptor(task.name, "PE", i, "taskIn", 0),
          task.widthTask,
          "AXIS"
        )
      }

      val selfSpawnedConnections = (0 until selfSpawnedCount(task.name)).map { i =>
        ConnectionDescriptor(
          PortDescriptor(task.name, "PE", i, "taskOut", 0),
          PortDescriptor(f"${task.name}", "HardCilk", 0, "taskIn", i),
          task.widthTask,
          "AXIS"
        )
      }

      val spawnedConnections = spawnedTasks.filterNot(_ == task.name).zipWithIndex.flatMap { case (spawnedTask, j) =>
        val spawnedTaskDescriptor = taskDescriptors.find(_.name == spawnedTask).get
        (0 until task.numProcessingElements).map { i =>
          ConnectionDescriptor(
            PortDescriptor(task.name, "PE", i, "taskOutGlobal", j),
            PortDescriptor(f"${spawnedTask}", "HardCilk", 0, "taskInGlobal", i),
            spawnedTaskDescriptor.widthTask,
            "AXIS"
          )
        }
      }

      val argumentConnections = argumentTasks.zipWithIndex.flatMap { case (argumentTask, j) =>
        taskDescriptors.find(_.name == argumentTask).get
        (0 until task.numProcessingElements).map { i =>
          aggregatorMapSendArg(argumentTask) += 1
          ConnectionDescriptor(
            PortDescriptor(task.name, "PE", i, "argOut", j),
            PortDescriptor(f"${argumentTask}", "HardCilk", 0, "argIn", aggregatorMapSendArg(argumentTask) - 1),
            widthAddress,
            "AXIS"
          )
        }
      }

      val spawnNextConnections = spawnNextTasks.zipWithIndex.flatMap { case (spawnNextTask, j) =>
        taskDescriptors.find(_.name == spawnNextTask).get
        (0 until task.numProcessingElements).map { i =>
          aggregatorMapSpawnNext(spawnNextTask) += 1
          ConnectionDescriptor(
            PortDescriptor(f"${spawnNextTask}", "HardCilk", 0, "closureOut", aggregatorMapSpawnNext(spawnNextTask) - 1),
            PortDescriptor(task.name, "PE", i, "closureIn", 0),
            widthAddress, // This is only an address disbrutor for now...
            "AXIS"
          )
        }
      }

      val mallocConnections = mallocTasks.zipWithIndex.flatMap { case (mallocTask, j) =>
        taskDescriptors.find(_.name == mallocTask).get
        (0 until task.numProcessingElements).map { i =>
          aggregatorMapMalloc(mallocTask) += 1
          ConnectionDescriptor(
            PortDescriptor(f"${mallocTask}", "HardCilk", 0, "mallocOut", aggregatorMapMalloc(mallocTask) - 1),
            PortDescriptor(task.name, "PE", i, "mallocIn", 0),
            widthAddress, // This is only an address distrbutor for now
            "AXIS"
          )
        }
      }

      taskConnections ++ selfSpawnedConnections ++ spawnedConnections ++ argumentConnections ++ spawnNextConnections ++ mallocConnections
    }

    SystemConnections(connections)
  }

  def getNumConfigPorts(): Int = {
    taskDescriptors.map(_.getNumServers("scheduler")).sum + taskDescriptors
      .map(_.getNumServers("memoryAllocator"))
      .sum + taskDescriptors.map(_.getNumServers("allocator")).sum +
      {
        var spawner_count = 0
        taskDescriptors.foreach(task => {
          if(task.spawnServersCount > 0) {
            spawner_count += task.spawnServersCount
          }
        })
        spawner_count
      } +
      {
        if(mFPGASynth || mFPGASimulation) 1 else 0
      } +
      {
        var count_info_ports = 0
        if(mFPGASynth || mFPGASimulation){
          // Add an extra one for each task type
          count_info_ports += taskDescriptors.length

          // Add an extra one for each task with task.generateArgOutWriteBuffer set
          taskDescriptors.foreach(task => {
            if(task.generateArgOutWriteBuffer) {
              count_info_ports += 1
            }
          })

          // Add an extra one for all the arg notifiers existing in each task
          taskDescriptors.foreach(task => {
            count_info_ports += task.getNumServers("argumentNotifier")
          })
        }
        count_info_ports
      }
  }

  def getSystemAXIPortsNames(reduce_axi: Int): List[String] = {
    Seq.tabulate(reduce_axi)(i => f"m_axi_${i}%02d").toList
  }

  def getMemoryConnectionsStats(reduce_axi: Int): MemStats = {
    val interconnectDescriptors = ListBuffer[InterconnectDescriptor]()

    val totalAXIPorts = reduce_axi

    var optimizer = totalAXIPorts

    var iteration = 0
    do {
      val ratio = (optimizer / (32.0 - iteration)).ceil.toInt
      interconnectDescriptors += InterconnectDescriptor(1, ratio)
      optimizer = optimizer - ratio
      iteration += 1
    } while (optimizer > 0)
    assert(optimizer == 0)

    // In the interconnectDescriptors list aggregate the entries with the same ratios into one entry
    val interconnectDescriptorsAggregated = interconnectDescriptors
      .groupBy(_.ratio)
      .map { case (ratio, descriptors) =>
        InterconnectDescriptor(descriptors.map(_.count).sum, ratio)
      }
      .toList

    assert(interconnectDescriptorsAggregated.map(_.count).sum <= 32)

    MemStats(totalAXIPorts, interconnectDescriptorsAggregated)
  }

  private def getTaskHardCilkPortsByTask(): Map[String, (Set[String], Set[String])] = {
    val inputsByTask = mutable.Map[String, mutable.Set[String]]().withDefaultValue(mutable.Set.empty[String])
    val outputsByTask = mutable.Map[String, mutable.Set[String]]().withDefaultValue(mutable.Set.empty[String])

    getSystemConnectionsDescriptor().connections.foreach { connection =>
      val src = connection.srcPort
      val dst = connection.dstPort

      if (src.parentType == "HardCilk" && dst.parentType == "PE") {
        val set = inputsByTask.getOrElseUpdate(dst.parentName, mutable.Set.empty[String])
        set += dst.portType
      } else if (src.parentType == "PE" && dst.parentType == "HardCilk") {
        val set = outputsByTask.getOrElseUpdate(src.parentName, mutable.Set.empty[String])
        set += src.portType
      }
    }

    taskDescriptors.foreach { task =>
      val outputSet = outputsByTask.getOrElseUpdate(task.name, mutable.Set.empty[String])
      if (task.generateArgOutWriteBuffer) {
        outputSet += "argDataOut"
      }
      if (task.generateSpawnNextWriteBuffer) {
        outputSet += "spawnNext"
      }
    }

    taskDescriptors.map { task =>
      task.name -> (
        inputsByTask.getOrElse(task.name, mutable.Set.empty[String]).toSet,
        outputsByTask.getOrElse(task.name, mutable.Set.empty[String]).toSet
      )
    }.toMap
  }

  def validate(): Unit = {
    taskDescriptors.foreach(_.validate()) // Validate all sub-tasks
    val physicalSharingGroups = expandedAiePlioSharingGroups

    val physicalNames = physicalSharingGroups.map(_.effectivePhysicalPortName)
    require(physicalNames.distinct.length == physicalNames.length,
      "AIE PLIO sharing groups must have unique physical port names")
    val sharedLogicalEndpoints = physicalSharingGroups.flatMap(group =>
      group.peIndices.map(index => (group.subPEName, group.portName, index)))
    require(sharedLogicalEndpoints.distinct.length == sharedLogicalEndpoints.length,
      "A logical AIE endpoint may belong to only one AIE PLIO sharing group")

    require(isPow2(widthAddress) && widthAddress <= 64, "widthAddress must be power of 2 and <= 64")
    require(isPow2(widthContCounter) && widthContCounter <= 64, "widthContCounter must be power of 2 and <= 64")
    require(taskDescriptors.nonEmpty, "must have at least one taskDescriptor")

    val taskNames = taskDescriptors.map(_.name).toSet
    require(spawnList.keys.forall(taskNames.contains), s"spawnList contains unknown task names: ${spawnList.keys.filterNot(taskNames.contains)}")
    // ... (rest of list checks) ...
    require(spawnNextList.keys.forall(taskNames.contains), "spawnNextList contains unknown task names")
    require(sendArgumentList.keys.forall(taskNames.contains), "sendArgumentList contains unknown task names")
    require(mallocList.keys.forall(taskNames.contains), "mallocList contains unknown task names")

    val taskHardCilkPortsByTask = getTaskHardCilkPortsByTask()
    val inputOverrideOwnerByTaskPort = mutable.Map[(String, String), String]()
    val outputOverrideOwnerByTaskPort = mutable.Map[(String, String), String]()

    subPEList.foreach { case (subPEName, subPEDescriptor) =>
      require(subPEName.nonEmpty, "subPEList contains an empty subPE name")
      subPEDescriptor.validate(subPEName)
      require(taskNames.contains(subPEDescriptor.peName),
        s"subPE '$subPEName': unknown peName '${subPEDescriptor.peName}'")

      val (availableTaskInputs, availableTaskOutputs) =
        taskHardCilkPortsByTask.getOrElse(subPEDescriptor.peName, (Set.empty[String], Set.empty[String]))

      subPEDescriptor.taskInputPorts.foreach { port =>
        require(availableTaskInputs.contains(port),
          s"subPE '$subPEName': taskInputPorts contains '$port', but task '${subPEDescriptor.peName}' does not expose it")
        val key = (subPEDescriptor.peName, port)
        require(!inputOverrideOwnerByTaskPort.contains(key),
          s"Task '${subPEDescriptor.peName}': task input port '$port' is assigned by both subPE '${inputOverrideOwnerByTaskPort(key)}' and '$subPEName'")
        inputOverrideOwnerByTaskPort(key) = subPEName
      }

      subPEDescriptor.taskOutputPorts.foreach { port =>
        require(availableTaskOutputs.contains(port),
          s"subPE '$subPEName': taskOutputPorts contains '$port', but task '${subPEDescriptor.peName}' does not expose it")
        val key = (subPEDescriptor.peName, port)
        require(!outputOverrideOwnerByTaskPort.contains(key),
          s"Task '${subPEDescriptor.peName}': task output port '$port' is assigned by both subPE '${outputOverrideOwnerByTaskPort(key)}' and '$subPEName'")
        outputOverrideOwnerByTaskPort(key) = subPEName
      }

      subPEDescriptor.rwRequest.flatMap(_.nextsubPE).foreach { nextSubPE =>
        require(subPEList.contains(nextSubPE),
          s"subPE '$subPEName': nextsubPE '$nextSubPE' is not defined in subPEList")
      }
    }

    require(fpgaModel == "ALVEO_U55C", s"Unsupported fpgaModel: $fpgaModel")

    // Check if the system is supposed to support MFPGA, and has argument notification is that
    // tasks with argument notifiers must have contigous ids startting from ID zero
    if(mFPGASynth || mFPGASimulation) {
      // Create a list of the tasks with argument notifiers
      val id_list = taskDescriptors.filter(_.getNumServers("argumentNotifier") > 0).map(_.taskId)

      // Require that id_list is contigous starting with ID 0
      var decesion = true
      for(i <- 0 until id_list.length - 1) {
        if(id_list(i) + 1 != id_list(i + 1)) {
          decesion = false
        }
      }
      require(decesion,"To support mfpga the IDs of tasks with argument notifiers must be contigous and starting from zero.\n")

    }
  }
}

// ... (FullSysGenDescriptorExtended remains the same) ...
case class FullSysGenDescriptorExtended(
    fullSysGenDescriptor: FullSysGenDescriptor,
    systemConnections: SystemConnections,
    val memStats: MemStats
)
object FullSysGenDescriptorExtended {
  def fromFullSysGenDescriptor(fullSysGenDescriptor: FullSysGenDescriptor): FullSysGenDescriptorExtended = {
    val systemConnections = fullSysGenDescriptor.getSystemConnectionsDescriptor()
    val memStats = fullSysGenDescriptor.getMemoryConnectionsStats(32) // Note: 32 is hardcoded here
    FullSysGenDescriptorExtended(fullSysGenDescriptor, systemConnections, memStats)
  }
}

// --- ALL JSON OBJECTS AND HELPERS ARE MOVED TO DescriptorJSON.scala ---
