package SoftwareUtil.aie

import Descriptors._
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

object AIEPacketIdResolver {
  private val BranchIndex = raw".*\.out\[(\d+)\]$$".r
  private val MergeInputIndex = raw".*\.in\[(\d+)\]$$".r

  case class ResolvedGroup(name: String, packetIds: Vector[Int])

  def resolve(
      descriptor: FullSysGenDescriptor,
      reportPath: Path
  ): Seq[ResolvedGroup] = {
    require(Files.isRegularFile(reportPath),
      s"AIE packet switching report not found: $reportPath")

    val reportText = Files.readString(reportPath, StandardCharsets.UTF_8)
    val report = parse(reportText).fold(
      error => throw new IllegalArgumentException(
        s"Cannot parse AIE packet switching report '$reportPath': ${error.message}"
      ),
      identity
    )
    val internal = report.hcursor.downField("Internal").focus
      .flatMap(_.asArray)
      .getOrElse(throw new IllegalArgumentException(
        s"AIE packet switching report '$reportPath' has no Internal array"
      ))

    descriptor.expandedAiePlioSharingGroups.map { group =>
      val switchName = if (group.direction == "plToAie") s"${group.name}_split"
        else s"${group.name}_merge"
      val branches = internal.flatMap(entry =>
        if (group.direction == "plToAie") parseBranch(entry, switchName)
        else parseMergeInput(entry, switchName))
      val duplicateIndices = branches.groupBy(_._1).collect {
        case (index, entries) if entries.lengthCompare(1) > 0 => index
      }
      require(duplicateIndices.isEmpty,
        s"AIE packet switching report has duplicate outputs for '$switchName': ${duplicateIndices.toSeq.sorted.mkString(", ")}")

      val byIndex = branches.toMap
      val expectedIndices = group.peIndices.indices
      val missing = expectedIndices.filterNot(byIndex.contains)
      require(missing.isEmpty,
        s"AIE packet switching report has no packet ID for ${missing.map(i => s"$switchName.${if (group.direction == "plToAie") "out" else "in"}[$i]").mkString(", ")}")

      val unexpected = byIndex.keySet.filterNot(expectedIndices.contains)
      require(unexpected.isEmpty,
        s"AIE packet switching report contains unexpected outputs for '$switchName': ${unexpected.toSeq.sorted.mkString(", ")}")

      val ids = expectedIndices.map(byIndex).toVector
      group.resolvePacketIds(ids)
      ResolvedGroup(group.name, ids)
    }
  }

  def writeManifest(
      resolvedGroups: Seq[ResolvedGroup],
      reportPath: Path,
      outputPath: Path
  ): Unit = {
    val groupsJson = resolvedGroups.map { group =>
      Json.obj(
        "name" -> group.name.asJson,
        "packetIds" -> group.packetIds.asJson
      )
    }
    val manifest = Json.obj(
      "sourceReport" -> reportPath.toAbsolutePath.normalize.toString.asJson,
      "groups" -> groupsJson.asJson
    )
    Files.writeString(
      outputPath,
      manifest.spaces2 + System.lineSeparator(),
      StandardCharsets.UTF_8
    )
  }

  private def parseMergeInput(entry: Json, expectedMerge: String): Option[(Int, Int)] = {
    val input = entry.hcursor.downField("Input")
    val merge = input.get[String]("Merge").toOption
    val portName = input.get[String]("PortName").toOption
    val belongs = merge.exists(name => name == expectedMerge || name.endsWith(s".$expectedMerge")) ||
      portName.exists(name => name.startsWith(s"$expectedMerge.in[") ||
        name.contains(s".$expectedMerge.in["))
    if (!belongs) None
    else {
      val index = portName.flatMap {
        case MergeInputIndex(value) => Some(value.toInt)
        case _ => None
      }.getOrElse(throw new IllegalArgumentException(
        s"AIE compiler input for '$expectedMerge' has no recognizable merge input PortName"))
      val ids = entry.hcursor.get[Vector[Int]]("PacketID").toOption.getOrElse(
        throw new IllegalArgumentException(s"AIE compiler input '$expectedMerge.in[$index]' has no PacketID array"))
      require(ids.length == 1,
        s"AIE compiler input '$expectedMerge.in[$index]' must have one packet ID, found ${ids.mkString(",")}")
      Some(index -> ids.head)
    }
  }

  private def parseBranch(entry: Json, expectedSplit: String): Option[(Int, Int)] = {
    val output = entry.hcursor.downField("Output")
    val split = output.get[String]("Split").toOption
    val portName = output.get[String]("PortName").toOption
    val packetIds = entry.hcursor.get[Vector[Int]]("PacketID").toOption

    if (!split.exists(name => name == expectedSplit || name.endsWith(s".$expectedSplit"))) {
      None
    } else {
      val index = portName.flatMap {
        case BranchIndex(value) => Some(value.toInt)
        case _ => None
      }.getOrElse(throw new IllegalArgumentException(
        s"AIE compiler output for '$expectedSplit' has no recognizable split output PortName"
      ))
      val ids = packetIds.getOrElse(throw new IllegalArgumentException(
        s"AIE compiler output '$expectedSplit.out[$index]' has no PacketID array"
      ))
      require(ids.length == 1,
        s"AIE compiler output '$expectedSplit.out[$index]' must have exactly one packet ID, found ${ids.mkString(",")}")
      Some(index -> ids.head)
    }
  }
}
