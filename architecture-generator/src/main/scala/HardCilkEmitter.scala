package HardCilk

import _root_.circt.stage.ChiselStage
import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalTime}
import java.nio.file.{Files, Paths}
import Util.ArgParser
import Descriptors._
import Descriptors.DescriptorJSON._
import Util.HardCilkEmitterUtil._
import SoftwareUtil._
import SoftwareUtil.aie._

object HardCilkEmitter extends App {
  ArgParser.parseArgs(args) match {
    case None =>
      // parser printed usage; exit quietly
    case Some(cfg) =>
      val jsonName = basename(cfg.json_path)
      val dateFmt = DateTimeFormatter.ofPattern("yyyy-MM-dd")
      val timeFmt = DateTimeFormatter.ofPattern("HH-mm-ss")
      val outputDirName =
          if (cfg.timestamped)
            s"${jsonName}_${LocalDate.now.format(dateFmt)}_${LocalTime.now.format(timeFmt)}"
          else
            s"${jsonName}_hardcilk_output"

      val systemDescriptor = parseJsonFile[FullSysGenDescriptor](cfg.json_path)

      // The original HardCilk flow validates before RTL elaboration.  Sharing
      // descriptors also affect the AIE graph, so apply that same validation
      // before emitting a packet-switched AIE project.  Descriptors without
      // sharing groups retain the original AIECilk behavior.
      if (cfg.project_aie_generation && systemDescriptor.aiePlioSharingGroups.nonEmpty) {
        try {
          systemDescriptor.validate()
        } catch {
          case e: IllegalArgumentException =>
            System.err.println(s"AIE project generation failed: ${e.getMessage}")
            System.exit(1)
        }
      }


      if (!cfg.rtl_generation) {
        println("RTL generation not requested.")
      } else {
        val outputDirPathRTL = s"${cfg.output_dir}/$outputDirName/rtl"
        Files.createDirectories(Paths.get(outputDirPathRTL))

        try {
          systemDescriptor.validate() // <-- EXPLICITLY VALIDATE HERE

          if (systemDescriptor.aiePlioSharingGroups.nonEmpty) {
            val automaticReport = Paths.get(
              cfg.output_dir,
              outputDirName,
              "software",
              s"aie_${jsonName}",
              "Work",
              "reports",
              "packet_switching_report.json"
            )
            val reportPath =
              if (cfg.aie_packet_report.nonEmpty) Paths.get(cfg.aie_packet_report)
              else automaticReport

            require(Files.isRegularFile(reportPath),
              s"AIE packet switching report not found: $reportPath. " +
              "Generate the AIE project with -k, compile it with aiecompiler/v++, " +
              "then rerun RTL generation with -g. Use --aie-packet-report when the report is outside the generated AIE project.")

            val resolvedGroups = AIEPacketIdResolver.resolve(systemDescriptor, reportPath)
            resolvedGroups.foreach { group =>
              println(s"Resolved AIE packet IDs for ${group.name}: ${group.packetIds.mkString(", ")}")
            }
            AIEPacketIdResolver.writeManifest(
              resolvedGroups,
              reportPath,
              Paths.get(outputDirPathRTL, "aie_packet_ids.json")
            )
          }

          // Call the generate RTL function
          val numHbmPortExports = generateRTL(
            systemDescriptor = systemDescriptor,
            pathInputJsonFile = cfg.json_path,
            outputDirPathRTL = outputDirPathRTL,
            flags = cfg,
            isSimulation = false
          )
          println(s"Emitted RTL to: $outputDirPathRTL")
        } catch {
          case e: IllegalArgumentException =>
            System.err.println(s"RTL generation failed: ${e.getMessage}")
            System.exit(1)
        }
      }

      if (cfg.project_sc_generation) {
        // Using java.nio copy a folder with all its content (files and subfolders) to another folder, source is "pwd/software_template" and destination is "outputDirPathSC"
        val source = new java.io.File("software_template")
        val outputDirPathSC = s"${cfg.output_dir}/$outputDirName/software"
        val destination = new java.io.File(outputDirPathSC)
        java.nio.file.Files
          .walk(source.toPath)
          .forEach(sourcePath => {
            val destinationPath =
              destination.toPath.resolve(source.toPath.relativize(sourcePath))
            if (sourcePath.toFile.isDirectory) {
              java.nio.file.Files.createDirectories(destinationPath)
            } else {
              java.nio.file.Files.copy(
                sourcePath,
                destinationPath,
                java.nio.file.StandardCopyOption.REPLACE_EXISTING
              )
            }
          })

        // Rename `outputDirPathSC/projects/project_template` to `outputDirPathSC/projects/${jsonName}`
        val projectTemplate = new java.io.File(s"$outputDirPathSC/projects/project_template")
        val projectDestination =  new java.io.File(s"$outputDirPathSC/projects/${jsonName}")
        projectTemplate.renameTo(projectDestination)

        // Generate the HDL in the `outputDirPathSC/projects/${jsonName}/hdl`
        new java.io.File(s"$outputDirPathSC/projects/${jsonName}/hdl").mkdirs()
        val numHbmPortExports = generateRTL(
          systemDescriptor,
          cfg.json_path,
          s"$outputDirPathSC/projects/${jsonName}/hdl",
          cfg,
          true
        )

        // Generate the SystemC project in the `outputDirPathSC/project/${jsonName}/include`
        new java.io.File(s"$outputDirPathSC/projects/${jsonName}/include")
          .mkdirs()
        CppHeaderTemplate.generateCppHeader(
          systemDescriptor,
          s"$outputDirPathSC/projects/${jsonName}/include",
          numHbmPortExports
        )

        // Generate the SystemC testbench in the `outputDirPathSC/projects/${jsonName}/include`
        TestBenchHeaderTemplate.generateCppHeader(
          systemDescriptor,
          s"$outputDirPathSC/projects/${jsonName}/include",
          numHbmPortExports
        )

        // Read the `outputDirPathSC/projects/${jsonName}/CMakeLists.txt` and replace the `${project_template}` with the `${jsonName}`
        val cmakeListsPath =
          s"$outputDirPathSC/projects/${jsonName}/CMakeLists.txt"
        val cmakeListsContent = readFile(cmakeListsPath)
        val newCmakeListsContent =
          cmakeListsContent.replace("${project_template}", jsonName)
        writeFile(cmakeListsPath, newCmakeListsContent)

        // Also copy `../software/${jsonName}` to `outputDirPathSC/projects/${jsonName}`
        var source_project_path = s"../software/${jsonName}"
        if(systemDescriptor.mFPGASimulation || systemDescriptor.mFPGASynth){
          source_project_path = s"../software/mfpga/${jsonName}"
        }

        val sourceProject = new java.io.File(source_project_path)

        java.nio.file.Files
          .walk(sourceProject.toPath)
          .forEach(sourcePath => {
            val destinationPath = projectDestination.toPath
              .resolve(sourceProject.toPath.relativize(sourcePath))
            if (sourcePath.toFile.isDirectory) {
              java.nio.file.Files.createDirectories(destinationPath)
            } else {
              java.nio.file.Files.copy(sourcePath, destinationPath, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
            }
          })

      }
      if (cfg.project_aie_generation) {
        val outputDirPathAIE = s"${cfg.output_dir}/$outputDirName/software"
        println(s"Generating AIE project in: $outputDirPathAIE")
        // AIE relevant templates
        new java.io.File(s"$outputDirPathAIE/aie_${jsonName}/").mkdirs()
        KernelXmlTemplate.generateHelperKernelXmls(
          systemDescriptor,
          s"$outputDirPathAIE/aie_${jsonName}"
        )
        ConnectivityTemplate.generateConnectivityCfg(
          systemDescriptor,
          s"$outputDirPathAIE/aie_${jsonName}"
        )
        new java.io.File(s"$outputDirPathAIE/aie_${jsonName}/src/aie").mkdirs()
        ProjectHeaderTemplate.generateProjectHeader(
          systemDescriptor,
          s"$outputDirPathAIE/aie_${jsonName}/src/aie"
        )
        new java.io.File(s"$outputDirPathAIE/aie_${jsonName}/src/host/projects/pageRank/include").mkdirs()
        CppHeaderTemplate.generateCppHeader(
          systemDescriptor,
          s"$outputDirPathAIE/aie_${jsonName}/src/host/projects/pageRank/include",
          1
        )
      } else {
        println("AIE project generation not requested.")
      }

  }
}
