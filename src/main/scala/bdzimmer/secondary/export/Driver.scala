// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Entry point for world builder export process.

// 2015-08-14: Configuration loaded from properties in user's home directory.
// 2015-08-22: Reworked DriverConfig. Style fixes.
// 2015-08-30: Further configuration updates.
// 2015-09-12: Per-project configs. Commands.
// 2015-09-15: Interactive mode.

// scalastyle:off regex

package bdzimmer.secondary.export

import java.awt.Desktop    // scalastyle:ignore illegal.imports
import java.net.URI
import java.io.{BufferedReader, File, InputStreamReader}

import scala.util.{Try, Success, Failure}

import bdzimmer.gdrivescala.{DriveUtils, DriveBuilder, GoogleDriveKeys}
import bdzimmer.secondary.view.Main


class Driver {

  // project directory is current working directory
  val projectDir = System.getProperty("user.dir")
  val projConf = ProjectConfig(projectDir)
  val driveSync = Driver.createDriveSync(projConf)

  def run(args: Array[String]): Unit = {
    val command = args.headOption.getOrElse(Driver.DefaultCommand)
    command match {
      case DriverCommands.Interactive => runInteractive
      case DriverCommands.Help => Driver.showUsage
      case _ => runCommand(command)
    }
  }


  // start the interactive shell
  def runInteractive(): Unit = {
    println(Driver.Title)
    val br = new BufferedReader(new InputStreamReader(System.in))
    while (true) {
      print("> ")
      val command = br.readLine()

      command match {
        case "" => () // do nothing
        case "exit" | "quit" | "q" => sys.exit(0)
        case _ => runCommand(command)
      }
    }
  }


  // run a command
  def runCommand(command: String): Unit = command match {
    case DriverCommands.Configure => {
      val prop = ProjectConfig.getProperties(projConf.projectDir)
      new ConfigurationGUI(prop).startup(Array())
      println("You must restart Secondary for configuration changes to take effect.")
    }
    case DriverCommands.ExportLocalAll => {
      ExportPipelines.exportLocalAll(projConf)
      ExportPipelines.addStyles(projConf)
    }
    case DriverCommands.ExportLocalSync => ExportPipelines.exportLocalSync(projConf)
    case DriverCommands.ExportDriveSync => driveSync match {
      case Right(ds) => ExportPipelines.exportDriveSync(projConf, ds)
      case Left(msg) => Driver.driveError(msg)
    }
    case DriverCommands.Browse => browseLocal
    case DriverCommands.BrowseDrive => browseRemote
    case DriverCommands.Editor => {
      val master = WorldLoader.loadWorld(projConf) match {
        case Success(x) => new Main(projConf.mappedContentPathActual, "Secondary Editor", x)
        case Failure(e) => println("Invalid YAML in master.")
      }
    }
    case DriverCommands.Server => serverMode(Driver.ServerRefreshSeconds)
    case DriverCommands.Help => Driver.showCommands
    case _ => println("Invalid command. Use 'help' for a list of commands.")
  }


  // open a local file in the default web browser
  def browseLocal(): Unit = {
    val filename = List(
        projConf.projectDir,
        ProjectStructure.WebDir,
        "index.html").mkString(File.separator)
    val uri = new File(filename).toURI
    Try(Desktop.getDesktop.browse(uri))
  }


  // browse to the project on Google Drive.
  def browseRemote(): Unit = {
    driveSync match {
      case Right(ds) => Try {
        Desktop.getDesktop.browse(
            new URI("http://www.googledrive.com/host/" + ds.driveOutputFile.getId))
      }
      case Left(msg) => Driver.driveError(msg)
    }
  }

  def serverMode(seconds: Int): Unit = {
    while(true) {
      runCommand(DriverCommands.ExportDriveSync)
      Thread.sleep(seconds * 1000)
    }
  }

}



object Driver {

  val Title = "Secondary - create worlds from text - v2015.10.20"
  val DefaultCommand = DriverCommands.Interactive
  val ServerRefreshSeconds = 60

  def main(args: Array[String]): Unit = {
    val driver = new Driver()
    driver.run(args)
  }


   // show help at the command line
  private def showUsage(): Unit = {
    println(Title)
    println()
    println("Usage:")
    println("  secondary <command>")
    println()
    println("Commands:")
    showCommands
    println("If no command is provided, Secondary will start in interactive mode.")
  }


  // print the list of commands / descriptions
  private def showCommands(): Unit = {
    println()
    val maxLength = DriverCommands.CommandsDescriptions.map(_._1.length).max
    DriverCommands.CommandsDescriptions.foreach(x => {
      val command = x._1
      val description = x._2
      println("  " + command + " " * (maxLength - command.length + 2) + description)
    })
    println()
  }


  private def driveError(msg: String): Unit = {
    println("Drive problem: " + msg)
  }

  // safely create a DriveSync object from the project configuration
  // None if the input or output directories don't exist
  def createDriveSync(projConf: ProjectConfig): Either[String, DriveSync] = {
    val drive = DriveSync.createDrive(projConf)
    val driveRootFile = DriveUtils.getRoot(drive)

    for {
      driveInputFile <- DriveUtils.getFileByPath(
          drive, driveRootFile, projConf.driveInputPathList).toRight("input path does not exist").right
      driveOutputFile <- DriveUtils.getFileByPath(
          drive, driveRootFile, projConf.driveOutputPathList).toRight("output path does not exist").right
    } yield (new DriveSync(projConf, drive, driveInputFile, driveOutputFile))

  }

}



object DriverCommands {

  val Configure = "config"
  val ExportLocalAll = "export-local-all"
  val ExportLocalSync = "export-local"
  val ExportDriveSync = "export"
  val Browse = "browse-local"
  val BrowseDrive = "browse"
  val Editor = "editor"
  val Server = "server"
  val Interactive = "interactive"
  val Help = "help"

  val CommandsDescriptions = List(
      (Configure, "edit project configuration"),
      (ExportLocalAll, "content to web - all"),
      (ExportLocalSync, "content to web"),
      (ExportDriveSync, "Drive to content, content to web, web to Drive"),
      (Browse, "browse local project web site"),
      (BrowseDrive, "browse Drive project web site"),
      (Editor, "start editor (alpha)"),
      (Server, "server mode"),
      (Help, "show usage / commands"))
}
