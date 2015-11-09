// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Entry point for world builder export process.

// 2015-08-14: Configuration loaded from properties in user's home directory.
// 2015-08-22: Reworked DriverConfig. Style fixes.
// 2015-08-30: Further configuration updates.
// 2015-09-12: Per-project configs. Commands.
// 2015-09-15: Interactive mode.

// scalastyle:off regex

package bdzimmer.secondary.export.controller

import java.awt.Desktop    // scalastyle:ignore illegal.imports
import java.net.URI
import java.io.{BufferedReader, File, InputStreamReader}

import scala.util.{Try, Success, Failure}

import bdzimmer.gdrivescala.{DriveUtils, DriveBuilder, GoogleDriveKeys}
import bdzimmer.secondary.export.model.{ProjectConfig, ProjectStructure, WorldItem}
import bdzimmer.secondary.export.view.ConfigurationGUI
import bdzimmer.secondary.editor.view.Main
import bdzimmer.secondary.editor.model.AssetMetadataUtils



class Driver {

  // project directory is current working directory
  val projectDir = System.getProperty("user.dir")
  val projConf = ProjectConfig(projectDir)
  val driveSync = DriveSync(projConf)

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
        case Right(master) => {
          val outputFilename = "assetmetadata.txt"
          AssetMetadataUtils.saveAssetMetadata(outputFilename, WorldItem.assetMetadata(master))
          new Main(projConf.mappedContentPathActual, "Secondary Editor", outputFilename)
        }
        case Left(msg) => println(msg)
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
        val drivePermaLink = "http://www.googledrive.com/host/" + ds.driveOutputFile.getId
        println(drivePermaLink)
        Desktop.getDesktop.browse(
            new URI(drivePermaLink))
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

  val Title = "Secondary - create worlds from text - v2015.11.08"
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
