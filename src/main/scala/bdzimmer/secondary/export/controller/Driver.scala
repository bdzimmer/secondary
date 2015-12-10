// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Entry point for world builder export process.

// 2015-08-14: Configuration loaded from properties in user's home directory.
// 2015-08-22: Reworked DriverConfig. Style fixes.
// 2015-08-30: Further configuration updates.
// 2015-09-12: Per-project configs. Commands.
// 2015-09-15: Interactive mode.


package bdzimmer.secondary.export.controller

import java.awt.Desktop    // scalastyle:ignore illegal.imports
import java.net.URI
import java.io.{BufferedReader, File, InputStreamReader}

import scala.annotation.tailrec
import scala.util.Try

import bdzimmer.util.{Result, Pass, Fail, PropertiesWrapper}
import bdzimmer.util.StringUtils._

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
  private def runInteractive(): Unit = {

    println(Driver.Title)
    val br = new BufferedReader(new InputStreamReader(System.in))
    readEval()

    @tailrec
    def readEval() {
      print("> ")
      val command = br.readLine()

      command match {
        case "exit" | "quit" | "q" => () // quit
        case "" => readEval() // do nothing
        case _ => {
          runCommand(command)
          readEval()
        }
      }
    }

  }


  // run a command
  private def runCommand(command: String): Unit = command match {
    case DriverCommands.Configure => {
      val prop = ProjectConfig.getProperties(projConf.projectDir)
      new ConfigurationGUI(prop).startup(Array())
      println("You must restart Secondary for configuration changes to take effect.")
    }
    case DriverCommands.Export => projConf.mode match {
      case "drive" => {
        driveSync match {
          case Pass(ds) => ExportPipelines.exportDriveSync(projConf, ds)
          case Fail(msg) => Driver.driveError(msg)
        }
      }
      case _ => ExportPipelines.exportLocalSync(projConf)
    }
    case DriverCommands.BrowseLocal => browseLocal
    case DriverCommands.Browse => projConf.mode match {
      case "drive" => browseDrive
      case _ => browseLocal
    }
    case DriverCommands.Editor => {
      val master = WorldLoader.loadWorld(projConf) match {
        case Pass(master) => {
          val outputFilename = "assetmetadata.txt"
          AssetMetadataUtils.saveAssetMetadata(outputFilename, WorldItem.assetMetadata(master))
          new Main(projConf.mappedContentPathActual, "Secondary Editor", outputFilename)
        }
        case Fail(msg) => println(msg)
      }
    }
    case DriverCommands.Server => serverMode(Driver.ServerRefreshSeconds)
    case DriverCommands.Help => Driver.showCommands
    case _ => println("Invalid command. Use 'help' for a list of commands.")
  }


  // browse to the local copy of the project website
  private def browseLocal(): Unit = {
    val filename = List(
        projConf.projectDir,
        ProjectStructure.WebDir,
        "index.html").mkString(slash)
    val uri = new File(filename).toURI
    Try(Desktop.getDesktop.browse(uri))
  }


  // browse to the exported website on Google Drive host
  private def browseDrive(): Unit = {
    driveSync match {
      case Pass(ds) => Try {
        val drivePermaLink = "http://www.googledrive.com/host/" + ds.driveOutputFile.getId
        println(drivePermaLink)
        Desktop.getDesktop.browse(new URI(drivePermaLink))
      }
      case Fail(msg) => Driver.driveError(msg)
    }
  }

  private def serverMode(seconds: Int): Unit = {
    while(true) {
      runCommand(DriverCommands.Export)
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

  val Configure = "configure"
  val Export = "export"
  val BrowseLocal = "browse-local"
  val Browse = "browse"
  val Editor = "editor"
  val Server = "server"
  val Interactive = "interactive"
  val Help = "help"

  val CommandsDescriptions = List(
      (Configure, "edit project configuration"),
      (Export, "export"),
      (BrowseLocal, "browse local copy of project web site"),
      (Browse, "browse exported project web site"),
      (Editor, "start editor (alpha)"),
      (Server, "server mode"),
      (Help, "show usage / commands"))
}
