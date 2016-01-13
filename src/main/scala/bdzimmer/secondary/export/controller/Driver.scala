// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Entry point for world builder export process.

// 2015-08-14: Configuration loaded from properties in user's home directory.
// 2015-08-22: Reworked DriverConfig. Style fixes.
// 2015-08-30: Further configuration updates.
// 2015-09-12: Per-project configs. Commands.
// 2015-09-15: Interactive mode.
// 2016-01-12: Edit item command.


package bdzimmer.secondary.export.controller

import java.awt.Desktop    // scalastyle:ignore illegal.imports
import java.net.URI
import java.io.{BufferedReader, File, InputStreamReader}

import scala.annotation.tailrec
import scala.util.Try

import bdzimmer.util.{Result, Pass, Fail, PropertiesWrapper}
import bdzimmer.util.StringUtils._

import bdzimmer.gdrivescala.{DriveUtils, DriveBuilder, GoogleDriveKeys}
import bdzimmer.secondary.export.model.{CollectionItem, ProjectConfig, ProjectStructure, WorldItem}
import bdzimmer.secondary.export.view.ConfigurationGUI
import bdzimmer.pixeleditor.view.Main
import bdzimmer.pixeleditor.model.AssetMetadataUtils



class Driver {

  // project directory is current working directory
  val projectDir = System.getProperty("user.dir")
  val projConf = ProjectConfig(projectDir)
  val driveSync = DriveSync(projConf)

  var loadedWorld: Result[String, CollectionItem] = Fail("World not loaded!")

  def run(args: Array[String]): Unit = {
    val command = args.headOption.getOrElse(Driver.DefaultCommand)
    command match {
      case DriverCommands.Interactive => runInteractive
      case DriverCommands.Help => Driver.showUsage
      case _ => runCommand(command, List())
    }
  }


  // start the interactive shell
  private def runInteractive(): Unit = {

    // TODO: attempt an initial loading of the world

    println(Driver.Title)
    val br = new BufferedReader(new InputStreamReader(System.in))
    readEval()

    @tailrec
    def readEval() {
      print("> ")
      val (command, args) = br.readLine().split("\\s+").toList match {
        case x :: xs => (x, xs)
        case Nil => ("", List())
      }

      command match {
        case "exit" | "quit" | "q" => () // quit
        case "" => readEval() // do nothing
        case _ => {
          runCommand(command, args)
          readEval()
        }
      }
    }

  }


  // run a command
  private def runCommand(command: String, args: List[String]): Unit = command match {
    case DriverCommands.Configure => {
      val prop = ProjectConfig.getProperties(projConf.projectDir)
      new ConfigurationGUI(prop).startup(Array())
      println("You must restart Secondary for configuration changes to take effect.")
    }
    case DriverCommands.Edit => {
      val name = args.mkString(" ")
      editItemByName(name)
    }
    case DriverCommands.Export => {
      val result = projConf.mode match {
        case "drive" => {
          driveSync match {
            case Pass(ds) => ExportPipelines.exportDriveSync(projConf, ds)
            case Fail(msg) => {
              Driver.driveError(msg)
              Fail(msg)
            }
          }
        }
        case _ => ExportPipelines.exportLocalSync(projConf)
      }
      loadedWorld = result.mapLeft(_ => "Failed to load world during export.")
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
    val filename = projConf.projectDir / ProjectStructure.WebDir / "index.html"
    val uri = new File(filename).toURI
    Try(Desktop.getDesktop.browse(uri))
  }

  // browse to the exported website on Google Drive host
  private def browseDrive(): Unit = driveSync match {
    case Pass(ds) => {
      val drivePermaLink = "http://www.googledrive.com/host/" + ds.driveOutputFile.getId
      println(drivePermaLink)
      Try(Desktop.getDesktop.browse(new URI(drivePermaLink)))
    }
    case Fail(msg) => Driver.driveError(msg)
  }

  // edit the source file associated with an object by name or id
  private def editItemByName(name: String): Unit = loadedWorld match {
    case Pass(master) => {
      val world = WorldItem.collectionToList(master)
      world.filter(item => item.id.equals(name) || item.name.equals(name)).headOption match {
        case Some(item) => editItem(item)
        case None => println("No such item!")
      }
    }
    case Fail(msg) => println(msg)
  }

  // edit an item's source file locally or in Drive
  private def editItem(item: WorldItem): Unit = projConf.mode match {
    case "local" =>
      Desktop.getDesktop.open(new File(projConf.localContentPath / item.srcyml))
    case "drive" => Desktop.getDesktop.browse(new URI(ExportPages.notepadURL(item)))
  }

  private def serverMode(seconds: Int): Unit = {
    while(true) {
      runCommand(DriverCommands.Export, List())
      Thread.sleep(seconds * 1000)
    }
  }

}



object Driver {

  val Title = "Secondary - create worlds from text - v2015.12.14"
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
  val Edit = "edit"
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
      (Edit, "edit the source file for an item"),
      (BrowseLocal, "browse local copy of project web site"),
      (Browse, "browse exported project web site"),
      (Editor, "start editor (alpha)"),
      (Server, "server mode"),
      (Help, "show usage / commands"))
}
