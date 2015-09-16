// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Entry point for world builder export process.

// 2015-08-14: Configuration loaded from properties in user's home directory.
// 2015-08-22: Reworked DriverConfig. Style fixes.
// 2015-08-30: Further configuration updates.
// 2015-09-12: Per-project configs. Commands.
// 2015-09-15: Interactive mode.

// scalastyle:off regex

package bdzimmer.secondary.export

import java.awt.Desktop
import java.net.URI
import java.io.{BufferedReader, File, InputStreamReader}

import scala.collection.JavaConverters._
import scala.util.Try

import com.google.api.client.util.DateTime
import com.google.api.services.drive.Drive
import com.google.api.services.drive.model.{File => DriveFile}
import org.apache.commons.io.{FileUtils, FilenameUtils}

import bdzimmer.gdrivescala.{DriveUtils, DriveBuilder, GoogleDriveKeys}


object Driver {

  val Title = "Secondary - create worlds from text - v2015.09.15"

  def main(args: Array[String]): Unit = {

    val defaultCommand = DriverCommands.Interactive

    // project directory is current working directory
    val projectDir = System.getProperty("user.dir")
    val projConf = new ProjectConfig(projectDir)

    val command = args.headOption.getOrElse(defaultCommand)

    command match {
      case DriverCommands.Interactive => runInteractive(projectDir, projConf)
      case DriverCommands.Help => showUsage
      case _ => runCommand(command, projectDir, projConf)
    }

  }


  def runInteractive(projectDir: String, projConf: ProjectConfig): Unit = {
    println(Title)
    val br = new BufferedReader(new InputStreamReader(System.in))
    while (true) {
      print("> ")
      val command = br.readLine()

      command match {
        case "" => () // do nothing
        case "exit" | "quit" | "q" => System.exit(0)
        case _ => runCommand(command, projectDir, projConf)
      }
    }
  }


  def runCommand(command: String, projectDir: String, projConf: ProjectConfig): Unit = command match {
    case DriverCommands.Configure => new ConfigurationGUI(projConf).startup(Array())
    case DriverCommands.ExportLocalAll => {
      ContentTransformer.exportLocalAll(projConf)
      ContentTransformer.addStyles(projConf)
    }
    case DriverCommands.ExportSync => ContentTransformer.exportSync(projConf)
    case DriverCommands.Browse => {
      val filename = List(projectDir, ProjectStructure.WebDir, "index.html").mkString(File.separator)
      browseLocal(filename)
    }
    case DriverCommands.BrowseDrive => browseRemote(projConf)
    case DriverCommands.Help => showCommands
    case _ => println("Invalid command. Use 'help' for a list of commands.")
  }


  // open a local file in the default web browser
  def browseLocal(filename: String): Try[Unit] = Try({
    val uri = new File(filename).toURI
    println(uri.getPath)
    Desktop.getDesktop.browse(uri)
  })


  // browse to the project on Google Drive.
  def browseRemote(conf: ProjectConfig): Try[Unit] = Try({
    val drive = ContentTransformer.createDrive(conf)
    val driveOutputFile = DriveUtils.getFileByPath(
        drive,
        DriveUtils.getRoot(drive),
        conf.driveOutputPathList).get
    Desktop.getDesktop.browse(new URI("http://www.googledrive.com/host/" + driveOutputFile.getId))
  })


  // show help at the command line
  def showUsage(): Unit = {
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
  def showCommands(): Unit = {
    println()
    val maxLength = DriverCommands.CommandsDescriptions.map(_._1.length).max
    DriverCommands.CommandsDescriptions.foreach(x => {
      val command = x._1
      val description = x._2
      println("  " + command + " " * (maxLength - command.length + 2) + description)
    })
    println()
  }

}



object DriverCommands {

  val Configure = "config"
  val ExportLocalAll = "export-local-all"
  val ExportSync = "export-sync"
  val Browse = "browse"
  val BrowseDrive = "browse-drive"
  val Interactive = "interactive"
  val Help = "help"

  val CommandsDescriptions = List(
      (Configure, "edit project configuration"),
      (ExportLocalAll, "content to web - all"),
      (ExportSync, "Drive to content, content to web, web to Drive"),
      (Browse, "browse local project web site"),
      (BrowseDrive, "browse Drive project web site"),
      (Interactive, "start interactive mode"),
      (Help, "show usage"))
}
