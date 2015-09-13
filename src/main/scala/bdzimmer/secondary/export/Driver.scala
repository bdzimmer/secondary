// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Entry point for world builder export process.

// 2015-08-14: Configuration loaded from properties in user's home directory.
// 2015-08-22: Reworked DriverConfig. Style fixes.
// 2015-08-30: Further configuration updates.
// 2015-09-12: Per-project configs. Commands.

package bdzimmer.secondary.export

import java.awt.Desktop
import java.net.URI
import java.io.File

import scala.collection.JavaConverters._
import scala.util.Try

import com.google.api.client.util.DateTime
import com.google.api.services.drive.Drive
import com.google.api.services.drive.model.{File => DriveFile}
import org.apache.commons.io.{FileUtils, FilenameUtils}


import bdzimmer.gdrivescala.{DriveUtils, DriveBuilder, GoogleDriveKeys}


object Driver {

  def main(args: Array[String]): Unit = {

    val defaultCommand = DriverCommands.ExportSync

    // project directory is current working directory
    val projectDir = System.getProperty("user.dir")
    val projConf = new DriverConfig(projectDir)

    val command = args.headOption.getOrElse(defaultCommand)

    command match {
      case DriverCommands.Configure => new ConfigurationGUI(projConf).startup(Array())
      case DriverCommands.ExportLocal => ContentTransformer.exportLocal(projConf)
      case DriverCommands.ExportSync => ContentTransformer.exportSync(projConf)
      case DriverCommands.Browse => {
        val filename = List(projectDir, ProjectStructure.WebDir, "index.html").mkString(File.separator)
        browseLocal(filename)
      }
      case DriverCommands.BrowseDrive => browseRemote(projConf)
      case DriverCommands.Help => showHelp
      case _ => println("Invalid command. Run with 'help' for usage.")
    }

  }


  // open a local file in the default web browser
  def browseLocal(filename: String): Try[Unit] = Try({
    val uri = new File(filename).toURI
    println(uri.getPath)
    Desktop.getDesktop.browse(uri)
  })


  // browse to the project on Google Drive.
  def browseRemote(conf: DriverConfig): Try[Unit] = Try({
    val drive = ContentTransformer.createDrive(conf)
    val driveOutputFile = DriveUtils.getFileByPath(
        drive,
        DriveUtils.getRoot(drive),
        conf.driveOutputPathList).get
    Desktop.getDesktop.browse(new URI("http://www.googledrive.com/host/" + driveOutputFile.getId))
  })


  // show help at the command line
  def showHelp(): Unit = {
    println("Secondary - create worlds from text")
    println
    println("Usage:")
    println("  secondary <command>")
    println
    println("Commands:")
    val maxLength = DriverCommands.CommandsDescriptions.map(_._1.length).max
    DriverCommands.CommandsDescriptions.foreach(x => {
      val command = x._1
      val description = x._2
      println("  " + command + " " * (maxLength - command.length + 2) + description)
    })
  }

}



object DriverCommands {

  val Configure = "config"
  val ExportLocal = "export-local"
  val ExportSync = "export-sync"
  val Browse = "browse"
  val BrowseDrive = "browse-drive"
  val Help = "help"

  val CommandsDescriptions = List(
      (Configure, "edit project configuration"),
      (ExportLocal, "content to web"),
      (ExportSync, "Drive to cache, cache to web, web to Drive"),
      (Browse, "browse local project web site"),
      (BrowseDrive, "browse Drive project web site"),
      (Help, "show usage"))
}
