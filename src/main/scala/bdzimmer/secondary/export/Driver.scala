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
import org.apache.commons.io.FileUtils

import bdzimmer.gdrivescala.{DriveUtils, DriveBuilder, GoogleDriveKeys}

object Driver {


  def main(args: Array[String]): Unit = {

    // project directory is current working directory
    val projectDir = System.getProperty("user.dir")
    val projectConfig = new DriverConfig(projectDir)

    // if run with no args, set a default command
    val defaultCommand = DriverCommands.Configure
    val command = args.headOption.getOrElse(defaultCommand)

    command match {
      case DriverCommands.Configure => new ConfigurationGUI(projectConfig).startup(Array())
      case DriverCommands.All => runAll(projectConfig)
      case DriverCommands.Browse => {
        val filename = List(projectDir, ProjectStructure.WebDir, "index.html").mkString(File.separator)
        browseLocal(filename)
      }
      case DriverCommands.BrowseDrive => ???
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


  // show help at the command line
  def showHelp(): Unit = {
    println("Secondary - build worlds from text")
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




  def runAll(projectConfig: DriverConfig) = {

    println("driver configuration loaded")

    val keys = GoogleDriveKeys(
      id = DriveBuilder.getClientIdFromJsonFile(
          new java.io.File(projectConfig.driveClientIdFile)),
      token = DriveBuilder.getAccessTokenFromJsonFile(
          new java.io.File(projectConfig.driveAccessTokenFile)))

    val drive = DriveBuilder.getDrive(keys, "DriveTesting")

    // content folder on Drive (path below root)
    val driveInputPath = projectConfig.driveInputPath.split("/").toList

    // output folder on Drive for exported website
    val driveOutputPath = projectConfig.driveOutputPath.split("/").toList
    val driveOutputFile = DriveUtils.getFileByPath(drive, DriveUtils.getRoot(drive), driveOutputPath).get

    val masterCollectionName = "master"
    val mainCollectionNames = projectConfig.mainCollectionNames.split(",").toList.map(_.trim)

    /////   /////   /////

    val ct = new ContentTransformer(
        projectConfig.projectDir,
        drive, driveInputPath, driveOutputPath,
        masterCollectionName, mainCollectionNames,
        projectConfig.license)

    // download the metadata, update status
    val metaStatusFile = projectConfig.projectDir + File.separator + "meta_timestamps.txt"
    // to test regenerating pages
    val metaStatus = ExportPages.loadOrEmptyModifiedMap(metaStatusFile)
    // val metaStatus = Export.getEmptyModifiedMap

    val (masterCollection, downloadMetaStatus) = ct.downloadMetadata(metaStatus)
    val updatedMetaStatus = ExportPages.mergeDateTimes(metaStatus, downloadMetaStatus)
    ExportPages.saveModifiedMap(metaStatusFile, updatedMetaStatus)

    // download required files, update status
    val fileStatusFile = projectConfig.projectDir + File.separator + "file_timestamps.txt"
    val fileStatus = ExportPages.loadOrEmptyModifiedMap(fileStatusFile)

    val downloadFileStatus = ct.downloadImages(masterCollection, fileStatus)
    val updatedFileStatus = ExportPages.mergeDateTimes(fileStatus, downloadFileStatus)
    ExportPages.saveModifiedMap(fileStatusFile, updatedFileStatus)

    // perform exports
    val (allPageOutputs, allImageOutputs) = ct.export(downloadMetaStatus, downloadFileStatus, masterCollection, images = true)

    allPageOutputs.foreach(x => println("page created: " + x ))
    allImageOutputs.foreach{case (k, v) => {
      v foreach(x => println("image created: " + k + " -> " + x))
    }}

    // do an upload; only uploading files derived from those that were downloaded
    val filesToUpload = allPageOutputs ++ allImageOutputs.values.toList.flatten
    filesToUpload foreach(x => println("to upload: " + x))

    //

    ct.upload(downloadFileStatus, filesToUpload)
    Desktop.getDesktop.browse(new URI("http://www.googledrive.com/host/" + driveOutputFile.getId))

    ////   /////   /////

  }


}



object DriverCommands {

  val Configure = "config"
  val All = "all"
  val Browse = "browse"
  val BrowseDrive = "browse-drive"
  val Help = "help"


  val CommandsDescriptions = List(
      (Configure, "edit project configuration"),
      (Browse, "browse local project web site"),
      (BrowseDrive, "browse Drive project web site"),
      (All, "sync content from Drive to local, export, sync web from local to Drive"),
      (Help, "show usage"))
}
