// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Entry point for world builder export process.

// 2015-08-14: Configuration loaded from properties in user's home directory.
// 2015-08-22: Reworked DriverConfig. Style fixes.
// 2015-08-30: Further configuration updates.

package bdzimmer.secondary.export


import java.awt.Desktop
import java.net.URI
import java.io.File

import scala.collection.JavaConverters._

import com.google.api.client.util.DateTime
import org.apache.commons.io.FileUtils

import bdzimmer.gdrivescala.{DriveUtils, DriveBuilder, GoogleDriveKeys}


object Driver {


  def main(args: Array[String]): Unit = {

    val driverConfig = new DriverConfig()

    println("driver configuration loaded")

    new java.io.File(driverConfig.localScratchPath).mkdirs

    val keys = GoogleDriveKeys(
      id = DriveBuilder.getClientIdFromJsonFile(
          new java.io.File(driverConfig.driveClientIdFile)),
      token = DriveBuilder.getAccessTokenFromJsonFile(
          new java.io.File(driverConfig.driveAccessTokenFile)))

    val drive = DriveBuilder.getDrive(keys, "DriveTesting")

    // content folder on Drive (path below root)
    val driveInputPath = driverConfig.driveInputPath.split("/").toList

    // output folder on Drive for exported website
    val driveOutputPath = driverConfig.driveOutputPath.split("/").toList
    val driveOutputFile = DriveUtils.getFileByPath(drive, DriveUtils.getRoot(drive), driveOutputPath).get

    val masterCollectionName = "master"
    // val mainCollectionNames = List("characters", "locations", "lore", "tilesets", "sprites")

    val mainCollectionNames = driverConfig.mainCollectionNames.split(",").toList.map(_.trim)

    /////   /////   /////

    val ct = new ContentTransformer(
        driverConfig.localScratchPath,
        drive, driveInputPath, driveOutputPath,
        masterCollectionName, mainCollectionNames,
        driverConfig.license)

    // download the metadata, update status
    val metaStatusFile = driverConfig.localScratchPath + "/" + "meta_timestamps.txt"
    // to test regenerating pages
    val metaStatus = ExportPages.loadOrEmptyModifiedMap(metaStatusFile)
    // val metaStatus = Export.getEmptyModifiedMap

    val (masterCollection, downloadMetaStatus) = ct.downloadMetadata(metaStatus)
    val updatedMetaStatus = ExportPages.mergeDateTimes(metaStatus, downloadMetaStatus)
    ExportPages.saveModifiedMap(metaStatusFile, updatedMetaStatus)

    // download required files, update status
    val fileStatusFile = driverConfig.localScratchPath + "/" + "file_timestamps.txt"
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

    Desktop.getDesktop.browse(new URI(driverConfig.localScratchPath + "/export/index.html"))

    ct.upload(downloadFileStatus, filesToUpload)
    Desktop.getDesktop.browse(new URI("http://www.googledrive.com/host/" + driveOutputFile.getId))

    ////   /////   /////

  }

}

