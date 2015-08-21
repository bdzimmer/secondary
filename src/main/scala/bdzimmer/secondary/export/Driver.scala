// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Entry point for world builder export process.

// 2015-08-14: Configuration loaded from properties in user's home directory.

package bdzimmer.secondary.export


import java.awt.Desktop
import java.net.URI
import java.io.File

import scala.collection.JavaConverters._

import org.apache.commons.io.FileUtils

import bdzimmer.gdrivescala.{DriveUtils, DriveBuilder}

import com.google.api.client.util.DateTime


object Driver {


  def main(args: Array[String]): Unit = {

    val driverConfig = new DriverConfig()

    println("driver configuration loaded")

    val license = driverConfig.prop(DriverConfig.LICENSE).get

    val localScratchPath = driverConfig.prop(DriverConfig.LOCAL_SCRATCH_PATH).get

    val keys = DriveBuilder.getKeysFromProperties(driverConfig.prop(DriverConfig.DRIVE_PROPERTIES_FILE).get)
    val drive = DriveBuilder.getDrive(keys, "DriveTesting")

    // content folder on Drive (path below root)
    val driveInputPath = driverConfig.prop(DriverConfig.DRIVE_INPUT_PATH).get.split("/").toList

    // output folder on Drive for exported website
    val driveOutputPath = driverConfig.prop(DriverConfig.DRIVE_OUTPUT_PATH).get.split("/").toList
    val driveOutputFile = DriveUtils.getFileByPath(drive, DriveUtils.getRoot(drive), driveOutputPath).get

    val masterCollectionName = "master"
    // val mainCollectionNames = List("characters", "locations", "lore", "tilesets", "sprites")

    val mainCollectionNames = driverConfig.prop(DriverConfig.MAIN_COLLECTION_NAMES).get.split(",").toList.map(_.trim)

    /////   /////   /////

    val ct = new ContentTransformer(
        localScratchPath,
        drive, driveInputPath, driveOutputPath,
        masterCollectionName, mainCollectionNames)

    // download the metadata, update status
    val metaStatusFile = localScratchPath + "/" + "meta_timestamps.txt"
    // to test regenerating pages
    val metaStatus = ExportPages.loadOrEmptyModifiedMap(metaStatusFile)
    // val metaStatus = Export.getEmptyModifiedMap

    val (masterCollection, downloadMetaStatus) = ct.downloadMetadata(metaStatus)
    val updatedMetaStatus = ExportPages.mergeDateTimes(metaStatus, downloadMetaStatus)
    ExportPages.saveModifiedMap(metaStatusFile, updatedMetaStatus)

    // download required files, update status
    val fileStatusFile = localScratchPath + "/" + "file_timestamps.txt"
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

    Desktop.getDesktop.browse(new URI(localScratchPath + "/export/index.html"))

    ct.upload(downloadFileStatus, filesToUpload)
    Desktop.getDesktop.browse(new URI("http://www.googledrive.com/host/" + driveOutputFile.getId))

    ////   /////   /////

  }

}



class DriverConfig() {

  val prop = new PropertiesWrapper(DriverConfig.propFilename)

  // verify that everything is in the properties
  val missing = DriverConfig.requiredProperties.filter(x => {
    !prop.prop.keySet().contains(x._1)
  })

  if (missing.length > 0) {
    throw new Exception(
        "Required properties missing from driver configuration:\n" +
        missing.map(x => x._1 + " - " + x._2).mkString("\n"))
  }

}


object DriverConfig {

  val homeDir = System.getProperty("user.home")
  val propFilename = homeDir + "/" + "worldbuilder.properties"

  val LOCAL_SCRATCH_PATH = "LOCAL_SCRATCH_PATH"
  val DRIVE_PROPERTIES_FILE = "DRIVE_PROPERTIES_FILE"
  val DRIVE_INPUT_PATH = "DRIVE_INPUT_PATH"
  val DRIVE_OUTPUT_PATH = "DRIVE_OUTPUT_PATH"
  val MAIN_COLLECTION_NAMES = "MAIN_COLLECTION_NAMES"
  val LICENSE = "LICENSE"
  val LOCAL_CONTENT_PATH = "LOCAL_CONTENT_PATH"

  val requiredProperties =  List(
      (LOCAL_SCRATCH_PATH, "Local scratch path"),
      (DRIVE_PROPERTIES_FILE, "Drive properties file"),
      (DRIVE_INPUT_PATH, "Drive input path"),
      (DRIVE_OUTPUT_PATH, "Drive output path"),
      (MAIN_COLLECTION_NAMES, "Main collection "),
      (LICENSE, "License text"),
      (LOCAL_CONTENT_PATH, "Local content path"))
}
