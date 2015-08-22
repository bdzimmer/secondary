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
    println(driverConfig.MAIN_COLLECTION_NAMES)

    System.exit(0)

    val license = driverConfig.LICENSE
    val localScratchPath = driverConfig.LOCAL_SCRATCH_PATH
    new java.io.File(localScratchPath).mkdirs

    val keys = DriveBuilder.getKeysFromProperties(driverConfig.DRIVE_PROPERTIES_FILE)
    val drive = DriveBuilder.getDrive(keys, "DriveTesting")

    // content folder on Drive (path below root)
    val driveInputPath = driverConfig.DRIVE_INPUT_PATH.split("/").toList

    // output folder on Drive for exported website
    val driveOutputPath = driverConfig.DRIVE_OUTPUT_PATH.split("/").toList
    val driveOutputFile = DriveUtils.getFileByPath(drive, DriveUtils.getRoot(drive), driveOutputPath).get

    val masterCollectionName = "master"
    // val mainCollectionNames = List("characters", "locations", "lore", "tilesets", "sprites")

    val mainCollectionNames = driverConfig.MAIN_COLLECTION_NAMES.split(",").toList.map(_.trim)

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

  val missing = DriverConfig.requiredProperties.filter(x => {
    !prop.prop.keySet().contains(x.key)
  })

  missing foreach(x => {
    System.err.println(
        "property " + x.key + " missing from driver configuration\n" +
        "\tusing default value " + x.default)
  })


  def getProp(cf: ConfigField): String = {
    prop(cf.key).getOrElse(cf.default)
  }

  val LOCAL_SCRATCH_PATH = getProp(DriverConfig.LOCAL_SCRATCH_PATH)
  val DRIVE_PROPERTIES_FILE = getProp(DriverConfig.DRIVE_PROPERTIES_FILE)
  val DRIVE_INPUT_PATH = getProp(DriverConfig.DRIVE_INPUT_PATH)
  val DRIVE_OUTPUT_PATH = getProp(DriverConfig.DRIVE_OUTPUT_PATH)
  val MAIN_COLLECTION_NAMES = getProp(DriverConfig.MAIN_COLLECTION_NAMES)
  val LICENSE = getProp(DriverConfig.LICENSE)
  val LOCAL_CONTENT_PATH = getProp(DriverConfig.LOCAL_CONTENT_PATH)

}


case class ConfigField(key: String, default: String, description: String)

object DriverConfig {

  val homeDir = System.getProperty("user.home")
  val propFilename = homeDir + "/" + "worldbuilder.properties"

  val LOCAL_SCRATCH_PATH = ConfigField("LOCAL_SCRATCH_PATH", "tmp", "Local scratch path")
  val DRIVE_PROPERTIES_FILE = ConfigField("DRIVE_PROPERTIES_FILE", "googledrive.properties", "Drive properties file")
  val DRIVE_INPUT_PATH = ConfigField("DRIVE_INPUT_PATH", "secondary/content", "Drive input path")
  val DRIVE_OUTPUT_PATH = ConfigField("DRIVE_OUTPUT_PATH", "secondary/web", "Drive output path")
  val MAIN_COLLECTION_NAMES = ConfigField("MAIN_COLLECTION_NAMES", "characters,locations,lore,tilesets,sprites", "Main collection names")
  val LICENSE = ConfigField("LICENSE", "", "License text")
  val LOCAL_CONTENT_PATH = ConfigField("LOCAL_CONTENT_PATH", "", "Local content path")

  val requiredProperties =  List(
      LOCAL_SCRATCH_PATH,
      DRIVE_PROPERTIES_FILE,
      DRIVE_INPUT_PATH,
      DRIVE_OUTPUT_PATH,
      MAIN_COLLECTION_NAMES,
      LICENSE,
      LOCAL_CONTENT_PATH)
}
