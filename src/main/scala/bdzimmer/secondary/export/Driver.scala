// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Entry point for world builder export process.

// 2015-08-14: Configuration loaded from properties in user's home directory.

// 2015-08-22: Reworked DriverConfig. Style fixes.

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

    // System.exit(0)


    new java.io.File(driverConfig.localScratchPath).mkdirs

    val keys = DriveBuilder.getKeysFromProperties(driverConfig.drivePropertiesFile)
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

  val localScratchPath = getProp(DriverConfig.localScratchPath)
  val drivePropertiesFile = getProp(DriverConfig.drivePropertiesFile)
  val driveInputPath = getProp(DriverConfig.driveInputPath)
  val driveOutputPath = getProp(DriverConfig.driveOutputPath)
  val mainCollectionNames = getProp(DriverConfig.mainCollectionNames)
  val license = getProp(DriverConfig.license)
  val localContentPath = getProp(DriverConfig.localContentPath)

}


case class ConfigField(key: String, default: String, description: String)

object DriverConfig {

  val homeDir = System.getProperty("user.home")
  val propFilename = homeDir + "/" + "worldbuilder.properties"

  val localScratchPath = ConfigField("localScratchPath", "tmp", "Local scratch path")
  val drivePropertiesFile = ConfigField("drivePropertiesFile", "googledrive.properties", "Drive properties file")
  val driveInputPath = ConfigField("driveInputPath", "secondary/content", "Drive input path")
  val driveOutputPath = ConfigField("driveOutputPath", "secondary/web", "Drive output path")
  val mainCollectionNames = ConfigField("mainCollectionNames", "characters,locations,lore,tilesets,sprites", "Main collection names")
  val license = ConfigField("license", "", "License text")
  val localContentPath = ConfigField("localContentPath", "", "Local content path")

  val requiredProperties =  List(
      localScratchPath,
      drivePropertiesFile,
      driveInputPath,
      driveOutputPath,
      mainCollectionNames,
      license,
      localContentPath)
}
