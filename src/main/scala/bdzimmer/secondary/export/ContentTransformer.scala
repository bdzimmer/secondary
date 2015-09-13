// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Class to wrap process of downloading content from Google drive,
// exporting, and uploading.

// 2015-09-12: Refactoring.

package bdzimmer.secondary.export

import bdzimmer.gdrivescala.{DriveUtils, GoogleDriveKeys}

import java.io.File

import scala.collection.JavaConverters._
import scala.collection.{immutable => sci}
import scala.ref
import scala.reflect.ClassTag

import org.apache.commons.io.FileUtils
import com.google.api.client.util.DateTime
import com.google.api.services.drive.Drive
import com.google.api.services.drive.model.{File => DriveFile}

import bdzimmer.gdrivescala.DriveBuilder


// functions for syncing to and from Google Drive
class ContentTransformer(projConf: DriverConfig, drive: Drive) {

  // get Drive files
  val driveRootFile = DriveUtils.getRoot(drive)
  val driveInputFile = DriveUtils.getFileByPath(drive, driveRootFile, projConf.driveInputPathList).get
  val driveOutputFile = DriveUtils.getFileByPath(drive, driveRootFile, projConf.driveOutputPathList).get

  // create local project directories if they don't already exist
  projConf.localDownloadPathFile.mkdirs
  projConf.localExportPathFile.mkdirs


  def downloadMetadata(fileStatus: FileModifiedMap): (List[WorldItem],  FileModifiedMap) = {

    // find the yaml files in drive
    val driveYamlFiles = DriveUtils.getFilesByParent(drive, driveInputFile) filter (_.getTitle.contains(".yml"))
    val driveYamlFilenames = driveYamlFiles.map(_.getTitle)

    // delete any local yaml files that aren't present on drive
    projConf.localDownloadPathFile.listFiles.filter(_.getName.endsWith(".yml")).map(x => {
      if (!driveYamlFilenames.contains(x.getName)) {
        println("deleting local: " + x.getName)
        x.delete
      }
    })

    // download / update the drive files to local with the download function
    val downloadFileStatus = downloadFilesIntelligent(driveYamlFilenames, fileStatus)
    val updatedFileStatus = ExportPages.mergeDateTimes(fileStatus, downloadFileStatus)

    println("--downloaded YAML metadata")

    // build the collection
    val masterCollection = WorldLoader.loadWorld(
        projConf.localDownloadPath,
        projConf.masterName,
        projConf.mainCollections,
        updatedFileStatus)

    println("--created collection")

    (masterCollection, downloadFileStatus)

  }


  def downloadImages(masterCollection: List[WorldItem], fileStatus:  FileModifiedMap): FileModifiedMap = {

    // find a unique list of the files pointed to by meta items.
    val uniqueFiles = WorldItem.filterList[MetaItem](masterCollection).map(_.filename).distinct

    // some of these files are not actual files, but links to remote data.
    // so filter those out
    val filteredFiles = uniqueFiles.filter(x => !x.startsWith("wikimedia:"))

    downloadFilesIntelligent(filteredFiles, fileStatus)

  }


  // 2015-07-12
  // a stab at more intelligent content downloading / syncing
  // only downloads files referred to by the metadata

  // 2015-07-18
  // Only downloads files that are not present in fileStatus or are newer.

  def downloadFilesIntelligent(files: List[String], fileStatus: FileModifiedMap): FileModifiedMap = {

    // find just the files that are newer those in fileStatus or not present in it

    // download each of these files, creating the proper directories in the download
    // location if they don't already exist

    val uniqueFilesDrive = files.map(filename => {
      val filePath = filename.split("/").toList
      val driveFile = DriveUtils.getFileByPath(drive, driveInputFile, filePath).get
      (filename, driveFile)
    })

    val filesToDownload = uniqueFilesDrive.filter({case (filename, driveFile) => {
      fileStatus.get(filename) match {
        case Some(x) => driveFile.getModifiedDate.getValue > x._2.getValue
        case None => true
      }

    }})

    downloadFiles(filesToDownload)

  }


  // download a list of files, creating proper directories in the
  // download location if they don't already exist.
  def downloadFiles(files: List[(String, DriveFile)]): FileModifiedMap = {

    files.map({case (filename, driveFile) => {

      println("downloading: " + filename)

      val localDir = filename.split("/").dropRight(1).mkString("/")
      val localDirFile = new java.io.File(projConf.localDownloadPath + "/" + localDir)
      localDirFile.mkdirs

      DriveUtils.downloadFile(drive, driveFile, projConf.localDownloadPath + "/" + filename)

      (filename, (driveFile.getId, driveFile.getModifiedDate))

    }}).toMap

  }


  // export content from download location to export location
  // using master collection
  def export(metaStatus: FileModifiedMap, fileStatus: FileModifiedMap,
             world: List[WorldItem], images: Boolean = false): (List[String], FileOutputsMap) = {

    // get only the world items that are described in the subset of the
    // meta we just downloaded
    // val world = WorldLoader.collectionToList(masterCollection)

    world foreach(x => println("world item: " + x.srcyml + " -- " + x.id))

    val metaToExport = world.filter(x => metaStatus.keySet.contains(x.srcyml))
    metaToExport foreach(x => println("meta to export: " + x.id))

    // the file items to export are:
    // 1) the metaitems in the whole collection whose files were refreshed
    // 2) the characteritems in refreshed metadata (need to get new images in case their images changed)
    // 3) the imageitems in refreshed metadata that reference wikimedia instead of local files

    // TODO: better names here
    val filesToExport = WorldItem.filterList[MetaItem](world).filter(x => fileStatus.keySet.contains(x.filename))
    val charsToExport = WorldItem.filterList[CharacterItem](metaToExport)
    val imagesToExport = WorldItem.filterList[ImageItem](metaToExport).filter(x => x.filename.startsWith("wikimedia:"))

    filesToExport foreach(x => println("file to export: " + x.id))

    val exportImages = new ExportImages(world, projConf.localExportPath, projConf.license)
    val exportPages = new ExportPages(world, projConf.localExportPath, projConf.license)

    val localContentDir = projConf.localDownloadPath

    println("--exporting pages")
    val allPageOutputs = List(exportPages.createMasterPage,
                              exportPages.createTasksPage,
                              exportPages.createIndexPage) ++
                         exportPages.exportPagesList(metaToExport)


    val allImageOutputs = if (images) {
      println("--exporting images")

      val imageOutputs = exportImages.exportAllImages(
          filesToExport ++ imagesToExport, projConf.localDownloadPath)

      val characterOutputs = if (charsToExport.length > 0) {
         exportImages.prepareCharacterImages(
             charsToExport,
             projConf.localDownloadPath)
      } else {
        ExportPages.getEmptyFileOutputsMap()
      }

      ExportPages.mergeFileOutputsMaps(imageOutputs, characterOutputs)

    } else {
      ExportPages.getEmptyFileOutputsMap()
    }

    println("--export finished")

    (allPageOutputs, allImageOutputs)

  }



  // 2015-07-19
  // new upload function
  def upload(fileStatus: FileModifiedMap, filesToUpload: List[String]): Unit = {

    val filesToUploadSplit = filesToUpload map (_.split("/").toList)

    // for each file to upload, delete it if it already exists
    val driveFiles = filesToUpload.zip(filesToUploadSplit.map(x => DriveUtils.getFileByPath(drive, driveOutputFile, x)))
    driveFiles.map({case(path, f) => f.foreach(x => {
      println("deleting drive: " + path )
      DriveUtils.deleteFile(drive, x)
    })})

    // create all the parent directories first
    val parentDirs = filesToUploadSplit.map(_.dropRight(1))

    // Note: when createFolders is called with List() for subfolders,
    // parent is returned - produces correct behavior
    val parentDirsMap = parentDirs.distinct.map(x => {
      println("creating drive folder: " + x.mkString("/"))
      (x, DriveUtils.createFolders(drive, driveOutputFile, x).get)
    }).toMap

    // then upload the files
    val resultFiles = filesToUpload.zip(parentDirs).map({case (x, drivePath) => {
      println("uploading: " + x)
      // val location = DriveUtils.createFolders(drive, driveOutputFile, x.split("/").toList.dropRight(1)).get

      val location = parentDirsMap(drivePath)
      DriveUtils.uploadFile(drive, projConf.localExportPath + "/" + x, location)

    }})

  }

}



// pipelines for local and synced exports
object ContentTransformer {

  // TODO: does AppName matter?
  val AppName = "DriveTesting"

  // local -> web
  def exportLocal(projConf: DriverConfig): Unit = {

    val outputDirFile = new File(projConf.localExportPath)
    FileUtils.deleteDirectory(outputDirFile)
    outputDirFile.mkdirs

    val world = WorldLoader.loadWorld(
        projConf.localContentPath,
        projConf.masterName,
        projConf.mainCollections,
        ExportPages.getEmptyFileModifiedMap)

    val exportPages = new ExportPages(
        world,
        projConf.localExportPath,
        projConf.license)

    val allPageOutputs = List(exportPages.createMasterPage,
                              exportPages.createTasksPage,
                              exportPages.createIndexPage) ++
                         exportPages.exportPagesList(world)

    val exportImages = new ExportImages(
        world,
        projConf.localExportPath,
        projConf.license)

    val imageOutputs = exportImages.exportAllImages(world, projConf.localContentPath)

    val charsToExport = WorldItem.filterList[CharacterItem](world)
    val characterOutputs = if (charsToExport.length > 0) {
       exportImages.prepareCharacterImages(charsToExport, projConf.localContentPath)
    } else {
      ExportPages.getEmptyFileOutputsMap()
    }


  }


  // Drive -> cache, cache -> web, web -> Drive
  def exportSync(projConf: DriverConfig): Unit = {

    val drive = createDrive(projConf)
    val ct = new ContentTransformer(projConf, drive)

    // download the metadata, update status
    val metaStatusFile = projConf.projectDir + File.separator + ProjectStructure.MetaStatusFile
    val metaStatus = ExportPages.loadOrEmptyModifiedMap(metaStatusFile)
    val (masterCollection, downloadMetaStatus) = ct.downloadMetadata(metaStatus)
    val updatedMetaStatus = ExportPages.mergeDateTimes(metaStatus, downloadMetaStatus)
    ExportPages.saveModifiedMap(metaStatusFile, updatedMetaStatus)

    // download referenced files, update status
    val fileStatusFile = projConf.projectDir + File.separator + ProjectStructure.FileStatusFile
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

    ct.upload(downloadFileStatus, filesToUpload)

  }


  def createDrive(projConf: DriverConfig): Drive = {
     val keys = GoogleDriveKeys(
         id = DriveBuilder.getClientIdFromJsonFile(new File(projConf.driveClientIdFile)),
         token = DriveBuilder.getAccessTokenFromJsonFile(new File(projConf.driveAccessTokenFile)))
    DriveBuilder.getDrive(keys, AppName)
  }

}
