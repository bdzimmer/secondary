// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Class to wrap process of downloading content from Google drive,
// exporting, and uploading.

package bdzimmer.secondary.export

import bdzimmer.gdrivescala.{DriveUtils, GoogleDriveKeys}

import java.io.File

import scala.collection.JavaConverters._
import scala.collection.{immutable => sci}

import org.apache.commons.io.FileUtils
import com.google.api.services.drive.Drive
import com.google.api.services.drive.model.{File => DriveFile}
import com.google.api.client.util.DateTime


import scala.ref
import scala.reflect.ClassTag



class ContentTransformer(projectConfig: DriverConfig, drive: Drive) {

  val localDownloadPath = projectConfig.projectDir + File.separator + ProjectStructure.CacheDir + File.separator
  val localExportPath = projectConfig.projectDir  + File.separator +  ProjectStructure.WebDir + File.separator

  val localDownloadPathFile = new File(localDownloadPath)
  val localExportPathFile = new File(localExportPath)

  // get Drive files
  val driveRootFile = DriveUtils.getRoot(drive)
  val driveInputFile = DriveUtils.getFileByPath(drive, driveRootFile, projectConfig.driveInputPathList).get
  val driveOutputFile = DriveUtils.getFileByPath(drive, driveRootFile, projectConfig.driveOutputPathList).get

  // create local project directories if they don't already exist
  localDownloadPathFile.mkdirs
  localExportPathFile.mkdirs




  def downloadMetadata(fileStatus: FileModifiedMap): (List[WorldItem],  FileModifiedMap) = {


    // find the yaml files in drive
    val driveYamlFiles = DriveUtils.getFilesByParent(drive, driveInputFile) filter (_.getTitle.contains(".yml"))
    val driveYamlFilenames = driveYamlFiles.map(_.getTitle)

    // delete any local yaml files that aren't present on drive
    localDownloadPathFile.listFiles.filter(_.getName.endsWith(".yml")).map(x => {
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
        localDownloadPath,
        projectConfig.masterName,
        projectConfig.mainCollections,
        updatedFileStatus)

    println("--created collection")

    // used to return downloadFileStatus
    // (masterCollection, downloadFileStatus)

    (masterCollection, downloadFileStatus)

  }



  def downloadImages(masterCollection: List[WorldItem], fileStatus:  FileModifiedMap): FileModifiedMap = {

    // find a unique list of the files pointed to by meta items.
    // val uniqueFiles = WorldItem.filterTree[MetaItem](masterCollection).map(_.filename).distinct
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
      val localDirFile = new java.io.File(localDownloadPath + "/" + localDir)
      localDirFile.mkdirs

      DriveUtils.downloadFile(drive, driveFile, localDownloadPath + "/" + filename)

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

    val exportImages = new ExportImages(world, localExportPath, projectConfig.license)
    val exportPages = new ExportPages(world, localExportPath, projectConfig.license)

    val localContentDir = localDownloadPath

    println("--exporting pages")
    val allPageOutputs = List(exportPages.createMasterPage,
                              exportPages.createTasksPage,
                              exportPages.createIndexPage) ++
                         exportPages.exportPagesList(metaToExport)


    val allImageOutputs = if (images) {
      println("--exporting images")

      val imageOutputs = exportImages.exportAllImages(filesToExport ++ imagesToExport, localDownloadPath)

      val characterOutputs = if (charsToExport.length > 0) {
         exportImages.prepareCharacterImages(
             charsToExport,
             localDownloadPath)
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
      DriveUtils.uploadFile(drive, localExportPath + "/" + x, location)

    }})


  }


}
