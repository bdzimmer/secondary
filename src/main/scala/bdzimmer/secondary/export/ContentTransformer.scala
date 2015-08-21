// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Class to wrap process of downloading content from Google drive,
// exporting, and uploading.

package bdzimmer.secondary.export

import bdzimmer.gdrivescala.{DriveUtils, GoogleDriveKeys}

import scala.collection.JavaConverters._
import scala.collection.{immutable => sci}

import org.apache.commons.io.FileUtils
import com.google.api.services.drive.Drive
import com.google.api.services.drive.model.File
import com.google.api.client.util.DateTime

import scala.ref
import scala.reflect.ClassTag



class ContentTransformer(
    localScratchPath: String,
    drive: Drive,
    driveInputPath: List[String],
    driveOutputPath: List[String],
    masterCollectionName: String,
    mainCollectionNames: List[String]) {



  // val MASTER_COLLECTION_NAME = "master"
  // val MAIN_COLLECTION_NAMES = List("characters", "locations", "lore", "tilesets", "sprites")


  // download YAML files to local temporary area
  val localDownloadPath = localScratchPath + "download/"
  val localExportPath = localScratchPath + "export/"

  val localDownloadPathFile = new java.io.File(localDownloadPath)
  val localExportPathFile = new java.io.File(localExportPath)

  val driveRootFile = DriveUtils.getRoot(drive)
  val driveInputFile = DriveUtils.getFileByPath(drive, driveRootFile, driveInputPath).get
  val driveOutputFile = DriveUtils.getFileByPath(drive, driveRootFile, driveOutputPath).get


  // create local download directory if it doesn't exist
  localDownloadPathFile.mkdir



  def downloadMetadata(fileStatus: FileModifiedMap): (CollectionItem,  FileModifiedMap) = {


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
    val masterCollection = WorldLoader.loadMasterCollection(
        localDownloadPath,
        masterCollectionName, mainCollectionNames,
        updatedFileStatus)

    println("--created collection")

    // used to return downloadFileStatus
    // (masterCollection, downloadFileStatus)

    (masterCollection, downloadFileStatus)

  }



  def downloadImages(masterCollection: CollectionItem, fileStatus:  FileModifiedMap): FileModifiedMap = {

    // find a unique list of the files pointed to by meta items.
    val uniqueFiles = WorldItem.filterTree[MetaItem](masterCollection).map(_.filename).distinct

    downloadFilesIntelligent(uniqueFiles, fileStatus)

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
  def downloadFiles(files: List[(String, File)]): FileModifiedMap = {

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
             masterCollection: CollectionItem, images: Boolean = false): (List[String], FileOutputsMap) = {

    if (!localExportPathFile.exists()) {
      localExportPathFile.mkdir
    }

    // get only the world items that are described in the subset of the
    // meta we just downloaded

    // TODO: this should happen when the world is first loaded,
    // and a list of world items should be passed around rather than the head
    def collectionToList(worldItem: WorldItem): List[WorldItem] = worldItem match {
      case x: CollectionItem => x :: x.children.flatMap(x => collectionToList(x))
      case _ => List(worldItem)
    }


    val masterCollectionList = collectionToList(masterCollection)
    masterCollectionList foreach(x => println("world item: " + x.srcyml + " -- " + x.id))

    val metaToExport = masterCollectionList.filter(x => metaStatus.keySet.contains(x.srcyml))
    metaToExport foreach(x => println("meta to export: " + x.id))

    // the file items to export are:
    // 1) the metaitems in the whole collection whose files were refreshed
    // 2) the characteritems in refreshed metadata (need to get new images in case their images changed)

    val filesToExport = WorldItem.filterList[MetaItem](masterCollectionList).filter(x => fileStatus.keySet.contains(x.filename))
    val charsToExport = WorldItem.filterList[CharacterItem](metaToExport)

    filesToExport foreach(x => println("file to export: " + x.id))

    val license = "Copyright (c) 2015 Ben Zimmer. All rights reserved."

    val exportImages = new ExportImages(localExportPath, license)

    val exportPages = new ExportPages(localExportPath, license)

    val localContentDir = localDownloadPath

    println("--exporting pages")
    val allPageOutputs = List(exportPages.createMasterPage(masterCollection),
                              exportPages.createTasksPage(masterCollectionList),
                              exportPages.createGlossaryPage(masterCollectionList)) ++
                         exportPages.exportPagesList(metaToExport)


    val allImageOutputs = if (images) {
      println("--exporting images")

      val imageOutputs = exportImages.exportAllImages(filesToExport, localDownloadPath)
      val characterOutputs = if (charsToExport.length > 0) {
         exportImages.prepareCharacterImages(charsToExport, WorldItem.filterList[SpritesheetItem](masterCollectionList), localDownloadPath)
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
