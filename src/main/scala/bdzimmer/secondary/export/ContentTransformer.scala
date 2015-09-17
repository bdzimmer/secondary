// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Class to wrap process of downloading content from Google drive,
// exporting, and uploading.

// 2015-09-12: Refactoring.
// 2015-09-17: WIP further refactoring.

package bdzimmer.secondary.export

import java.io.File

import scala.collection.JavaConverters._
import scala.collection.{immutable => sci}
import scala.ref
import scala.reflect.ClassTag

import org.apache.commons.io.{FileUtils, FilenameUtils}
import com.google.api.client.util.DateTime
import com.google.api.services.drive.Drive
import com.google.api.services.drive.model.{File => DriveFile}

import bdzimmer.gdrivescala.{DriveBuilder, DriveUtils, GoogleDriveKeys}


// TODO: rename ContentTransformer to something more meaningful
// it really just validates and holds references to Drive files for nonlocal projects.
// functions for syncing to and from Google Drive
class ContentTransformer(projConf: ProjectConfig, drive: Drive) {

  // get Drive files
  // TODO: use pattern matching and sys.exit on these rather than get
  val driveRootFile = DriveUtils.getRoot(drive)
  val driveInputFile = DriveUtils.getFileByPath(drive, driveRootFile, projConf.driveInputPathList).get
  val driveOutputFile = DriveUtils.getFileByPath(drive, driveRootFile, projConf.driveOutputPathList).get

  // create local project directories if they don't already exist
  projConf.localContentPathFile.mkdirs
  projConf.localExportPathFile.mkdirs


  def downloadMetadata(fileStatus: FileModifiedMap): FileModifiedMap = {

    // find the yaml files in drive
    val driveYamlFiles = DriveUtils.getFilesByParent(drive, driveInputFile) filter (_.getTitle.endsWith(".yml"))
    val driveYamlFilenames = driveYamlFiles.map(_.getTitle)

    // delete any local yaml files that aren't present on drive
    projConf.localContentPathFile.listFiles.filter(_.getName.endsWith(".yml")).map(x => {
      if (!driveYamlFilenames.contains(x.getName)) {
        println("deleting local: " + x.getName)
        x.delete
      }
    })

    // download / update the drive files to local with the download function
    val downloadFileStatus = downloadFilesIntelligent(driveYamlFilenames, fileStatus)
    // val updatedFileStatus = ExportPages.mergeDateTimes(fileStatus, downloadFileStatus)

    println("--downloaded YAML metadata")

    downloadFileStatus

  }


  def downloadImages(masterCollection: List[WorldItem], fileStatus:  FileModifiedMap): FileModifiedMap = {
    val imageFiles = ContentTransformer.getReferencedImages(masterCollection)
    downloadFilesIntelligent(imageFiles, fileStatus)
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
      val localDirFile = new java.io.File(projConf.localContentPath + "/" + localDir)
      localDirFile.mkdirs

      DriveUtils.downloadFile(drive, driveFile, projConf.localContentPath + "/" + filename)

      (filename, (driveFile.getId, driveFile.getModifiedDate))

    }}).toMap

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
    // TODO: awkward - empty list for current directory must attempt to be created.
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

  // content -> web; always export everything
  def exportLocalAll(projConf: ProjectConfig): Unit = {

    FileUtils.deleteDirectory(projConf.localExportPathFile)
    projConf.localExportPathFile.mkdirs

    val world = WorldLoader.loadWorld(
        projConf.localContentPath,
        projConf.masterName,
        projConf.mainCollections,
        ExportPages.getEmptyFileModifiedMap)

    val exportPages = new ExportPages(
        world,
        projConf.localExportPath,
        projConf.license)

    val allPageOutputs = List(
        exportPages.createMasterPage,
        exportPages.createTasksPage,
        exportPages.createIndexPage) ++ exportPages.exportPagesList(world)

    val exportImages = new ExportImages(
        world,
        projConf.localExportPath,
        projConf.license)

    val imageOutputs = exportImages.exportAllImages(world, projConf.localContentPath)

  }


  // content -> web; use local file time stamps
  // this is untested!
  def exportLocalSync(projConf: ProjectConfig): Unit = {

    // WIP WIP WIP WIP WIP

    return // do nothing for now if run


    def localMetaStatusChanges(oldMetaStatus: FileModifiedMap, projConf: ProjectConfig): FileModifiedMap = {
      val ymlFiles = projConf.localContentPathFile.listFiles.toList.map(_.getName).filter(_.endsWith(".yml"))
      localFileUpdates(ymlFiles, oldMetaStatus, projConf)
    }

    def localFileStatusChanges(world: List[WorldItem], oldFileStatus: FileModifiedMap, projConf: ProjectConfig): FileModifiedMap = {
      val imageFiles =  getReferencedImages(world)
      localFileUpdates(imageFiles, oldFileStatus, projConf)
    }

    val metaStatusFile = projConf.projectDir + File.separator + ProjectStructure.LocalMetaStatusFile
    val oldMetaStatus = ExportPages.loadOrEmptyModifiedMap(metaStatusFile)
    val metaStatusChanges = localMetaStatusChanges(oldMetaStatus, projConf)
    val newMetaStatus = ExportPages.mergeDateTimes(oldMetaStatus, metaStatusChanges)
    ExportPages.saveModifiedMap(metaStatusFile, newMetaStatus)

    // build the collection
    val world = WorldLoader.loadWorld(
        projConf.localContentPath,
        projConf.masterName,
        projConf.mainCollections,
        newMetaStatus)

    // download referenced files, update status
    val fileStatusFile = projConf.projectDir + File.separator + ProjectStructure.LocalFileStatusFile
    val oldFileStatus = ExportPages.loadOrEmptyModifiedMap(fileStatusFile)
    val fileStatusChanges = localFileStatusChanges(world, oldFileStatus, projConf)
    val newFileStatus = ExportPages.mergeDateTimes(oldFileStatus, fileStatusChanges)
    ExportPages.saveModifiedMap(fileStatusFile, newFileStatus)

    // perform exports
    val (allPageOutputs, allImageOutputs) = ContentTransformer.export(
        metaStatusChanges, fileStatusChanges, world, images = true, projConf)

    allPageOutputs.foreach(x => println("page created: " + x ))
    allImageOutputs.foreach{case (k, v) => {
      v foreach(x => println("image created: " + k + " -> " + x))
    }}

  }



  // Drive -> content, content -> web, web -> Drive
  def exportSync(projConf: ProjectConfig): Unit = {

    // TODO: rename variables like exportLocalSync
    // TODO: think about how to handle duplicate code between the different pipelines

    val drive = createDrive(projConf)
    val ct = new ContentTransformer(projConf, drive)

    // download the metadata, update status
    val metaStatusFile = projConf.projectDir + File.separator + ProjectStructure.DriveMetaStatusFile
    val metaStatus = ExportPages.loadOrEmptyModifiedMap(metaStatusFile)
    val downloadMetaStatus = ct.downloadMetadata(metaStatus)
    val updatedMetaStatus = ExportPages.mergeDateTimes(metaStatus, downloadMetaStatus)
    ExportPages.saveModifiedMap(metaStatusFile, updatedMetaStatus)

    // build the collection
    val masterCollection = WorldLoader.loadWorld(
        projConf.localContentPath,
        projConf.masterName,
        projConf.mainCollections,
        updatedMetaStatus)

    println("--created collection")

    // download referenced files, update status
    val fileStatusFile = projConf.projectDir + File.separator + ProjectStructure.DriveFileStatusFile
    val fileStatus = ExportPages.loadOrEmptyModifiedMap(fileStatusFile)
    val downloadFileStatus = ct.downloadImages(masterCollection, fileStatus)
    val updatedFileStatus = ExportPages.mergeDateTimes(fileStatus, downloadFileStatus)
    ExportPages.saveModifiedMap(fileStatusFile, updatedFileStatus)

    // perform exports
    val (allPageOutputs, allImageOutputs) = ContentTransformer.export(
        downloadMetaStatus, downloadFileStatus, masterCollection, images = true, projConf)

    allPageOutputs.foreach(x => println("page created: " + x ))
    allImageOutputs.foreach{case (k, v) => {
      v foreach(x => println("image created: " + k + " -> " + x))
    }}

    // do an upload; only upload files derived from those that were downloaded
    val filesToUpload = allPageOutputs ++ allImageOutputs.values.toList.flatten
    filesToUpload foreach(x => println("to upload: " + x))

    ct.upload(downloadFileStatus, filesToUpload)

  }



  // export content from download location to export location
  // using file timestamps to only process content that has changed
  def export(metaStatus: FileModifiedMap, fileStatus: FileModifiedMap,
             world: List[WorldItem], images: Boolean = false, projConf: ProjectConfig): (List[String], FileOutputsMap) = {

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
    imagesToExport foreach(x => println("image to export: " + x.id))

    val exportImages = new ExportImages(world, projConf.localExportPath, projConf.license)
    val exportPages = new ExportPages(world, projConf.localExportPath, projConf.license)

    val localContentDir = projConf.localContentPath

    println("--exporting pages")
    val allPageOutputs = List(
        exportPages.createMasterPage,
        exportPages.createTasksPage,
        exportPages.createIndexPage) ++ exportPages.exportPagesList(metaToExport)


    val allImageOutputs = if (images) {
      println("--exporting images")
      exportImages.exportAllImages(
          filesToExport ++ imagesToExport ++ charsToExport, projConf.localContentPath)
    } else {
      ExportPages.getEmptyFileOutputsMap()
    }

    println("--export finished")

    (allPageOutputs, allImageOutputs)

  }




  // generate stylesheets into project web dir
  def addStyles(projConf: ProjectConfig): Unit = {

    val outputDirFile = new File(projConf.localExportPath)

    // if Bootstrap doesn't exist in the project directory, download and extract it
    val extractedBootstrapName = FilenameUtils.removeExtension(Styles.BootstrapFilename)
    val extractedBootstrap = new File(projConf.projectDir, extractedBootstrapName)
    if (!extractedBootstrap.exists) {
      Styles.getBootstrap(projConf.projectDir)
    }

    // copy bootstrap into styles directory in export directory and rename
    val stylesDir = new File(outputDirFile, "styles")
    stylesDir.mkdir
    FileUtils.copyDirectoryToDirectory(extractedBootstrap, stylesDir)
    FileUtils.moveDirectory(
        new File(stylesDir, extractedBootstrapName),
        new File(stylesDir, "bootstrap"))

    // generate secondary.css in styles directory
    Styles.createStyleSheet(projConf.localExportPath + "/styles/" + "secondary.css")

  }


  def createDrive(projConf: ProjectConfig): Drive = {
     val keys = GoogleDriveKeys(
         id = DriveBuilder.getClientIdFromJsonFile(new File(projConf.driveClientIdFile)),
         token = DriveBuilder.getAccessTokenFromJsonFile(new File(projConf.driveAccessTokenFile)))
    DriveBuilder.getDrive(keys, AppName)
  }


  // get all of the local image files referenced by a world
  def getReferencedImages(world: List[WorldItem]): List[String] = {

    // find a unique list of the files pointed to by meta items.
    val uniqueFiles = WorldItem.filterList[MetaItem](world).map(_.filename).distinct

    // some of these files are not actual files, but links to remote data.
    // so filter those out
    uniqueFiles.filter(x => !x.startsWith("wikimedia:"))

  }


  // files is names of files relative to local content directory
  def localFileUpdates(
      files: List[String],
      oldFileStatus: FileModifiedMap,
      projConf: ProjectConfig): FileModifiedMap = {

    val currentStatus = files.map(x =>
      (x, new File(projConf.localContentPath + File.separator + x).lastModified))

    currentStatus.filter({case (k, v) => oldFileStatus.get(k) match {
        case Some(x) => v > x._2.getValue
        case None => true
    }}).map(x => (x._1, ("", new DateTime(x._2)))).toMap

  }


}
