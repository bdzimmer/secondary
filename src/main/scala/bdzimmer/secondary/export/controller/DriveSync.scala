// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Class for downloading and uploading project content.

// TODO: think about generalizing some of these methods
// TODO: Refactor into separate project - general drive sync capability

package bdzimmer.secondary.export.controller

import java.io.File
import java.util.regex.Pattern

import com.google.api.services.drive.Drive
import com.google.api.services.drive.model.{File => DriveFile}

import bdzimmer.util.{Result, Pass, Fail}
import bdzimmer.util.StringUtils._

import bdzimmer.gdrivescala.{DriveBuilder, DriveUtils, GoogleDriveKeys}
import bdzimmer.secondary.export.model.{ProjectConfig, WorldItem}

// functions for syncing to and from Google Drive

class DriveSync(
    projConf: ProjectConfig,
    drive: Drive,
    val driveInputFile: DriveFile,
    val driveOutputFile: DriveFile) {

  // create local project directories if they don't already exist
  projConf.localContentPathFile.mkdirs
  projConf.localExportPathFile.mkdirs


  val driveContentStatusFile = projConf.projectDir / "drivecontentstatus.txt"
  val localWebStatusFile     = projConf.projectDir / "localwebstatus.txt"


  // download any changed files in the content directory
  // run before export
  def downloadInput(): Unit = {

    val driveContentStatus = WorldLoader.loadOrEmptyModifiedMap(driveContentStatusFile)

    // recursively find all files in Drive content folder
    val driveContentFiles = driveFilesbyParentRecursive(drive, driveInputFile, List())

    // TODO: delete local content files that aren't in drive content

    // download / update the drive files to local with the download function
    val driveContentStatusUpdates = downloadFilesIntelligent(driveContentFiles.map(_._1.mkString(slash)), driveContentStatus)

    val driveContentStatusNew = WorldLoader.mergeFileMaps(driveContentStatus, driveContentStatusUpdates)
    WorldLoader.saveFileMap(driveContentStatusFile, driveContentStatusNew)

  }


  // upload output file changes
  // run after export
  def uploadOutput(): Unit = {

    val localWebStatus = WorldLoader.loadOrEmptyModifiedMap(localWebStatusFile)

    // recursively find all files in local web folder
    val localContentFiles = filesByParentRecursive(projConf.localExportPathFile, List())

    // TODO: delete Drive web files that aren't in local web

    val localWebStatusUpdates = ExportPipeline.localFileUpdates(
        localContentFiles.map(_._1.mkString(slash)), localWebStatus, projConf.localExportPath)

    // upload only the updated files
    upload(localWebStatusUpdates.keys.toList)

    val localWebStatusNew = WorldLoader.mergeFileMaps(localWebStatus, localWebStatusUpdates)
    WorldLoader.saveFileMap(localWebStatusFile, localWebStatusNew)

  }


  private def driveFilesbyParentRecursive(drive: Drive, driveDir: DriveFile, driveDirName: List[String]): List[(List[String], DriveFile)] = {
    DriveUtils.getFilesByParent(drive, driveDir).flatMap(file => file.getMimeType.equals(DriveUtils.folderType) match {
      case true  => driveFilesbyParentRecursive(drive, file, driveDirName :+ file.getTitle)
      case false => List((driveDirName :+ file.getTitle, file))
    })
  }


  private def filesByParentRecursive(dir: File, dirName: List[String]): List[(List[String], File)] = {
    dir.listFiles.toList.flatMap(file => file.isDirectory() match {
      case true  => filesByParentRecursive(file, dirName :+ file.getName)
      case false => List((dirName :+ file.getName, file))
    })
  }


  // download only files that are
  // 1) not present in fileStatus
  // 2) newer than what is recorded in fileStatus
  // 3) don't exist locally
  def downloadFilesIntelligent(files: List[String], fileStatus: FileMap): FileMap = {

    // find just the files that are newer those in fileStatus or not present in it

    // download each of these files, creating the proper directories in the download
    // location if they don't already exist

    val uniqueFilesDrive = files.map(filename => {

      println("checking status: " + filename)

      val filePath = filename.split(slash).toList
      val driveFile = {

        // if it's already in the fileStatus, get it directly by id
        // otherwise find it
        fileStatus.get(filename) match {
          case Some(x) => Option(drive.files.get(x._1).execute) // also need to deal with the possibility that the file doesn't exist
          case None    => DriveUtils.getFileByPath(drive, driveInputFile, filePath)
        }

      }

      driveFile.map((filename, _))

    }).flatten

    // only download the a file if it's newer than what's in the fileStatus or it doesn't exist locally
    val filesToDownload = uniqueFilesDrive.filter({case (filename, driveFile) => {
      fileStatus.get(filename) match {
        case Some(x) => driveFile.getModifiedDate.getValue > x._2.getValue || !(new File(projConf.localContentPath / filename).exists)
        case None    => true
      }

    }})

    downloadFiles(filesToDownload)

  }


  // download a list of files, creating proper directories in the
  // download location if they don't already exist.
  def downloadFiles(files: List[(String, DriveFile)]): FileMap = {

    files.map({case (filename, driveFile) => {

      println("downloading: " + filename)

      val localDir = filename.split(slash).dropRight(1).mkString(slash)
      val localDirFile = new java.io.File(projConf.localContentPath /localDir)
      localDirFile.mkdirs

      DriveUtils.downloadFile(drive, driveFile, projConf.localContentPath / filename)

      (filename, (driveFile.getId, driveFile.getModifiedDate))

    }}).toMap

  }


  // upload files
  def upload(filesToUpload: List[String]): Unit = {

    val filesToUploadSplit = filesToUpload.map(
        _.split(Pattern.quote(slash)).toList)

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
    // TODO: using get on Option!!!!
    val parentDirsMap = parentDirs.distinct.map(x => {
      println("creating drive folder: " + x.mkString(slash))
      (x, DriveUtils.createFolders(drive, driveOutputFile, x).get)
    }).toMap

    // then upload the files
    val resultFiles = filesToUpload.zip(parentDirs).map({case (x, drivePath) => {
      println("uploading: " + x)
      // val location = DriveUtils.createFolders(drive, driveOutputFile, x.split(slash).toList.dropRight(1)).get

      val location = parentDirsMap(drivePath)
      DriveUtils.uploadFile(drive, projConf.localExportPath / x, location)

    }})

  }

}



object DriveSync {

  val AppName = "Secondary"  // the actual value of this does not seem to matter

  val DriveWebUrl = "https://drive.google.com/drive/folders"

  // safely create a DriveSync object from the project configuration
  // failure messages if input or output directories don't exist
  def apply(projConf: ProjectConfig): Result[String, DriveSync] = for {

    drive <- DriveSync.createDrive(projConf)
    driveRootFile = DriveUtils.getRoot(drive)

    driveInputFile <- Result.fromOption(
        DriveUtils.getFileByPath(drive, driveRootFile, projConf.driveInputPathList),
        "input path does not exist")

    driveOutputFile <- Result.fromOption(
        DriveUtils.getFileByPath(drive, driveRootFile, projConf.driveOutputPathList),
        "output path does not exist")

  } yield (new DriveSync(projConf, drive, driveInputFile, driveOutputFile))


  def createDrive(projConf: ProjectConfig): Result[String, Drive] =  for {

    clientIdFile <- Result.fromFilename(projConf.driveClientIdFile)
    accessTokenFile <-  Result.fromFilename(projConf.driveAccessTokenFile)

    id = DriveBuilder.getClientIdFromJsonFile(clientIdFile)
    token = DriveBuilder.getAccessTokenFromJsonFile(accessTokenFile)

  } yield {
    val keys = GoogleDriveKeys(id, token)
    DriveBuilder.getDrive(keys, AppName)
  }


}
