// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Class for downloading and uploading project content.

// 2015-09-17: Created from refactoring ContentTransformer.

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

    println("--refreshed YAML metadata")

    downloadFileStatus

  }


  def downloadImages(masterCollection: List[WorldItem], fileStatus:  FileModifiedMap): FileModifiedMap = {
    val imageFiles = ExportPipelines.getReferencedImages(masterCollection)
    val downloadImageStatus = downloadFilesIntelligent(imageFiles, fileStatus)

    println("--refreshed image files")

    downloadImageStatus
  }


  // download only files that are not present in fileStatus or are newer
  // 2015-10-10: modified so it won't attempt to download files that don't exist
  def downloadFilesIntelligent(files: List[String], fileStatus: FileModifiedMap): FileModifiedMap = {

    // find just the files that are newer those in fileStatus or not present in it

    // download each of these files, creating the proper directories in the download
    // location if they don't already exist

    val uniqueFilesDrive = files.map(filename => {

      println("checking status: " + filename)

      val filePath = filename.split(slash).toList
      val driveFile = {

        // original version
        // DriveUtils.getFileByPath(drive, driveInputFile, filePath)

        // if it's already in the fileStatus, get it directly by id
        // otherwise find it
        fileStatus.get(filename) match {
          case Some(x) => Option(drive.files.get(x._1).execute) // also need to deal with the possibility that the file doesn't exist
          case None =>  DriveUtils.getFileByPath(drive, driveInputFile, filePath)
        }

      }

      driveFile.map((filename, _))

    }).flatten

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

      val localDir = filename.split(slash).dropRight(1).mkString(slash)
      val localDirFile = new java.io.File(projConf.localContentPath /localDir)
      localDirFile.mkdirs

      DriveUtils.downloadFile(drive, driveFile, projConf.localContentPath / filename)

      (filename, (driveFile.getId, driveFile.getModifiedDate))

    }}).toMap

  }


  // 2015-07-19
  // new upload function
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

  // TODO: does AppName matter?
  val AppName = "DriveTesting"

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


  def createDrive(projConf: ProjectConfig): Result[String, Drive] = {

    for {

      clientIdFile <- Result.fromFilename(projConf.driveClientIdFile)
      accessTokenFile <-  Result.fromFilename(projConf.driveAccessTokenFile)

      id = DriveBuilder.getClientIdFromJsonFile(clientIdFile)
      token = DriveBuilder.getAccessTokenFromJsonFile(accessTokenFile)

    } yield {
      val keys = GoogleDriveKeys(id, token)
      DriveBuilder.getDrive(keys, AppName)
    }
  }

}
