// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Class for downloading and uploading project content.

// 2015-09-17: Created from refactoring ContentTransformer.

package bdzimmer.secondary.export

import java.io.File
import java.util.regex.Pattern

import com.google.api.services.drive.Drive
import com.google.api.services.drive.model.{File => DriveFile}

import bdzimmer.gdrivescala.{DriveBuilder, DriveUtils, GoogleDriveKeys}


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
    downloadFilesIntelligent(imageFiles, fileStatus)
  }


  // download only files that are not present in fileStatus or are newer
  // 2015-10-10: modified so it won't attempt to download files that don't exist
  def downloadFilesIntelligent(files: List[String], fileStatus: FileModifiedMap): FileModifiedMap = {

    // find just the files that are newer those in fileStatus or not present in it

    // download each of these files, creating the proper directories in the download
    // location if they don't already exist

    val uniqueFilesDrive = files.map(filename => {
      val filePath = filename.split("/").toList
      val driveFile = DriveUtils.getFileByPath(drive, driveInputFile, filePath)
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

      val localDir = filename.split("/").dropRight(1).mkString(File.separator)
      val localDirFile = new java.io.File(projConf.localContentPath + File.separator + localDir)
      localDirFile.mkdirs

      DriveUtils.downloadFile(drive, driveFile, projConf.localContentPath + File.separator + filename)

      (filename, (driveFile.getId, driveFile.getModifiedDate))

    }}).toMap

  }


  // 2015-07-19
  // new upload function
  def upload(filesToUpload: List[String]): Unit = {


    // TODO: quote file separator string for pattern (fails when file separator is a backslash)
    val filesToUploadSplit = filesToUpload.map(
        _.split(Pattern.quote(File.separator)).toList)

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
      println("creating drive folder: " + x.mkString(File.separator))
      (x, DriveUtils.createFolders(drive, driveOutputFile, x).get)
    }).toMap

    // then upload the files
    val resultFiles = filesToUpload.zip(parentDirs).map({case (x, drivePath) => {
      println("uploading: " + x)
      // val location = DriveUtils.createFolders(drive, driveOutputFile, x.split("/").toList.dropRight(1)).get

      val location = parentDirsMap(drivePath)
      DriveUtils.uploadFile(drive, projConf.localExportPath + File.separator + x, location)

    }})

  }

}



object DriveSync {

  // TODO: does AppName matter?
  val AppName = "DriveTesting"

  def createDrive(projConf: ProjectConfig): Drive = {
     val keys = GoogleDriveKeys(
         id = DriveBuilder.getClientIdFromJsonFile(new File(projConf.driveClientIdFile)),
         token = DriveBuilder.getAccessTokenFromJsonFile(new File(projConf.driveAccessTokenFile)))
    DriveBuilder.getDrive(keys, AppName)
  }

}
