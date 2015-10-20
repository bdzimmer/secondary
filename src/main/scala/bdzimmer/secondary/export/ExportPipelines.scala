// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// 2015-09-12: Refactoring.
// 2015-09-17: WIP further refactoring.
// 2015-10-20: Changes for YAML parse error handling.

package bdzimmer.secondary.export

import java.io.File

import scala.collection.JavaConverters._
import scala.collection.{immutable => sci}
import scala.util.{Try, Success, Failure}

import org.apache.commons.io.{FileUtils, FilenameUtils}
import com.google.api.client.util.DateTime

import bdzimmer.gdrivescala.DriveUtils


// pipelines for local and synced exports
object ExportPipelines {

  // content -> web; always export everything
  def exportLocalAll(projConf: ProjectConfig): Unit = {

    FileUtils.deleteDirectory(projConf.localExportPathFile)
    projConf.localExportPathFile.mkdirs

    WorldLoader.loadWorld(projConf) match {

      case Success(master) => {

        val world = WorldLoader.collectionToList(master)

        val exportPages = new ExportPages(
            master,
            world,
            projConf.localExportPath,
            projConf.license)

        val allPageOutputs = List(
            exportPages.createMasterPage,
            exportPages.createTasksPage,
            exportPages.createIndexPage,
            exportPages.createFamilyTreesPage) ++ exportPages.exportPagesList(world)

        val exportImages = new ExportImages(
            world,
            projConf.localExportPath,
            projConf.license)

        val imageOutputs = exportImages.exportAllImages(world, projConf.localContentPath)

      }

      case Failure(e) => println("Invalid YAML in master.")
    }


  }


  // content -> web; use local file time stamps
  // TODO: test after 2015-10-20 changes
  def exportLocalSync(projConf: ProjectConfig): Unit = {

    def localMetaStatusChanges(oldMetaStatus: FileModifiedMap, projConf: ProjectConfig): FileModifiedMap = {
      val ymlFiles = projConf.localContentPathFile.listFiles.toList.map(_.getName).filter(_.endsWith(".yml"))
      localFileUpdates(ymlFiles, oldMetaStatus, projConf)
    }

    def localFileStatusChanges(world: List[WorldItem], oldFileStatus: FileModifiedMap, projConf: ProjectConfig): FileModifiedMap = {
      val imageFiles = getReferencedImages(world)
      localFileUpdates(imageFiles, oldFileStatus, projConf)
    }

    val metaStatusFile = projConf.projectDir + File.separator + ProjectStructure.LocalMetaStatusFile
    val oldMetaStatus = ExportPages.loadOrEmptyModifiedMap(metaStatusFile)
    val metaStatusChanges = localMetaStatusChanges(oldMetaStatus, projConf)
    val newMetaStatus = ExportPages.mergeDateTimes(oldMetaStatus, metaStatusChanges)
    ExportPages.saveModifiedMap(metaStatusFile, newMetaStatus)

    // build the collection

    WorldLoader.loadWorld(projConf, newMetaStatus) match {
      case Success(master) => {

        val world = WorldLoader.collectionToList(master)

        // download referenced files, update status
        val fileStatusFile = projConf.projectDir + File.separator + ProjectStructure.LocalFileStatusFile
        val oldFileStatus = ExportPages.loadOrEmptyModifiedMap(fileStatusFile)
        val fileStatusChanges = localFileStatusChanges(world, oldFileStatus, projConf)
        val newFileStatus = ExportPages.mergeDateTimes(oldFileStatus, fileStatusChanges)
        ExportPages.saveModifiedMap(fileStatusFile, newFileStatus)

        // only export / upload if files have changed
        if (metaStatusChanges.size > 0 || fileStatusChanges.size > 0) {

          val (allPageOutputs, allImageOutputs) = export(
              metaStatusChanges, fileStatusChanges, master, world, images = true, projConf)

        } else {
          println("Nothing to do.")
        }
      }

      case Failure(e) => println("Invalid YAML in master.")

    }
  }


  // Drive -> content, content -> web, web -> Drive
  def exportDriveSync(projConf: ProjectConfig, ds: DriveSync): Unit = {

    // download the metadata, update status
    val metaStatusFile = projConf.projectDir + File.separator + ProjectStructure.DriveMetaStatusFile
    val oldMetaStatus = ExportPages.loadOrEmptyModifiedMap(metaStatusFile)
    val metaStatusChanges = ds.downloadMetadata(oldMetaStatus)
    val newMetaStatus = ExportPages.mergeDateTimes(oldMetaStatus, metaStatusChanges)
    ExportPages.saveModifiedMap(metaStatusFile, newMetaStatus)

    // build the collection

    WorldLoader.loadWorld(projConf, newMetaStatus) match {
      case Success(master) => {

        val world = WorldLoader.collectionToList(master)

        // download referenced files, update status
        val fileStatusFile = projConf.projectDir + File.separator + ProjectStructure.DriveFileStatusFile
        val oldFileStatus = ExportPages.loadOrEmptyModifiedMap(fileStatusFile)
        val fileStatusChanges = ds.downloadImages(world, oldFileStatus)
        val newFileStatus = ExportPages.mergeDateTimes(oldFileStatus, fileStatusChanges)
        ExportPages.saveModifiedMap(fileStatusFile, newFileStatus)

        // only export / upload if files have changed
        if (metaStatusChanges.size > 0 || fileStatusChanges.size > 0) {

          val (allPageOutputs, allImageOutputs) = export(
              metaStatusChanges, fileStatusChanges, master, world, images = true, projConf)

          // do an upload; only upload files derived from those that were downloaded
          val filesToUpload = allPageOutputs ++ allImageOutputs.values.toList.flatten
          filesToUpload foreach(x => println("to upload: " + x))
          ds.upload(filesToUpload)

        } else {
          println("Nothing to do.")
        }
      }

      case Failure(e) => println("Invalid YAML in master.")
    }
  }


  // export content from download location to export location
  // using file timestamps to only process content that has changed
  def export(metaStatus: FileModifiedMap, fileStatus: FileModifiedMap,
             master: CollectionItem, world: List[WorldItem],
             images: Boolean = false, projConf: ProjectConfig): (List[String], FileOutputsMap) = {

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

    println("--exporting pages")
    val exportPages = new ExportPages(master, world, projConf.localExportPath, projConf.license)
    val allPageOutputs = List(
        exportPages.createMasterPage,
        exportPages.createTasksPage,
        exportPages.createIndexPage,
        exportPages.createFamilyTreesPage) ++ exportPages.exportPagesList(metaToExport)

    val allImageOutputs = if (images) {
      println("--exporting images")
      val exportImages = new ExportImages(world, projConf.localExportPath, projConf.license)
      exportImages.exportAllImages(
          filesToExport ++ imagesToExport ++ charsToExport, projConf.localContentPath)
    } else {
      ExportPages.getEmptyFileOutputsMap()
    }

    println("--export finished")

    logCreated(allPageOutputs, allImageOutputs)

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


  def logCreated(allPageOutputs: List[String], allImageOutputs: FileOutputsMap): Unit = {
    allPageOutputs.foreach(x => println("page created: " + x ))
    allImageOutputs.foreach{case (k, v) => {
      v.foreach(x => println("image created: " + k + " -> " + x))
    }}
  }


  // TODO: method for logging lists of strings with a prefix

}
