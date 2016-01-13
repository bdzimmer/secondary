// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// 2015-09-12: Refactoring.
// 2015-09-17: WIP further refactoring.
// 2015-10-20: Changes for YAML parse error handling.

package bdzimmer.secondary.export.controller

import java.io.File

import scala.collection.JavaConverters._
import scala.collection.{immutable => sci}
import scala.util.{Try, Success, Failure}

import org.apache.commons.io.{FileUtils, FilenameUtils}
import com.google.api.client.util.DateTime

import bdzimmer.util.{Result, Pass, Fail}
import bdzimmer.util.StringUtils._

import bdzimmer.gdrivescala.DriveUtils
import bdzimmer.secondary.export.model._
import bdzimmer.secondary.export.view.Styles


// pipelines for local and synced exports
object ExportPipelines {

  // content -> web; always export everything
  def exportLocalAll(projConf: ProjectConfig): Result[String, CollectionItem] = {

    FileUtils.deleteDirectory(projConf.localExportPathFile)
    projConf.localExportPathFile.mkdirs

    val loadedWorld = WorldLoader.loadWorld(projConf)

    loadedWorld match {

      case Pass(master) => {

        val world = WorldItem.collectionToList(master)

        val exportPages = new ExportPages(
            master,
            world,
            projConf.localExportPath,
            projConf.license)

        val allPageOutputs = List(
            exportPages.createMasterPage(),
            exportPages.createTasksPage(),
            exportPages.createIndexPage(),
            exportPages.createStatsPage()) ++ exportPages.exportPagesList(world)

        val exportImages = new ExportImages(
            world,
            projConf.localExportPath,
            projConf.license)

        val imageOutputs = exportImages.exportAllImages(world, projConf.localContentPath)

      }

      case Fail(msg) => println(msg)
    }

    loadedWorld

  }


  // content -> web; use local file time stamps
  def exportLocalSync(projConf: ProjectConfig): Result[String, CollectionItem] = {

    def localMetaStatusChanges(oldMetaStatus: FileModifiedMap, projConf: ProjectConfig): FileModifiedMap = {
      val ymlFiles = projConf.localContentPathFile.listFiles.toList.map(_.getName).filter(_.endsWith(".yml"))
      localFileUpdates(ymlFiles, oldMetaStatus, projConf)
    }

    def localFileStatusChanges(world: List[WorldItem], oldFileStatus: FileModifiedMap, projConf: ProjectConfig): FileModifiedMap = {
      val imageFiles = getReferencedImages(world)
      localFileUpdates(imageFiles, oldFileStatus, projConf)
    }

    val metaStatusFile = projConf.projectDir / ProjectStructure.LocalMetaStatusFile
    val oldMetaStatus = WorldLoader.loadOrEmptyModifiedMap(metaStatusFile)
    val metaStatusChanges = localMetaStatusChanges(oldMetaStatus, projConf)
    val newMetaStatus = WorldLoader.mergeModifiedMaps(oldMetaStatus, metaStatusChanges)

    // build the collection

    val loadedWorld = WorldLoader.loadWorld(projConf, newMetaStatus)

    loadedWorld match {
      case Pass(master) => {

        val world = WorldItem.collectionToList(master)

        // download referenced files, update status
        val fileStatusFile = projConf.projectDir / ProjectStructure.LocalFileStatusFile
        val oldFileStatus = WorldLoader.loadOrEmptyModifiedMap(fileStatusFile)
        val fileStatusChanges = localFileStatusChanges(world, oldFileStatus, projConf)
        val newFileStatus = WorldLoader.mergeModifiedMaps(oldFileStatus, fileStatusChanges)

        // only export / upload if files have changed
        if (metaStatusChanges.size > 0 || fileStatusChanges.size > 0) {

          val (allPageOutputs, allImageOutputs) = export(
              metaStatusChanges, fileStatusChanges, master, world, images = true, projConf)

          // only record updated file status if export succeeds
          WorldLoader.saveModifiedMap(metaStatusFile, newMetaStatus)
          WorldLoader.saveModifiedMap(fileStatusFile, newFileStatus)

        } else {
          println("Nothing to do.")
        }
      }

      case Fail(msg) => println(msg)

    }

    loadedWorld
  }


  // Drive -> content, content -> web, web -> Drive
  def exportDriveSync(projConf: ProjectConfig, ds: DriveSync): Result[String, CollectionItem] = {

    // download the metadata, update status
    val metaStatusFile = projConf.projectDir / ProjectStructure.DriveMetaStatusFile
    val oldMetaStatus = WorldLoader.loadOrEmptyModifiedMap(metaStatusFile)
    val metaStatusChanges = ds.downloadMetadata(oldMetaStatus)
    val newMetaStatus = WorldLoader.mergeModifiedMaps(oldMetaStatus, metaStatusChanges)

    // build the collection

    val loadedWorld = WorldLoader.loadWorld(projConf, newMetaStatus)

    loadedWorld match {
      case Pass(master) => {

        val world = WorldItem.collectionToList(master)

        // download referenced files, update status
        val fileStatusFile = projConf.projectDir / ProjectStructure.DriveFileStatusFile
        val oldFileStatus = WorldLoader.loadOrEmptyModifiedMap(fileStatusFile)
        val fileStatusChanges = ds.downloadImages(world, oldFileStatus)
        val newFileStatus = WorldLoader.mergeModifiedMaps(oldFileStatus, fileStatusChanges)

        // only export / upload if files have changed
        if (metaStatusChanges.size > 0 || fileStatusChanges.size > 0) {

          val (allPageOutputs, allImageOutputs) = export(
              metaStatusChanges, fileStatusChanges, master, world, images = true, projConf)

          // do an upload; only upload files derived from those that were downloaded
          val filesToUpload = allPageOutputs ++ allImageOutputs.values.toList.flatten
          logList("file to upload", filesToUpload)
          ds.upload(filesToUpload)

          // only record updated file status if export succeeds
          WorldLoader.saveModifiedMap(metaStatusFile, newMetaStatus)
          WorldLoader.saveModifiedMap(fileStatusFile, newFileStatus)

        } else {
          println("Nothing to do.")
        }
      }

      case Fail(msg) => println(msg)
    }

    loadedWorld
  }


  // export content from download location to export location
  // using file timestamps to only process content that has changed
  def export(
      metaStatus: FileModifiedMap, fileStatus: FileModifiedMap,
      master: CollectionItem, world: List[WorldItem],
      images: Boolean = false, projConf: ProjectConfig): (List[String], FileOutputsMap) = {

    val exportPages = new ExportPages(master, world, projConf.localExportPath, projConf.license)

    // get only the world items that are described in the subset of the meta just downloaded
    val metaToExport = world.filter(x => metaStatus.keySet.contains(x.srcyml))

    // the file items to export are:
    // 1) the metaitems in the whole collection whose files were refreshed
    // 2) the characteritems in refreshed metadata (need to get new images in case their images changed)
    // 3) the imageitems in refreshed metadata that reference wikimedia instead of local files

    val filesToExport = WorldItem.filterList[MetaItem](world).filter(x => fileStatus.keySet.contains(x.filename))
    val charsToExport = WorldItem.filterList[CharacterItem](metaToExport)
    val imagesToExport = WorldItem.filterList[ImageItem](metaToExport).filter(x => x.filename.startsWith("wikimedia:"))

    // logList("world item", world.map(x => x.srcyml + " -- " + x.id))
    logList("meta to export", metaToExport.map(_.id))
    logList("file to export", filesToExport.map(_.id))
    logList("image to export", imagesToExport.map(_.id))

    println("--exporting pages")
    val allPageOutputs = List(
        exportPages.createMasterPage(),
        exportPages.createTasksPage(),
        exportPages.createIndexPage(),
        exportPages.createStatsPage()) ++ exportPages.exportPagesList(metaToExport)

    val allImageOutputs = if (images) {
      println("--exporting images")
      val exportImages = new ExportImages(world, projConf.localExportPath, projConf.license)
      exportImages.exportAllImages(
          filesToExport ++ imagesToExport ++ charsToExport, projConf.localContentPath)
    } else {
      ExportImages.getEmptyFileOutputsMap
    }

    println("--export finished")

    logList("page created", allPageOutputs)
    allImageOutputs.foreach{case (k, v) => {
      logList("image created", v.map(k + " -> " + _))
    }}

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
    Styles.createStyleSheet(projConf.localExportPath / "styles" / "secondary.css")

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
      (x, new File(projConf.localContentPath / x).lastModified))

    currentStatus.filter({case (k, v) => oldFileStatus.get(k) match {
        case Some(x) => v > x._2.getValue
        case None => true
    }}).map(x => (x._1, ("", new DateTime(x._2)))).toMap

  }


  def logList(prefix: String, list: List[String]): Unit = {
    list.foreach(x => println(prefix + ": " + x))
  }


}
