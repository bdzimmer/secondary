// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// 2015-09-12: Refactoring.
// 2015-09-17: WIP further refactoring.
// 2015-10-20: Changes for YAML parse error handling.
// 2016-01-16: Big refactor.

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


abstract class ExportPipeline(projConf: ProjectConfig)  {

  val ymlFormat = true   // for testing!

  val metaStatusFile = projConf.projectDir / ProjectStructure.MetaStatusFile
  val refStatusFile = projConf.projectDir / ProjectStructure.RefStatusFile
  val itemStatusFile = projConf.projectDir / ProjectStructure.ItemStatusFile

  // download the metadata and get updated status maps
  def downloadMeta(): (FileMap, FileMap)

  // download the referenced files and get updated status maps
  def downloadRefs(world: List[WorldItem]): (FileMap, FileMap)

  def uploadExports(allPageOutputs: List[String], allImageOutputs: FileOutputsMap): Unit

  // side-effecting operation that syncs the world down, exports, syncs up, and returns the world
  // only exports if things have changed since last sync
  def run(): Result[String, CollectionItem] = {

    val (newMetaStatus, metaStatusChanges) = downloadMeta()
    val loadedWorld = ymlFormat match {
      case true => WorldLoaderYML.loadWorld(projConf, newMetaStatus)
      case false => WorldLoaderFlat.loadWorld(projConf, newMetaStatus)
    }

    loadedWorld match {
      case Pass(master) => {

        val world = WorldItem.collectionToList(master)
        val (newRefStatus, refStatusChanges) = downloadRefs(world)
        val (newItemStatus, itemStatusChanges) = loadItemStatus(world)

        // only export the things that have changed
        if (itemStatusChanges.size > 0 || refStatusChanges.size > 0) {

          // val pagesToExport = world.filter(x => metaStatusChanges.keySet.contains(x.srcyml)) // old behavior
          val pagesToExport = world.filter(x => itemStatusChanges.keySet.contains(x.id))
          val filesToExport = WorldItem.filterList[RefItem](world).filter(x => refStatusChanges.keySet.contains(x.filename))

          val (allPageOutputs, allImageOutputs) = ExportPipeline.export(
               pagesToExport, filesToExport, master, world, images = true, projConf)

          uploadExports(allPageOutputs, allImageOutputs)
          saveStatus(newMetaStatus, newRefStatus, newItemStatus)
        } else {
          println("Nothing to do.")
        }
      }
      case Fail(msg) => println(msg)
    }

    loadedWorld
  }


  private def loadItemStatus(world: List[WorldItem]): (ItemMap, ItemMap) = {
    val oldItemStatus = WorldLoader.loadOrEmptyItemMap(itemStatusFile)
    val newItemStatus = world.map(x => (x.id, (x.srcyml, x.hashCode))).toMap
    val changes = newItemStatus.filter({case (k, v) => oldItemStatus.get(k) match {
      case Some(x) => v._2 != x._2
      case None => true
    }})

    (newItemStatus, changes)
  }


  private def saveStatus(
      metaStatus: FileMap,
      refStatus: FileMap,
      itemStatus: ItemMap): Unit = {
    WorldLoader.saveFileMap(metaStatusFile, metaStatus)
    WorldLoader.saveFileMap(refStatusFile, refStatus)
    WorldLoader.saveItemMap(itemStatusFile, itemStatus)
  }


}



class LocalSyncExportPipeline(projConf: ProjectConfig) extends ExportPipeline(projConf) {

  def downloadMeta(): (FileMap, FileMap) = {
    val oldMetaStatus = WorldLoader.loadOrEmptyModifiedMap(metaStatusFile)
    val metaStatusChanges = localMetaStatusChanges(oldMetaStatus, projConf)
    val newMetaStatus = WorldLoader.mergeFileMaps(oldMetaStatus, metaStatusChanges)
    (newMetaStatus, metaStatusChanges)
  }

  def downloadRefs(world: List[WorldItem]): (FileMap, FileMap) = {
    val oldRefStatus = WorldLoader.loadOrEmptyModifiedMap(refStatusFile)
    val refStatusChanges = localFileStatusChanges(world, oldRefStatus, projConf)
    val newRefStatus = WorldLoader.mergeFileMaps(oldRefStatus, refStatusChanges)
    (newRefStatus, refStatusChanges)
  }

  def uploadExports(allPageOutputs: List[String], allImageOutputs: FileOutputsMap): Unit = {
    // do nothing
  }

  private def localMetaStatusChanges(
      oldMetaStatus: FileMap,
      projConf: ProjectConfig): FileMap = {

    val ymlFiles = projConf.localContentPathFile.listFiles.toList.map(_.getName).filter(_.endsWith(".yml"))
    localFileUpdates(ymlFiles, oldMetaStatus, projConf)
  }

  private def localFileStatusChanges(
      world: List[WorldItem],
      oldFileStatus: FileMap,
      projConf: ProjectConfig): FileMap = {

    val imageFiles = ExportPipeline.getReferencedImages(world)
    localFileUpdates(imageFiles, oldFileStatus, projConf)
  }

  // files is names of files relative to local content directory
  private def localFileUpdates(
      files: List[String],
      oldFileStatus: FileMap,
      projConf: ProjectConfig): FileMap = {

    val currentStatus = files.map(x =>
      (x, new File(projConf.localContentPath / x).lastModified))

    currentStatus.filter({case (k, v) => oldFileStatus.get(k) match {
        case Some(x) => v > x._2.getValue
        case None => true
    }}).map(x => (x._1, ("", new DateTime(x._2)))).toMap
  }

}



class DriveSyncExportPipeline(projConf: ProjectConfig, ds: DriveSync) extends ExportPipeline(projConf) {

  def downloadMeta(): (FileMap, FileMap) = {
    val oldMetaStatus = WorldLoader.loadOrEmptyModifiedMap(metaStatusFile)
    val metaStatusChanges = ds.downloadMetadata(oldMetaStatus)
    val newMetaStatus = WorldLoader.mergeFileMaps(oldMetaStatus, metaStatusChanges)
    (newMetaStatus, metaStatusChanges)
  }

  def downloadRefs(world: List[WorldItem]): (FileMap, FileMap) = {
    val oldRefStatus = WorldLoader.loadOrEmptyModifiedMap(refStatusFile)
    val refStatusChanges = ds.downloadImages(world, oldRefStatus)
    val newRefStatus = WorldLoader.mergeFileMaps(oldRefStatus, refStatusChanges)
    (newRefStatus, refStatusChanges)
  }

  def uploadExports(allPageOutputs: List[String], allImageOutputs: FileOutputsMap): Unit = {
    val filesToUpload = allPageOutputs ++ allImageOutputs.values.toList.flatten
    ExportPipeline.logList("file to upload", filesToUpload)
    ds.upload(filesToUpload)
  }

}



object ExportPipeline {

  // simple export - local export everything without regard to what has changed
  def exportAll(projConf: ProjectConfig): Result[String, CollectionItem] = {
    FileUtils.deleteDirectory(projConf.localExportPathFile)
    projConf.localExportPathFile.mkdirs

    val loadedWorld = WorldLoaderYML.loadWorld(projConf)
    loadedWorld match {
      case Pass(master) => {
        val world = WorldItem.collectionToList(master)
        ExportPipeline.exportPagesAndImages(master, world, world, world, projConf)
      }
      case Fail(msg) => println(msg)
    }
    loadedWorld
  }


  // export content from local content location to export location
  // using file timestamps to only process content that has changed
  def export(
      pagesToExport: List[WorldItem], refsToExport: List[RefItem],
      master: CollectionItem, world: List[WorldItem],
      images: Boolean = false, projConf: ProjectConfig): (List[String], FileOutputsMap) = {

    logList("pages to export", pagesToExport.map(_.id))

    val imagesToExport = if (images) {

      // the images to export are:
      // 1) the metaitems in the whole collection whose files were refreshed (refsToExport)
      // 2) the characteritems in refreshed metadata (need to get new images in case their images changed)
      // 3) the imageitems in refreshed metadata that reference wikimedia instead of local files

      val charsToExport = WorldItem.filterList[CharacterItem](pagesToExport)
      val imagesToExport = WorldItem.filterList[ImageItem](pagesToExport)

      logList("refs to export", refsToExport.map(_.id))
      logList("chars to export", charsToExport.map(_.id))
      logList("image to export", imagesToExport.map(_.id))

      refsToExport ++ charsToExport ++ imagesToExport

    } else {
      List()
    }

    val (pageOutputs, imageOutputs) = exportPagesAndImages(
        master, world, pagesToExport, imagesToExport, projConf)

    println("--export finished")

    logList("page created", pageOutputs)
    imageOutputs.foreach{case (k, v) => {
      logList("image created", v.map(k + " -> " + _))
    }}

    (pageOutputs, imageOutputs)
  }


  def exportPagesAndImages(
      master: CollectionItem,
      world: List[WorldItem],
      pagesToExport: List[WorldItem],
      imagesToExport: List[WorldItem],
      projConf: ProjectConfig): (List[String], FileOutputsMap) = {

    val exportPages = new ExportPages(
        master,
        world,
        projConf.localExportPath,
        projConf.license,
        projConf.navbars,
        projConf.editLinks)

    val pageOutputs = List(
        exportPages.createMasterPage(),
        exportPages.createTasksPage(),
        exportPages.createIndexPage(),
        exportPages.createStatsPage()) ++ exportPages.exportPagesList(pagesToExport)

    val imageOutputs = if (imagesToExport.nonEmpty) {
      val exportImages = new ExportImages(
        world,
        projConf.localExportPath,
        projConf.license)
      exportImages.exportAllImages(imagesToExport, projConf.localContentPath)
    } else {
      ExportImages.getEmptyFileOutputsMap
    }

    (pageOutputs, imageOutputs)
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
    if (stylesDir.exists) FileUtils.deleteDirectory(stylesDir)
    stylesDir.mkdir
    FileUtils.moveDirectory(extractedBootstrap, new File(stylesDir, "bootstrap"))

    // generate secondary.css in styles directory
    Styles.createStyleSheet(projConf.localExportPath / "styles" / "secondary.css")

    // copy family tree javascript into output directory
    val treeDestDir = new File(outputDirFile, "tree")
    if (treeDestDir.exists) FileUtils.deleteDirectory(treeDestDir)

    treeDestDir.mkdirs()
    FileUtils.copyURLToFile(
        getClass.getResource("/tree/drawtree.js"),
        new File(treeDestDir, "drawtree.js"))
    FileUtils.copyURLToFile(
        getClass.getResource("/tree/tree.css"),
        new File(treeDestDir, "tree.css"))
  }


  // get all of the local image files referenced by a world
  def getReferencedImages(world: List[WorldItem]): List[String] = {
    // find a unique list of the files pointed to by meta items.
    val uniqueFiles = WorldItem.filterList[RefItem](world).map(_.filename).distinct
    // some of these files are not actual files, but links to remote data.
    // so filter those out
    uniqueFiles.filter(x => !x.startsWith("wikimedia:"))
  }


  def logList(prefix: String, list: List[String]): Unit = {
    list.foreach(x => println(prefix + ": " + x))
  }

}
