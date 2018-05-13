// Copyright (c) 2018 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.export.controller

import java.io.{File, FileOutputStream}
import java.net.{HttpURLConnection, URL}

import scala.util.Try

import org.apache.commons.io.{FileUtils, FilenameUtils, IOUtils}
import org.apache.commons.compress.archivers.zip.ZipFile

import bdzimmer.secondary.export.model.{ProjectConfig, ProjectStructure, WorldItems, Tags}
import bdzimmer.secondary.export.model.WorldItems._
import bdzimmer.secondary.export.view.Styles
import bdzimmer.secondary.export.view.WebResource

import bdzimmer.util.{Result, Pass, Fail}
import bdzimmer.util.StringUtils._


class ExportProcess(projConf: ProjectConfig)  {

  val metaStatusFile = projConf.projectDir / ProjectStructure.MetaStatusFile
  val refStatusFile  = projConf.projectDir / ProjectStructure.RefStatusFile
  val itemStatusFile = projConf.projectDir / ProjectStructure.ItemStatusFile

  val wikiCache = new FilesystemCache(
      projConf.projectDir / "wikicache",
      ".json",
      ImageDownloader.getWikimediaJson)

  def loadMeta(): (FileMap, FileMap) = {
    val oldMetaStatus = WorldLoader.loadOrEmptyModifiedMap(metaStatusFile)
    val metaStatusChanges = localMetaStatusChanges(oldMetaStatus, projConf)
    val newMetaStatus = WorldLoader.mergeFileMaps(oldMetaStatus, metaStatusChanges)
    (newMetaStatus, metaStatusChanges)
  }


  def loadRefs(world: List[WorldItem]): (FileMap, FileMap) = {
    val oldRefStatus = WorldLoader.loadOrEmptyModifiedMap(refStatusFile)
    val refStatusChanges = localReferencedFileStatusChanges(world, oldRefStatus, projConf)
    val newRefStatus = WorldLoader.mergeFileMaps(oldRefStatus, refStatusChanges)
    (newRefStatus, refStatusChanges)
  }


  def run(): Result[String, CollectionItem] = {

    if (!projConf.localExportPathFile.exists()) {
      println("creating export directory: " + projConf.localExportPath)
      projConf.localExportPathFile.mkdirs()
    }

    val (newMetaStatus, metaStatusChanges) = loadMeta()
    val loadedWorld = WorldLoader.loadWorld(projConf, newMetaStatus)

    loadedWorld match {
      case Pass(master) => {

        val world = WorldItems.collectionToList(master)
        val (newRefStatus,  refStatusChanges)  = loadRefs(world)
        val (newItemStatus, itemStatusChanges) = loadItemStatus(world)

        // only export the things that have changed
        if (itemStatusChanges.size > 0 || refStatusChanges.size > 0) {

          // parse all of the tags
          val stringToItem = (world.map(x => (x.id, x)) ++ world.map(x => (x.name, x))).toMap
          val stringToTags = world.map(x => (x.id, x.tags.mapValues(tag => ParseTags.parse(tag, stringToItem)))).toMap
          val hiddenItems = projConf.hiddenItems.split(";\\s+").toList.flatMap(x => {
            stringToItem.get(x).map(WorldItems.collectionToList(_))
          }).flatten.distinct

          val exportPages = new RenderPages(
              master,
              world,
              stringToTags,
              wikiCache,
              projConf.license,
              projConf.navbars,
              projConf.subarticles,
              projConf.relativeLinks,
              hiddenItems,
              projConf.unifiedJumbotron,
              projConf.search)

          val exportImages = new RenderImages(
              world,
              stringToTags,
              wikiCache,
              projConf.localExportPath,
              projConf.license)

          val modifiedIds = itemStatusChanges.keySet

          // items that have been modified
          val modifiedItems = world.filter(x => modifiedIds.contains(x.id))

          // items that modified items reference
          // this will keep "referenced by" lists up to date
          // I don't think this is usually important; so this may be something to make optional
          val modifiedItemsReferences = modifiedItems.flatMap(x => exportPages.references.get(x.id)).flatten

          // items that modified items are referenced by
          // updates link names, flight predictions, etc.
          // This is more important than above in terms of keeping things up to date.
          val modifiedItemsReferencedBy = modifiedItems.flatMap(x => exportPages.referencedBy.get(x.id)).flatten

          modifiedItems.foreach(x             => println("modified item:      " + x.id))
          modifiedItemsReferences.foreach(x   => println("modified item refs: " + x.id))
          modifiedItemsReferencedBy.foreach(x => println("modified item refd: " + x.id))

          // val itemsToExport = (modifiedItems ++ modifiedItemsReferences ++ modifiedItemsReferencedsBy ++ timelineItemsRefresh).distinct
          val itemsToExport = (modifiedItems ++ modifiedItemsReferences ++ modifiedItemsReferencedBy).distinct

          val modifiedRefs = world.collect({case x: RefItem => x}).filter(x => refStatusChanges.keySet.contains(x.filename))

          val (allPageOutputs, allImageOutputs) = ExportPipeline.export(
               itemsToExport, modifiedRefs, exportPages, exportImages,
               projConf.localExportPath, projConf.localContentPath)

          saveStatus(newMetaStatus, newRefStatus, newItemStatus)
        } else {
          println("Nothing to do.")
        }
      }
      case Fail(msg) => println(msg)
    }

    loadedWorld
  }


  private def localMetaStatusChanges(
      oldMetaStatus: FileMap,
      projConf: ProjectConfig): FileMap = {

    val srcFiles = projConf.localContentPathFile.listFiles.toList.map(_.getName).filter(x => {
      x.endsWith(".sec")
    })

    ExportPipeline.localFileUpdates(srcFiles, oldMetaStatus, projConf.localContentPath)
  }


  private def localReferencedFileStatusChanges(
      world: List[WorldItem],
      oldFileStatus: FileMap,
      projConf: ProjectConfig): FileMap = {

    val imageFiles = ExportPipeline.getReferencedImages(world)
    ExportPipeline.localFileUpdates(imageFiles, oldFileStatus, projConf.localContentPath)
  }


  private def loadItemStatus(world: List[WorldItem]): (ItemMap, ItemMap) = {
    val oldItemStatus = WorldLoader.loadOrEmptyItemMap(itemStatusFile)
    val newItemStatus = world.map(x => (x.id, (x.srcfilename, x.hashCode))).toMap
    val changes = newItemStatus.filter({case (k, v) => oldItemStatus.get(k) match {
      case Some(x) => v._2 != x._2
      case None    => true
    }})

    (newItemStatus, changes)
  }


  private def saveStatus(
      metaStatus: FileMap,
      refStatus: FileMap,
      itemStatus: ItemMap): Unit = {
    WorldLoader.saveFileMap(metaStatusFile, metaStatus)
    WorldLoader.saveFileMap(refStatusFile,  refStatus)
    WorldLoader.saveItemMap(itemStatusFile, itemStatus)
  }


}


object ExportPipeline {

  // simple export - local export everything without regard to what has changed
  def exportAll(projConf: ProjectConfig): Result[String, CollectionItem] = {
    FileUtils.deleteDirectory(projConf.localExportPathFile)
    projConf.localExportPathFile.mkdirs()
    val wikiCache = new FilesystemCache(
      projConf.projectDir / "wikicache",
      ".json",
      ImageDownloader.getWikimediaJson)

    val loadedWorld = WorldLoader.loadWorld(projConf)
    loadedWorld match {
      case Pass(master) => {

        val world = WorldItems.collectionToList(master)

        // parse all of the tags
        val stringToItem = (world.map(x => (x.id, x)) ++ world.map(x => (x.name, x))).toMap
        val stringToTags = world.map(x => (x.id, x.tags.mapValues(tag => ParseTags.parse(tag, stringToItem)))).toMap
        val hiddenItems = projConf.hiddenItems.split(";\\s+").toList.flatMap(x => {
          stringToItem.get(x).map(WorldItems.collectionToList(_))
        }).flatten.distinct

        val exportPages = new RenderPages(
          master,
          world,
          stringToTags,
          wikiCache,
          projConf.license,
          projConf.navbars,
          projConf.subarticles,
          projConf.relativeLinks,
          hiddenItems,
          projConf.unifiedJumbotron,
          projConf.search)

        val exportImages = new RenderImages(
          world,
          stringToTags,
          wikiCache,
          projConf.localExportPath,
          projConf.license)

        performExportPages(exportPages, world, projConf.localExportPath)
        performExportImages(exportImages, world, projConf.localContentPath)

      }
      case Fail(msg) => println(msg)
    }
    loadedWorld
  }


  // export content from local content location to export location
  // based on lists of refs to export
  def export(
      pagesToExport: List[WorldItem],
      refsToExport:  List[RefItem],
      exportPages: RenderPages,
      exportImages: RenderImages,
      localExportPath: String,
      localContentPath: String): (List[String], FileOutputsMap) = {

    logList("page to export", pagesToExport.map(_.id))

    // the images to export are:
    // 1) the refitems in the whole collection whose referenced files were updated (refsToExport)
    // 2) the imageitems whose metadata changed

    val imagePagesToExport = pagesToExport.collect({case x: ImageItem => x})

    logList("ref to export",  refsToExport.map(_.id))
    logList("image to export", imagePagesToExport.map(_.id))

    // there may be overlap between the above two lists, for instance if a tileset file is modified
    // and the description is also updated, so the "distinct" here should prevent duplicate work.
    val imagesToExport = (refsToExport ++ imagePagesToExport).distinct

    val pageOutputs = performExportPages(exportPages, pagesToExport, localExportPath)
    val imageOutputs = performExportImages(exportImages, imagesToExport, localContentPath)

    println("--export finished")

    imageOutputs.foreach{case (k, v) => {
      logList("image created", v.map(k + " -> " + _))
    }}

    (pageOutputs, imageOutputs)
  }


  def performExportPages(
      exportPages: RenderPages,
      pagesToExport: List[WorldItem],
      localExportPath: String): List[String] = {

    val pageOutputs = scala.collection.mutable.Buffer[String]()

    // create master and summary pages that are always exported

    writePage(
      localExportPath / RenderPages.MasterPageFile,
      exportPages.masterPage())
    pageOutputs += RenderPages.MasterPageFile

    // writePage(
    //   localExportPath / RenderPages.TasksPageFile,
    //   exportPages.tasksPage())
    // pageOutputs += RenderPages.TasksPageFile

    // writePage(
    //   localExportPath / RenderPages.IndexPageFile,
    //   exportPages.indexPage())
    // pageOutputs += RenderPages.IndexPageFile

    // writePage(
    //   localExportPath / RenderPages.StatsPageFile,
    //   exportPages.statsPage())
    // pageOutputs += RenderPages.StatsPageFile

    logList("project page created", pageOutputs)

    // create other pages
    pagesToExport.foreach(item => {
      val pageFilename = RenderPages.itemPageName(item)

      writePage(
        localExportPath / pageFilename,
        exportPages.itemPageDispatch(item))

      println("page created: " + pageFilename)

      if (pageFilename.length > 0) {
        pageOutputs += pageFilename
      }
    })

    pageOutputs.toList
  }


  def performExportImages(
      exportImages: RenderImages,
      imagesToExport: List[WorldItem],
      localContentPath: String): FileOutputsMap = {

    // TODO: move file IO here from ExportIamges
    val imageOutputs = if (imagesToExport.nonEmpty) {
      exportImages.exportAllImages(imagesToExport, localContentPath)
    } else {
      RenderImages.emptyFileOutputsMap
    }

    imageOutputs

  }


  // generate stylesheets and download web resources into project web dir
  def addStyles(projConf: ProjectConfig): Unit = {

    val outputDirFile = new File(projConf.localExportPath)

    def download(wr: WebResource): Unit = {
      val hc = wr.url.openConnection().asInstanceOf[HttpURLConnection]
      hc.setRequestProperty("User-Agent", "curl/7.43.0")
      FileUtils.copyInputStreamToFile(
          hc.getInputStream(),
          new File(outputDirFile, wr.localRelFilename))
    }

    def copy(wr: WebResource): Unit = {
      FileUtils.copyURLToFile(wr.url, new File(outputDirFile, wr.localRelFilename))
    }

    // extract a zip archive
    // http://stackoverflow.com/questions/9324933/what-is-a-good-java-library-to-zip-unzip-files
    def extractArchive(archive: File, outputDirname: String): Unit = {

      val zipFile = new ZipFile(archive)
      Try {
        val entries = zipFile.getEntries
        val entriesIterator = Iterator.continually((entries, entries.nextElement)).takeWhile(_._1.hasMoreElements).map(_._2)
        entriesIterator.foreach(entry => {
          val extractedEntry = new File(outputDirname, entry.getName)

          if (entry.isDirectory) {
            extractedEntry.mkdirs
          } else {
            extractedEntry.getParentFile.mkdirs
            val in = zipFile.getInputStream(entry)
            val out = new FileOutputStream(extractedEntry)
            IOUtils.copy(in, out)
            IOUtils.closeQuietly(in)
            out.close()
          }
        })
      }
      zipFile.close()

    }

    // create the output directories

    val stylesDir = new File(outputDirFile, WebResource.StylesRelDirname)
    if (stylesDir.exists) FileUtils.deleteDirectory(stylesDir)
    stylesDir.mkdir()

    val fontsDir = new File(outputDirFile, WebResource.FontsRelDirname)
    if (fontsDir.exists) FileUtils.deleteDirectory(fontsDir)
    fontsDir.mkdir()

    val imagesDir = new File(outputDirFile, WebResource.ImagesRelDirname)
    if (!imagesDir.exists) {
      imagesDir.mkdir()
    }

    val treeDestDir = new File(outputDirFile, WebResource.TreeRelDirname)
    if (treeDestDir.exists) FileUtils.deleteDirectory(treeDestDir)
    treeDestDir.mkdir()

    // generate main stylesheet in styles directory
    val mainCss = Styles.styleSheet
    val fileWriter = new java.io.FileWriter(
        projConf.localExportPath / WebResource.MainStylesheet, false)
    fileWriter.write(mainCss)
    fileWriter.close()

    // download webfonts
    val (fontCss, fontUrls) = Fonts.convert(Styles.FontDescription)
    FileUtils.writeStringToFile(
        new File(outputDirFile, WebResource.FontsStylesheet), fontCss)
    fontUrls.foreach(x => download(WebResource(x._1, WebResource.FontsRelDirname)))

    // copy family tree files from JAR
    copy(WebResource.TreeJs)
    copy(WebResource.TreeCss)

    // copy search files from JAR
    copy(WebResource.SearchJs)
    copy(WebResource.SearchCss)

    // download other stylesheets and scripts used into appropriate directories
    download(WebResource.BootstrapZip)
    extractArchive(
        new File(outputDirFile, WebResource.BootstrapZip.localRelFilename),
        stylesDir.getPath)

    download(WebResource.Jquery)
    download(WebResource.D3)
    download(WebResource.DataTablesJs)
    download(WebResource.DataTablesCss)
    download(WebResource.SortAsc)
    download(WebResource.SortDesc)
    download(WebResource.SortBoth)

  }


  // get all of the local image files referenced by a world
  def getReferencedImages(world: List[WorldItem]): List[String] = {
    // find a unique list of the files pointed to by meta items.
    val uniqueFiles = world.collect({case x: RefItem => x}).map(_.filename).distinct
    // some of these files are not actual local files, but links to remote data.
    // so filter those out
    uniqueFiles.filter(x => !x.startsWith("wikimedia:"))
  }


  // files is names of files relative to local content directory
  def localFileUpdates(
      files: List[String],
      oldFileStatus: FileMap,
      parentDir: String): FileMap = {

    val currentStatus = files.map(x =>
      (x, new File(parentDir / x).lastModified))

    currentStatus.filter({case (k, v) => oldFileStatus.get(k) match {
        case Some(x) => v > x._2
        case None    => true
    }}).map(x => (x._1, ("", x._2))).toMap
  }


  def logList(prefix: String, list: scala.collection.Seq[String]): Unit = {
    list.foreach(x => println(prefix + ": " + x))
  }


  private def writePage(outputFile: String, contents: String): Unit = {
    val fileWriter = new java.io.FileWriter(outputFile, false)
    fileWriter.write(contents)
    fileWriter.close()
  }


}
