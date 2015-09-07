// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Tests for ContentTransformer.
// Generates a local copy of the Secondary documentation.

// 2015-09-03: Created. This will eventually become a test suite.
// 2015-09-05: Moved to test suite.

package bdzimmer.secondary.export

import org.apache.commons.io.{FileUtils, FilenameUtils, IOUtils}
import org.apache.commons.compress.archivers.zip.ZipFile
import org.scalatest.FunSuite

import java.awt.Desktop
import java.io.{File, FileOutputStream}
import java.net.URI
import java.util.zip.Deflater

import scala.collection.JavaConverters


class ExportLocalTest extends FunSuite {

  val inputDir = "doc/"
  val outputDir = "doc/export"

  val masterCollectionName = "master"
  val mainCollectionNames = List("doc", "images")
  val license = "Copyright &copy; 2015 Ben Zimmer. All rights reserved."


  test("local export 'integration' test") {

    val outputDirFile = new java.io.File(outputDir)
    FileUtils.deleteDirectory(outputDirFile)
    outputDirFile.mkdirs

    val masterCollection = WorldLoader.loadMasterCollection(
        inputDir,
        masterCollectionName, mainCollectionNames,
        ExportPages.getEmptyFileModifiedMap)

    val world = WorldLoader.collectionToList(masterCollection)

    val exportPages = new ExportPages(world, outputDir, license)
    val allPageOutputs = List(exportPages.createMasterPage(masterCollection),
                              exportPages.createTasksPage,
                              exportPages.createIndexPage) ++
                         exportPages.exportPagesList(world)

    val exportImages = new ExportImages(world, outputDir, license)
    val imageOutputs = exportImages.exportAllImages(world, inputDir)

    val charsToExport = WorldItem.filterList[CharacterItem](world)
    val characterOutputs = if (charsToExport.length > 0) {
       exportImages.prepareCharacterImages(charsToExport, inputDir)
    } else {
      ExportPages.getEmptyFileOutputsMap()
    }

    // if Boostrap doesn't exist in the doc folder, download and extract it
    val extractedBootstrapName = FilenameUtils.removeExtension(PrepareBootstrap.BootstrapFilename)
    val extractedBootstrap = new File(inputDir, extractedBootstrapName)
    if (!extractedBootstrap.exists) {
      PrepareBootstrap.getBootstrap(inputDir)
    }

    // copy bootstrap into export directory and rename
    FileUtils.copyDirectoryToDirectory(extractedBootstrap, outputDirFile)
    FileUtils.moveDirectory(
        new File(outputDirFile, extractedBootstrapName),
        new File(outputDirFile, "bootstrap"))

    // TODO: generate secondary.css
    // copy secondary.css into export directory
    FileUtils.copyFileToDirectory(new File(inputDir, "secondary.css"), outputDirFile)

    val curDir = System.getProperty("user.dir").replace('\\', '/')
    Desktop.getDesktop.browse(new URI(s"${curDir}/${outputDir}/index.html"))

  }

}



object PrepareBootstrap {

  val BootstrapFilename = "bootstrap-3.3.5-dist.zip"
  val BootstrapUrl = "https://github.com/twbs/bootstrap/releases/download/v3.3.5/" + BootstrapFilename

  // download boostrap archive, extract, and delete
  def getBootstrap(downloadDir: String): Unit = {

    val outputFilename = downloadDir + "/" + BootstrapFilename
    val outputFile = new java.io.File(outputFilename)

    FileUtils.copyURLToFile(new java.net.URL(BootstrapUrl), outputFile)
    extractArchive(outputFile, downloadDir)
    outputFile.delete

  }


  // extract a zip archive
  // http://stackoverflow.com/questions/9324933/what-is-a-good-java-library-to-zip-unzip-files
  def extractArchive(archive: File, outputDirname: String) {

    // TODO: idiomatic exception handling
    val zipFile = new ZipFile(archive)
    try {
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
          out.close
        }
      })

    } finally {
      zipFile.close
    }
  }


}
