// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Tests for ContentTransformer.
// Generates a local copy of the Secondary documentation.

// 2015-09-03: Created. This will eventually become a test suite.
// 2015-09-05: Moved to test suite.

package bdzimmer.secondary.export

import org.scalatest.FunSuite

import org.apache.commons.io.{FileUtils, FilenameUtils}
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

    val outputDirFile = new File(outputDir)
    FileUtils.deleteDirectory(outputDirFile)
    outputDirFile.mkdirs

    val world = WorldLoader.loadWorld(
        inputDir,
        masterCollectionName, mainCollectionNames,
        ExportPages.getEmptyFileModifiedMap)

    // val world = WorldLoader.collectionToList(masterCollection)

    val exportPages = new ExportPages(world, outputDir, license)
    val allPageOutputs = List(exportPages.createMasterPage,
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
    val extractedBootstrapName = FilenameUtils.removeExtension(Styles.BootstrapFilename)
    val extractedBootstrap = new File(inputDir, extractedBootstrapName)
    if (!extractedBootstrap.exists) {
      Styles.getBootstrap(inputDir)
    }

    // copy bootstrap into styles directory in export directory and rename
    val stylesDir = new File(outputDirFile, "styles")
    stylesDir.mkdir
    FileUtils.copyDirectoryToDirectory(extractedBootstrap, stylesDir)
    FileUtils.moveDirectory(
        new File(stylesDir, extractedBootstrapName),
        new File(stylesDir, "bootstrap"))


    // generate secondary.css in styles directory
    Styles.createStyleSheet(outputDir + "/styles/" + "secondary.css")

    val curDir = System.getProperty("user.dir").replace('\\', '/')
    Desktop.getDesktop.browse(new URI(s"${curDir}/${outputDir}/index.html"))

  }

}

