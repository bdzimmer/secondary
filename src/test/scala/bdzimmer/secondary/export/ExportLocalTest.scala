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


class ExportLocalTest extends FunSuite {

  // test local export by building the documentation
  test("local export integration test") {

    val projConf = new DriverConfig("doc/")

    ContentTransformer.exportLocal(projConf)

    val outputDirFile = new File(projConf.localExportPath)

    // if Bootsrap doesn't exist in the doc folder, download and extract it
    val extractedBootstrapName = FilenameUtils.removeExtension(Styles.BootstrapFilename)
    val extractedBootstrap = new File(projConf.localContentPath, extractedBootstrapName)
    if (!extractedBootstrap.exists) {
      Styles.getBootstrap(projConf.localContentPath)
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

    val curDir = System.getProperty("user.dir")
    Desktop.getDesktop.browse(new File(s"${curDir}/${projConf.localExportPath}/index.html").toURI)


  }

}

