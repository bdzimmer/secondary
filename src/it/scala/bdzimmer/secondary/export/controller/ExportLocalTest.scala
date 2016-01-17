// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Tests for ContentTransformer.
// Generates a local copy of the Secondary documentation.

// 2015-09-03: Created. This will eventually become a test suite.
// 2015-09-05: Moved to test suite.

package bdzimmer.secondary.export.controller

import org.scalatest.FunSuite

import java.awt.Desktop       // scalastyle:ignore illegal.imports
import java.io.File

import scala.util.Try

import bdzimmer.secondary.export.model.ProjectConfig
import bdzimmer.util.StringUtils._

class ExportLocalTest extends FunSuite {

  // test local export by building the documentation
  test("local export integration test") {

    val projectDir = System.getProperty("user.dir") / "doc"
    val projConf = ProjectConfig(projectDir)

    ExportPipeline.exportAll(projConf)
    ExportPipeline.addStyles(projConf)

    val curDir = System.getProperty("user.dir")
    Try {
      Desktop.getDesktop.browse(
          new File(projConf.localExportPath / "index.html").toURI)
    }

  }

}

