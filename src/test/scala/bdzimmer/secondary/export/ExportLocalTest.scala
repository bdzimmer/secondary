// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Tests for ContentTransformer.
// Generates a local copy of the Secondary documentation.

// 2015-09-03: Created. This will eventually become a test suite.
// 2015-09-05: Moved to test suite.

package bdzimmer.secondary.export

import org.scalatest.FunSuite

import java.awt.Desktop       // scalastyle:ignore illegal.imports
import java.io.File


class ExportLocalTest extends FunSuite {

  // test local export by building the documentation
  test("local export integration test") {

    val projConf = ProjectConfig("doc/")

    ExportPipelines.exportLocalAll(projConf)
    ExportPipelines.addStyles(projConf)

    val curDir = System.getProperty("user.dir")
    Desktop.getDesktop.browse(new File(s"${curDir}/${projConf.localExportPath}/index.html").toURI)

  }

}

