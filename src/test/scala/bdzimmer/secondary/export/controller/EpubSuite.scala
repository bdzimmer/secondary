// Copyright (c) 2018 Ben Zimmer. All rights reserved.

// Tests for WorldLoader.

package bdzimmer.secondary.export.controller

import org.scalatest.FunSuite
import java.io.File

import bdzimmer.util.TempDirectory
import bdzimmer.util.StringUtils._


class EpubSuite extends FunSuite with TempDirectory {

  test("export") {

     val outputFilename = tempDirname / "test.epub"
     val outputFile = new File(outputFilename)

     Epub.export(
       outputFilename,
       "0000",
       "Test Book",
       Some(("B", "Z")),
       Some("C Z"),
       List(
         Book.SectionInfo("0", "Title Page", None, None, "<body>Test Book<br>D Z</body>"),
         Book.SectionInfo("1", "Chapter 1", None, None, "<body>This is the content of chapter 1.</body>")
       ),
       None,
       "",
       Set(),
       None
     )

    assert(outputFile.exists())

  }

}
