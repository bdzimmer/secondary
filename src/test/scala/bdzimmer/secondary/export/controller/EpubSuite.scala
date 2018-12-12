// Copyright (c) 2018 Ben Zimmer. All rights reserved.

// Tests for WorldLoader.

package bdzimmer.secondary.export.controller

import org.scalatest.FunSuite

import java.io.File


class EpubSuite extends FunSuite{

  test("export") {

     val outputFilename = "test.epub"
     val outputFile = new File(outputFilename)

     if (outputFile.exists) {
       outputFile.delete()
     }

     Epub.export(
       outputFilename,
       "0000",
       "Test Book",
       "B",
       "Z",
       List(
         Epub.SectionInfo("0", "Title Page", "<body>Test Book<br>B Z</body>"),
         Epub.SectionInfo("1", "Chapter 1", "<body>This is the content of chapter 1.</body>")
       ),
       None,
       ""
     )

    assert(outputFile.exists())

  }

}
