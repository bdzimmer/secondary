// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Functions for managing style sheets for an export.

// 2015-09-07: Created.

// scalastyle:off magic.number

package bdzimmer.secondary.export.view

import org.apache.commons.io.{FileUtils, IOUtils}
import org.apache.commons.compress.archivers.zip.ZipFile

import java.io.{File, FileOutputStream}
import scala.util.Try


object Styles {

  // val FontFace = "Lora"
  val FontFace = "Libre Baskerville"
  val FontFallback = "serif"
  val HeadingSizes = (1 to 6) zip List(42, 36, 30, 24, 18, 14)
  val H1JumbotronSize = 72
  val BodyFontSize = 16
  val BodyLineHeight = 2
  val BodyMarginBottom = 20


  val BootstrapFilename = "bootstrap-3.3.5-dist.zip"
  val BootstrapUrl = "https://github.com/twbs/bootstrap/releases/download/v3.3.5/" + BootstrapFilename

  // TODO: separate collapsible list stuff into a constant
  def createStyleSheet(outputFile: String): Unit = {

    val headingSizeStyle = HeadingSizes.map({case (level, size) => {
      s"""h${level}, .h${level} {font-size: ${size}px}"""
    }}).mkString("\n")


    val sheetText = s"""/* Copyright (c) 2015 Ben Zimmer. All rights reserved. */

/* Set a custom font and increase the font size for everything. */

h1, h2, h3, h4, h5, h6, .h1, .h2, .h3, .h4, .h5, .h6 {
  font-family: '${FontFace}', ${FontFallback};
}

${headingSizeStyle}

.jumbotron h1, .jumbotron h1 {
  font-size: ${H1JumbotronSize}px;
}

p, div {
  font-family: '${FontFace}', ${FontFallback};
  font-size: ${BodyFontSize}px;
}

p {
  margin: 0 0 ${BodyMarginBottom}px;
}

body {
  line-height: ${BodyLineHeight};
}
""" +
"""
/* swiveling arrow checkboxes and collapsible lists */

input.swivel[type=checkbox] { display:none; }
input.swivel[type=checkbox] ~ label:after {
  font-family: 'Glyphicons Halflings';
  font-size: 12px;
  display: inline-block;
  margin-left: 10px;
}
input.swivel[type=checkbox] ~ label:after { content: "\e258"; }
input.swivel[type=checkbox]:checked ~ label:after { content: "\e259"; }

li.swivel > input ~ ul { display: none; }
li.swivel > input:checked ~ ul { display: block; }
li.swivel label { margin-bottom: 0px; }
"""

    val fileWriter = new java.io.FileWriter(outputFile, false)
    fileWriter.write(sheetText)
    fileWriter.close

  }



  // download boostrap archive, extract, and delete
  def getBootstrap(downloadDir: String): Unit = {

    val outputFilename = downloadDir + "/" + BootstrapFilename
    val outputFile = new File(outputFilename)

    FileUtils.copyURLToFile(new java.net.URL(BootstrapUrl), outputFile)
    extractArchive(outputFile, downloadDir)
    outputFile.delete

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
          out.close
        }
      })
    }
    zipFile.close

  }


}
