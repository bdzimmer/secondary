// Copyright (c) 2018 Ben Zimmer. All rights reserved.

// Tests for ImageDownloader.


package bdzimmer.secondary.export.controller

import org.scalatest.FunSuite

import org.apache.commons.io.FileUtils
import java.io.File

import bdzimmer.util.TempDirectory
import bdzimmer.util.StringUtils._


class ImageDownloaderSuite extends FunSuite with TempDirectory {

  // val inputFile = "Mars_Hubble.jpg"
  // val inputFile = "Arthur_Rackham_Little_Red_Riding_Hood%2B.jpg"
  val inputFile = "NGC_4414_(NASA-med).jpg"

  test("get metadata") {

    val resultJson = ImageDownloader.getWikimediaJson(inputFile)
    assert(resultJson.isDefined)

    // for manual examination of JSON
    val jsonFile = new File(tempDirname / "json.txt")
    resultJson.foreach(x => FileUtils.writeStringToFile(jsonFile, x, "UTF-8"))

    val meta = resultJson.flatMap(ImageDownloader.parseWikimediaJson)
    assert(meta.isDefined)

  }


  test("download image") {

    val outputFilename = tempDirname / "output.jpg"
    val outputFile = new File(outputFilename)

    val resultFilename = for {
      json <- ImageDownloader.getWikimediaJson(inputFile)
      wm   <- ImageDownloader.parseWikimediaJson(json)
      name <- ImageDownloader.downloadImage(wm, outputFilename)
    } yield name

    assert(resultFilename.isDefined)
    assert(outputFile.exists())

    val downsizedFilename = tempDirname / "downsized.jpg"
    val downsizedFile = new File(downsizedFilename)

    resultFilename.foreach(x => ImageDownloader.downsizeImage(
        outputFilename, downsizedFilename, "jpg", 800))
    assert(downsizedFile.exists())

  }


  test("parse bad JSON") {
    val meta = ImageDownloader.parseWikimediaJson("""{"baloney": 0}""")
    assert(meta.isEmpty)
  }



}
