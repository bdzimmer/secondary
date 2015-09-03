// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Tests for ImageDownloader.

// 2015-08-22: Created.
// 2015-08-23: Tests for getting metadata and downloading an image file.
// 2015-08-24: Revised tests.
// 2015-09-02: Updates for JSON parsing fixes.

package bdzimmer.secondary.export

import org.scalatest.FunSuite

import org.apache.commons.io.{FileUtils, FilenameUtils}
import java.io.File
import java.nio.charset.StandardCharsets


class ImageDownloaderSuite extends FunSuite {

  // val inputFile = "Mars_Hubble.jpg"
  // val inputFile = "Arthur_Rackham_Little_Red_Riding_Hood%2B.jpg"
  val inputFile = "NGC_4414_(NASA-med).jpg"

  test("get metadata") {

    val resultJson = ImageDownloader.getWikimediaJson(inputFile)
    assert(resultJson.isDefined)

    // for manual examination of JSON
    // resultJson.map(x => FileUtils.writeStringToFile(new File("json.txt"), x, StandardCharsets.UTF_8))

    val meta = resultJson.flatMap(ImageDownloader.parseWikimediaJson(_))
    assert(meta.isDefined)

    // meta foreach println

  }


  test("download image") {

    val outputName = "output"

    val outputFilename = for {
      json <- ImageDownloader.getWikimediaJson(inputFile)
      wm <- ImageDownloader.parseWikimediaJson(json)
    } yield {
      ImageDownloader.downloadImage(wm, outputName)
    }

    assert(outputFilename.isDefined)
    outputFilename.map(x => assert(FilenameUtils.getBaseName(x).equals(outputName)))
    outputFilename.map(x => new java.io.File(outputName).exists)

  }


  test("parse bad JSON") {

    val meta = ImageDownloader.parseWikimediaJson("""{"baloney": 0}""")
    assert(meta.isEmpty)

  }



}
