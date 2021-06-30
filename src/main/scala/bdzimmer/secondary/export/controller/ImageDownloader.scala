// Copyright (c) 2018 Ben Zimmer. All rights reserved.

// Object with functions for getting metadata and images from Wikimedia.

package bdzimmer.secondary.export.controller

import java.awt.image.BufferedImage  // scalastyle:ignore illegal.imports
import java.awt.Image                // scalastyle:ignore illegal.imports
import java.io.File
import java.net.URL
import javax.imageio.ImageIO

import scala.util.Try

import com.fasterxml.jackson.core.{JsonFactory, JsonParser, JsonToken}

import org.apache.commons.io.{FileUtils, FilenameUtils, IOUtils}

import bdzimmer.util.StringUtils._



case class WikimediaMeta(
    title: String,
    width: Int,
    height: Int,
    url: String,
    descriptionurl: String,

    artist: String,
    license: String,
    attribution: Boolean,
    copyrighted: Boolean,
    restrictions: String
)


object ImageDownloader {

  val WikipediaURL = "https://en.wikipedia.org"
  val MetaDataQuery = "/w/api.php?action=query&prop=imageinfo&format=json&iiprop=url%7Cdimensions%7Cmime%7Cextmetadata&titles="

  val MaxWidth = 1920

  // get a JSON string - None if the query fails
  def getWikimediaJson(filename: String): Option[String] = {

    // I think we can always assume a "File:" prefix
    val queryString = WikipediaURL + MetaDataQuery + "File:" + filename
    println("reading wikimedia json\n\t" + queryString)
    val result = Try({
      val is = new URL(queryString).openStream
      val json = IOUtils.toString(is)
      IOUtils.closeQuietly(is)
      json
    })
    if (result.isFailure) {
      println("read failed")
    }
    result.toOption

  }


  def parseWikimediaJson(json: String): Option[WikimediaMeta] = {

    val parser = new JsonFactory().createJsonParser(json);

    var title          = ""
    var width          = 0
    var height         = 0
    var url            = ""
    var descriptionurl = ""
    var artist         = ""
    var license        = ""
    var attribution    = true
    var copyrighted    = true
    var restrictions   = ""

    var foundPage      = false

    try {

      parser.nextToken()
      while(!parser.isClosed && parser.nextToken() != JsonToken.END_OBJECT) {
        if ("query".equals(parser.getCurrentName())) {              // "query":

          parser.nextToken()                                        // {
          while(!parser.isClosed && parser.nextToken() != JsonToken.END_OBJECT) {
            if ("pages".equals(parser.getCurrentName())) {          // "pages":

              parser.nextToken()                                    // {
              while(!parser.isClosed && parser.nextToken() != JsonToken.END_OBJECT) {
                // println("page: " + parser.getCurrentName() + " " + parser.getValueAsString(""))
                if (!foundPage && parser.getCurrentName() != null) {  // only process the first one that has an imginfo with url

                  parser.nextToken()                                // {
                  while(!parser.isClosed && parser.nextToken() != JsonToken.END_OBJECT) {
                    // println("page token: " + parser.getCurrentName() + " " + parser.getValueAsString(""))
                    if ("title".equals(parser.getCurrentName())) {  // "title":
                      parser.nextToken()
                      title = parser.getValueAsString("")

                    } else if ("imageinfo".equals(parser.getCurrentName())) { // "imageinfo":

                      parser.nextToken()    // [
                      var foundImageinfo = false
                      while(!parser.isClosed && parser.nextToken() != JsonToken.END_ARRAY) {
                        // it's an array, and we only care about the first element
                        if (!foundImageinfo && parser.getCurrentToken().equals(JsonToken.START_OBJECT)) { // {
                          foundImageinfo = true

                          while(!parser.isClosed && parser.nextToken() != JsonToken.END_OBJECT) {
                            // println("page imageinfo token: " + parser.getCurrentName() + " " + parser.getValueAsString(""))
                            if ("width".equals(parser.getCurrentName())) {
                              parser.nextToken()
                              width = parser.getIntValue()
                            } else if ("height".equals(parser.getCurrentName())) {
                              parser.nextToken()
                              height = parser.getIntValue()
                            } else if ("url".equals(parser.getCurrentName())) {
                              parser.nextToken()
                              url = parser.getValueAsString("")
                              foundPage = true
                            } else if ("descriptionurl".equals(parser.getCurrentName())) {
                              parser.nextToken()
                              descriptionurl = parser.getValueAsString("")
                            } else if ("extmetadata".equals(parser.getCurrentName())) {

                              parser.nextToken()   // {
                              while(!parser.isClosed && parser.nextToken() != JsonToken.END_OBJECT) {
                                // println("page imageinfo extmetadata token: " + parser.getCurrentName() + " " + parser.getValueAsString(""))
                                if ("Artist".equals(parser.getCurrentName())) {
                                  parser.nextToken()
                                  artist = impureGetSingle(parser, "value")
                                } else if ("LicenseShortName".equals(parser.getCurrentName())) {
                                  parser.nextToken()
                                  license = impureGetSingle(parser, "value")
                                } else if ("AttributionRequired".equals(parser.getCurrentName())) {
                                  parser.nextToken()
                                  attribution = impureGetSingle(parser, "value", "true").toBoolean
                                } else if ("Copyrighted".equals(parser.getCurrentName())) {
                                  parser.nextToken()
                                  copyrighted = impureGetSingle(parser, "value", "true").toBoolean
                                } else if ("Restrictions".equals(parser.getCurrentName())) {
                                  parser.nextToken()
                                  restrictions = impureGetSingle(parser, "value")
                                } else {
                                  parser.skipChildren()
                                }
                              }
                            } else {
                              parser.skipChildren()
                            }
                          }
                        } else {
                          parser.skipChildren()
                        }
                      }
                    } else {
                      parser.skipChildren()
                    }
                  }
                } else {
                  parser.skipChildren()
                }
              }
            } else {
              parser.skipChildren()
            }
          }
        } else {
          parser.skipChildren()  // skip potential subobjects
        }
      }

    } catch {
      case _: Throwable => // do nothing
    }

    if (foundPage) {
      Some(WikimediaMeta(
        title, width, height, url, descriptionurl,
        artist, license, attribution, copyrighted, restrictions))
    } else {
      None
    }
  }


  // parser is pointing to JsonToken.START_OBJECT
  private def impureGetSingle(parser: JsonParser, key: String, default: String = ""): String = {
    var v = default
    while(!parser.isClosed && parser.nextToken() != JsonToken.END_OBJECT) {
      if (key.equals(parser.getCurrentName())) {
        parser.nextToken()
        v = parser.getValueAsString("")
      } else {
        parser.skipChildren()  // skip potential subobjects
      }
    }
    v
  }


  // download the image referenced by a metadata object
  def downloadImage(wm: WikimediaMeta, outputFilename: String): Option[String] = {
    // val outputExtension = FilenameUtils.getExtension(wm.url)
    // val outputFilename = outputName + "." + outputExtension
    val outputFile = new File(outputFilename)
    println("downloading\n\t" + wm.url + "\n\t" + outputFilename)

    val download = Try(FileUtils.copyURLToFile(new java.net.URL(wm.url), outputFile))
    if (download.isFailure) {
      println("download failed")
    }
    download.toOption.map(_ => outputFilename)
  }


  // read an image, downsize, save
  def copyAndDownsizeImage(inputImage: String, outputImage: String, ext: String, maxWidth: Int = MaxWidth): Unit = {
    Try {
      val inputImageFile = new File(inputImage)
      val img = ImageIO.read(inputImageFile)

      if (img.getWidth > maxWidth) {
        println("downsizing\n\t" + inputImage + "\n\t" + outputImage)
        val scaledHeight = img.getHeight * (maxWidth / img.getWidth.toFloat)
        val scaledImage = img.getScaledInstance(maxWidth, scaledHeight.toInt, Image.SCALE_SMOOTH)
        val scaledBufferedImage = new BufferedImage(
            scaledImage.getWidth(null),
            scaledImage.getHeight(null),
            BufferedImage.TYPE_INT_RGB)
        scaledBufferedImage.getGraphics.drawImage(scaledImage, 0, 0, null)
        ImageIO.write(scaledBufferedImage, ext, new File(outputImage))
      } else {
        println("copying\n\t" + inputImage + "\n\t" + outputImage)
        if (!inputImage.equals(outputImage)) FileUtils.copyFile(inputImageFile, new File(outputImage))
      }
    }
  }

}



class FilesystemCache(val dir: String, suffix: String, process: String => Option[String]) {
  // cache strings to files in the file system
  // probably not threadsafe

  val dirFile = new File(dir)
  if (!dirFile.exists) {
    println("creating filesystem cache (" + suffix + "): " + dir)
    dirFile.mkdirs()
  }

  def apply(key: String): Option[String] = {
    val file = new File(dir / key + suffix)
    if (file.exists) {
      Some(FileUtils.readFileToString(file))
    } else {
      process(key).map(x => {
        FileUtils.writeStringToFile(file, x)
        x
      })
    }
  }

}
