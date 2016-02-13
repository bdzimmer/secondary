// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Object with functions for getting metadata and images from Wikimedia.

// 2015-08-22: Created.
// 2015-08-23: Metadata retrieval and image downloads.
// 2015-08-24: Extension awareness in image downloader. JSON parsing fixes.
// 2015-09-02: JSON parsing fixes.

package bdzimmer.secondary.export.controller

import java.awt.image.BufferedImage  // scalastyle:ignore illegal.imports
import java.awt.Image                // scalastyle:ignore illegal.imports
import java.io.File
import java.net.URL
import javax.imageio.ImageIO

import scala.util.Try

import net.liftweb.json.JsonParser
import net.liftweb.json.JsonParser._
import net.liftweb.json.JsonAST._
import org.apache.commons.io.{FileUtils, FilenameUtils, IOUtils}


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
    // println(queryString)
    val is = new URL(queryString).openStream
    val result = Try(IOUtils.toString(is))
    IOUtils.closeQuietly(is)
    result.toOption

  }


  // transform JSON into a metadata object
  // return None if it doesn't make sense to parse the JSON
  // into metadata
  def parseWikimediaJson(json: String): Option[WikimediaMeta] = {

    val root = JsonParser.parse(json)

    for { // Option monad

      // get first page
      page <- (for {
        JField("pages", JObject(pages)) <- root
        page <- pages
      } yield page).headOption


      // if page is present with imageinfo and a url, then
      // attempt tp parse
      result <- (page \ "imageinfo" \ "url") match {
        case x: JString => Some(parsePage(page))
        case _ => None
      }

    } yield result

  }


  // helper function - parse a page
  private def parsePage(page: JField): WikimediaMeta = {

    def extractStr(jval: JValue, default: String = ""): String = jval match {
      case JString(x) => x
      case _ => default
    }

    def extractInt(jval: JValue, default: Int = 0): Int = jval match {
      case JInt(x) => x.toInt
      case _ => default
    }

    val imageinfo = page \ "imageinfo"
    val extmeta = imageinfo \ "extmetadata"
    def metaValue(key: String): JValue = {
      extmeta \ key \ "value"
    }

    WikimediaMeta(
      title = extractStr(page \ "title"),
      width = extractInt(imageinfo \ "width").toInt,
      height = extractInt(imageinfo \ "height").toInt,
      url = extractStr(imageinfo \ "url"),
      descriptionurl = extractStr(imageinfo \ "descriptionurl"),

      artist = extractStr(metaValue("Artist")),
      license = extractStr(metaValue("LicenseShortName")),
      attribution = extractStr(metaValue("AttributionRequired"), "true").toBoolean,
      copyrighted = extractStr(metaValue("Copyrighted"), "true").toBoolean,
      restrictions = extractStr(metaValue("Restrictions"))
    )
  }


  // download the image referenced by a metadata object
  def downloadImage(wm: WikimediaMeta, outputName: String): String = {
    val outputExtension = FilenameUtils.getExtension(wm.url)
    val outputFilename = outputName + "." + outputExtension
    val outputFile = new File(outputFilename)
    println(wm.url)
    FileUtils.copyURLToFile(new java.net.URL(wm.url), outputFile)
    outputFilename
  }


  // read an image, downsize, save
  def downsizeImage(inputImage: String, outputImage: String, ext: String, maxWidth: Int = MaxWidth): Unit = {
    Try {
      val inputImageFile = new File(inputImage)
      val img = ImageIO.read(inputImageFile)

      if (img.getWidth > maxWidth) {
        println("downsizing " + inputImage + " to " + outputImage)
        val scaledHeight = img.getHeight * (maxWidth / img.getWidth.toFloat)
        val scaledImage = img.getScaledInstance(maxWidth, scaledHeight.toInt, Image.SCALE_SMOOTH)
        val scaledBufferedImage = new BufferedImage(
            scaledImage.getWidth(null),
            scaledImage.getHeight(null),
            BufferedImage.TYPE_INT_RGB)
        scaledBufferedImage.getGraphics.drawImage(scaledImage, 0, 0, null)
        ImageIO.write(scaledBufferedImage, ext, new File(outputImage))
      } else {
        if (!inputImage.equals(outputImage)) FileUtils.copyFile(inputImageFile, new File(outputImage))
      }
    }
  }

}
