// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Object with functions for getting metadata and images from Wikimedia.

// 2015-08-22: Created.
// 2015-08-23: Metadata retrieval and image downloads.
// 2015-08-24: Extension awareness in image downloader. JSON parsing fixes.

package bdzimmer.secondary.export

import java.net.URL

import org.apache.commons.io.{FileUtils, FilenameUtils, IOUtils}
import scala.util.Try

import net.liftweb.json.JsonParser
import net.liftweb.json.JsonParser._
import net.liftweb.json.JsonAST._


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

  val wikipediaURL = "https://en.wikipedia.org"
  val metaDataQuery = "/w/api.php?action=query&prop=imageinfo&format=json&iiprop=url%7Cdimensions%7Cmime%7Cextmetadata&titles="

  // get a JSON string - none if the query fails
  def getWikimediaJson(filename: String): Option[String] = {

    // I think we can always assume a "File:" prefix
    val queryString = wikipediaURL + metaDataQuery + "File:" + filename
    // println(queryString)
    val is = new URL(queryString).openStream
    val result = Try(IOUtils.toString(is))
    IOUtils.closeQuietly(is)
    result.toOption

  }


  // transform JSON into a metadata object
  def parseWikimediaJson(json: String): WikimediaMeta = {

    val root = JsonParser.parse(json)

    def extractStr(jval: JValue, default: String = ""): String = jval match {
      case JString(x) => x
      case _ => default
    }

    def extractInt(jval: JValue, default: Int = 0): Int = jval match {
      case JInt(x) => x.toInt
      case _ => default
    }

    val main = root \ "query" \ "pages" \ "-1"
    val imageinfo = main \ "imageinfo"
    val extmeta = imageinfo \ "extmetadata"

    def metaValue(key: String): JValue = {
      extmeta \ key \ "value"
    }

    // TODO: get description (title is kind of useless)
    WikimediaMeta(
      title = extractStr(main \ "title"),
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
    val outputFile = new java.io.File(outputFilename)
    FileUtils.copyURLToFile(new java.net.URL(wm.url), outputFile)
    outputFilename
  }

}
