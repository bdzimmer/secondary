// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Object with functions for getting metadata and images from Wikimedia.

// 2015-08-22: Created.
// 2015-08-23: Metadata retrieval and image downloads.
// 2015-08-24: Extension awareness in image downloader. JSON parsing fixes.
// 2015-09-02: JSON parsing fixes.

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


  // get a JSON string - None if the query fails
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
    val outputFile = new java.io.File(outputFilename)
    println(wm.url)
    FileUtils.copyURLToFile(new java.net.URL(wm.url), outputFile)
    outputFilename
  }

}
