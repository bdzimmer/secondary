// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Object with functions for getting metadata and images from Wikimedia.

// 2015-08-22: Created.
// 2015-08-23: Metadata retrieval and image downloads.

package bdzimmer.secondary.export

import java.net.URL

import org.apache.commons.io.IOUtils
import org.apache.commons.io.FileUtils
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

    val queryString = wikipediaURL + metaDataQuery + filename
    println(queryString)
    val is = new URL(queryString).openStream
    val result = Try(IOUtils.toString(is))
    IOUtils.closeQuietly(is)
    result.toOption

  }


  // transform JSON into a metadata object
  def parseWikimediaJson(json: String): WikimediaMeta = {

    val root = JsonParser.parse(json)

    // there's probably a better way to do this
    def extractStr(jval: JValue, default: String = ""): String = {
      Try({
        val JString(str) = jval
        str
      }).toOption.getOrElse(default)
    }

    def extractInt(jval: JValue, default: Int = 0): Int = {
      Try({
        val JInt(num) = jval
        num.toInt
      }).toOption.getOrElse(default)
    }

    val main = root \ "query" \ "pages" \ "-1"
    val imageinfo = main \ "imageinfo"
    val extmeta = imageinfo \ "extmetadata"

    def metaValue(key: String): JValue = {
      extmeta \ key \ "value"
    }

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


  def downloadImage(wm: WikimediaMeta, outputFilename: String): String = {

    // TODO: outputFilename should be a base name without extension
    // the resulting file should use the extension from the meta

    val outputFile = new java.io.File(outputFilename)
    FileUtils.copyURLToFile(new java.net.URL(wm.url), outputFile)
    outputFilename
  }

}
