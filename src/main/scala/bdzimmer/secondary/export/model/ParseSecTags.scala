// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Functionality for parsing markdown and special Secondary tags.

// 2015-08-30: Created. Link and general tags.
// 2015-09-01: Image tags.
// 2015-09-02: Tags for jumbotron background image and text color.

package bdzimmer.secondary.export.model

import org.pegdown.PegDownProcessor
import scala.util.matching.Regex
import org.apache.commons.io.FileUtils

case class SecTag(kind: String, value: String)



object ParseSecTags {

  val matcher = "\\{\\{(.*?)\\}\\}".r

  // upper camelcase looks wierd, but I guess it's convention
  // http://docs.scala-lang.org/style/naming-conventions.html

  val LinkKind = "link"
  val ImageKind = "image"
  val ImageResponsiveKind = "image-responsive"
  val JumbotronBackgroundKind = "jumbotron-bg"

  val EmbedPreKind = "embed-pre"
  val JumbotronForegroundKind = "jumbotron-fg"
  val TodoKind = "todo"
  val ThoughtKind = "thought"

  val itemTagKinds = List(LinkKind, ImageKind, ImageResponsiveKind, JumbotronBackgroundKind)
  val otherTagKinds = List(EmbedPreKind, JumbotronForegroundKind, TodoKind, ThoughtKind)


  def getAllTags(text: String): List[SecTag] = {
    ParseSecTags.matcher.findAllMatchIn(text).map(m => {
      getTag(m.group(1))
    }).toList
  }


  // generate a tag from text
  def getTag(tagText: String): SecTag = {

    // println("\tgetTag")
    // TODO: find a way to return error messages
    // println(s"\t\t$tagText")

    tagText.contains(":") match {
      case true => {
        val tagParts = tagText.split(":\\s+")
        SecTag(tagParts(0).toLowerCase, tagParts(1))
      }
      case false => SecTag("link", tagText)
    }

  }


  // process a line of text (like a description or title) with PegDown
  // eliminating beginning and ending paragraph tags
  def processLine(line: String): String = {
    // TODO: not sure if creating the pegdown processor every time here
    // will be performant or not.
    val pp = getPegDown
    val html = pp.markdownToHtml(line)
    html.stripPrefix("<p>").stripSuffix("</p>")
  }


  def getPegDown(): PegDownProcessor = {
    // new PegDownProcessor(Extensions.HARDWRAPS)
    new PegDownProcessor
  }


}
