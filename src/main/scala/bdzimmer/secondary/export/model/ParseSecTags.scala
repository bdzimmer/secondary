// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Functionality for parsing markdown and special Secondary tags.

// 2015-08-30: Created. Link and general tags.
// 2015-09-01: Image tags.
// 2015-09-02: Tags for jumbotron background image and text color.

package bdzimmer.secondary.export.model

import scala.util.matching.Regex
import org.apache.commons.io.FileUtils

case class SecTag(kind: String, value: String)


object ParseSecTags {

  val matcher = "\\{\\{(.*?)\\}\\}".r

  // upper camelcase looks wierd, but I guess it's convention
  // http://docs.scala-lang.org/style/naming-conventions.html

  val Link = "link"
  val Image = "image"
  val ImageResponsive = "image-responsive"
  val JumbotronBackground = "jumbotron-bg"

  val EmbedPre = "embed-pre"
  val JumbotronForeground = "jumbotron-fg"
  val Todo = "todo"
  val Thought = "thought"

  val ItemTagKinds = List(Link, Image, ImageResponsive, JumbotronBackground)
  val OtherTagKinds = List(EmbedPre, JumbotronForeground, Todo, Thought)


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


}
