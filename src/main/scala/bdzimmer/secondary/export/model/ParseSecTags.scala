// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Functionality for parsing markdown and special Secondary tags.

// 2015-08-30: Created. Link and general tags.
// 2015-09-01: Image tags.
// 2015-09-02: Tags for jumbotron background image and text color.
// 2015-11-06: Family tree tags.

package bdzimmer.secondary.export.model

import scala.util.matching.Regex
import org.apache.commons.io.FileUtils

case class SecTag(kind: String, value: String)


object ParseSecTags {

  val matcher = "\\{\\{(.*?)\\}\\}".r

  // upper CamelCase looks weird, but it's convention for constants
  // http://docs.scala-lang.org/style/naming-conventions.html

  val Link = "link"
  val Image = "image"
  val ImageResponsive = "image-responsive"
  val FamilyTree = "familytree"
  val Jumbotron = "jumbotron"

  val EmbedPre = "embed-pre"
  val Todo = "todo"
  val Thought = "thought"

  val ItemTagKinds = List(Link, Image, ImageResponsive, FamilyTree, Jumbotron)
  val OtherTagKinds = List(EmbedPre, Todo, Thought)


  def getAllTags(text: String): List[SecTag] = {
    ParseSecTags.matcher.findAllMatchIn(text).map(m => {
      getTag(m.group(1))
    }).toList
  }


  // generate a tag from text
  def getTag(tagText: String): SecTag = {

    tagText.contains(":") match {
      case true => {
        val tagParts = tagText.split(":\\s+")
        SecTag(tagParts(0).toLowerCase, tagParts(1))
      }
      case false => SecTag("link", tagText)
    }

  }


}
