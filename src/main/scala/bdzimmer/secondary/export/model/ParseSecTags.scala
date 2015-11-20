// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Functionality for parsing markdown and special Secondary tags.

// 2015-08-30: Created. Link and general tags.
// 2015-09-01: Image tags.
// 2015-09-02: Tags for jumbotron background image and text color.
// 2015-11-06: Family tree tags.
// 2015-11-09: Added list of args to SecTags.

package bdzimmer.secondary.export.model

import scala.util.matching.Regex
import org.apache.commons.io.FileUtils


case class SecTag(kind: String, value: String, args: List[String])


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

    tagText.split(":\\s+").toList match {
      case kind :: text :: extra => text.split("\\s*\\|\\s*").toList match {
        case value :: argsList :: extra => {
          SecTag(kind.toLowerCase(), value, argsList.split("\\s+").toList)
        }
        case _ => SecTag(kind.toLowerCase(), text, List())
      }
      case _ => SecTag("link", tagText, List())
    }

  }

}
