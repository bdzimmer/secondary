// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Functionality for parsing markdown and special Secondary tags.

// 2015-08-30: Created.

package bdzimmer.secondary.export

import org.pegdown.PegDownProcessor
import scala.util.matching.Regex
import org.apache.commons.io.FileUtils


case class SecTag(kind: String, id: String)


class NotesParser(world: List[WorldItem]) {

  def getPegDown(): PegDownProcessor = {
    // new PegDownProcessor(Extensions.HARDWRAPS)
    new PegDownProcessor
  }


  // transform markdown text with special tags to HTML
  def transform(text: String): String = {

    // process special tags
    val updatedText = NotesParser.matcher.replaceAllIn(text, m => {
      val tag = getTag(m.group(1))
      processTag(tag)
    })

    val pp = getPegDown
    pp.markdownToHtml(updatedText)
  }


  // generate a tag from text
  def getTag(tagText: String): SecTag = {

    // TODO: find a way to return error messages
    println(s"\t\t$tagText")

    tagText.contains(":") match {
      case true => {
        val tagParts = tagText.split(":\\s+")
        SecTag(tagParts(0).toLowerCase, tagParts(1))
      }

      case false => SecTag("link", tagText)
    }

  }


  // validate that a tag can be processed
  def processTag(tag: SecTag): String = {

    if (NotesParser.otherTagKinds.contains(tag.kind)) {
      processOtherTag(tag)
    } else {
      val tagItemOption = world filter(_.id.equals(tag.id)) headOption

      tagItemOption match {
        case Some(x) => processItemTag(tag, x)
        case None => {
          println("\t\tinvalid item tag id: " + tag.id)
          tagString(tag)
        }
      }
    }

  }


  // generate text for tags that reference WorldItems
  def processItemTag(tag: SecTag, item: WorldItem): String = tag.kind match {

    case NotesParser.LinkKind => ExportPages.textLinkPage(item)

    // for now
    case _ => (s"""<b>${tag.kind.capitalize}: </b>"""
      + ExportPages.textLinkPage(item)
      + Tags.br)
  }


  // generate text for tag kinds that don't reference WorldItems
  def processOtherTag(tag: SecTag): String = tag.kind match {

    case NotesParser.EmbedPreKind => {
      // TODO: implement this!
      tagString(tag)
    }

    case _ => ""
  }


  def tagString(tag: SecTag): String = {
    "{{" + tag.kind + ":" + tag.id + "}}"
  }

}



object NotesParser {

  val matcher = "\\{\\{(.*?)\\}\\}".r

  // upper camelcase looks wierd, but I guess it's convention
  // http://docs.scala-lang.org/style/naming-conventions.html
  val LinkKind = "link"
  val EmbedPreKind = "embed-pre"

  val itemTagKinds = List(LinkKind)
  val otherTagKinds = List(EmbedPreKind)


}
