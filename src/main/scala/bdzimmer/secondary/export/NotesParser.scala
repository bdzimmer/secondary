// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Functionality for parsing markdown and special Secondary tags.

// 2015-08-30: Created. Link and general tags.
// 2015-09-01: Image tags.
// 2015-09-02: Tags for jumbotron background image and text color.

package bdzimmer.secondary.export

import org.pegdown.PegDownProcessor
import scala.util.matching.Regex
import org.apache.commons.io.FileUtils

// TODO: change id to value
case class SecTag(kind: String, value: String)


class NotesParser(world: List[WorldItem]) {

  val metaItems = WorldItem.filterList[MetaItem](world)

  // transform markdown text with special tags to HTML
  def transform(text: String): String = {

    // process special tags
    val updatedText = NotesParser.matcher.replaceAllIn(text, m => {
      val tag = NotesParser.getTag(m.group(1))
      processTag(tag)
    })

    val pp = NotesParser.getPegDown
    pp.markdownToHtml(updatedText)
  }

  // validate that a tag can be processed
  def processTag(tag: SecTag): String = {

    if (NotesParser.otherTagKinds.contains(tag.kind)) {
      processOtherTag(tag)
    } else {
      val tagItemOption = world filter(_.id.equals(tag.value)) headOption

      tagItemOption match {
        case Some(x) => processItemTag(tag, x)
        case None => {
          println("\t\tinvalid item tag id: " + tag.value)
          tagString(tag)
        }
      }
    }

  }


  // generate text for tags that reference WorldItems
  def processItemTag(tag: SecTag, item: WorldItem): String = tag.kind match {

    case NotesParser.LinkKind => ExportPages.textLinkPage(item)
    case NotesParser.ImageKind => ExportPages.panel(ExportPages.imageLinkPage(item, metaItems, false, 320), true)
    case NotesParser.ImageResponsiveKind => ExportPages.panel(ExportPages.imageLinkPage(item, metaItems, true), false)
    case NotesParser.JumbotronBackgroundKind => jumbotronBackground(item, metaItems)

    // tags that aren't recognized are displayed along with links
    case _ => (s"""<b>${tag.kind.capitalize}: </b>"""
      + ExportPages.textLinkPage(item)
      + Tags.br)
  }


  // generate text for tag kinds that don't reference WorldItems
  def processOtherTag(tag: SecTag): String = tag.kind match {
    case NotesParser.JumbotronForegroundKind => jumbotronForeground(tag.value)
    case _ => tagString(tag)
  }

  // helper methods
  // TODO: put these helper methods in the companion object?


  def jumbotronBackground(item: WorldItem, metaItems: List[MetaItem]): String = {

    val imagePath = ExportPages.itemImagePath(item, metaItems)

    s"""<style>
  .jumbotron {
    background-image: url("${imagePath}");
    background-size: cover;
    background-position: 0% 50%;
  }
</style>"""

  }


  def jumbotronForeground(color: String): String = {

    s"""<style>
  .jumbotron {
    color: ${color};
  }
</style>"""

  }


  def tagString(tag: SecTag): String = {
    // "{{" + tag.kind + ":" + tag.value + "}}"
    s"""<b>${tag.kind.capitalize}: </b>""" + tag.value + Tags.br
  }


}



object NotesParser {

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
    NotesParser.matcher.findAllMatchIn(text).map(m => {
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
