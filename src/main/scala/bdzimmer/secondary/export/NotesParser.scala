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
case class SecTag(kind: String, id: String)


class NotesParser(world: List[WorldItem]) {

  val metaItems = WorldItem.filterList[MetaItem](world)


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

    println("\tgetTag")

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
    case NotesParser.ImageKind => panel(ExportPages.imageLinkPage(item, metaItems, false, 320), true)
    case NotesParser.ImageResponsiveKind => panel(ExportPages.imageLinkPage(item, metaItems, true), false)
    case NotesParser.JumbotronBackgroundKind => jumbotronBackground(item, metaItems)

    // tags that aren't recognized are displayed along with links
    case _ => (s"""<b>${tag.kind.capitalize}: </b>"""
      + ExportPages.textLinkPage(item)
      + Tags.br)
  }


  // generate text for tag kinds that don't reference WorldItems
  def processOtherTag(tag: SecTag): String = tag.kind match {

    case NotesParser.JumbotronForegrounKind => jumbotronForeground(tag.id)

    case NotesParser.EmbedPreKind => {
      // TODO: implement this!
      tagString(tag)
    }

    case _ => ""
  }

  // helper methods
  // TODO: put these helper methods in the companion object?

  def panel(contents: String, pullRight: Boolean = false): String = {

    val pullClass = pullRight match {
      case true => " pull-right"
      case false => ""
    }

    val leftMargin = pullRight match {
      case true => """ style="margin-left:32px""""
      case false => ""
    }

    (s"""<div class="panel panel-default${pullClass}"${leftMargin}><div class="panel-body">${contents}""" +
    """</div></div>""")

  }


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
    "{{" + tag.kind + ":" + tag.id + "}}"
  }


}



object NotesParser {

  val matcher = "\\{\\{(.*?)\\}\\}".r

  // upper camelcase looks wierd, but I guess it's convention
  // http://docs.scala-lang.org/style/naming-conventions.html
  val LinkKind = "link"
  val ImageKind = "image"
  val ImageResponsiveKind = "image-responsive"
  val EmbedPreKind = "embed-pre"
  val JumbotronBackgroundKind = "jumbotron-bg"
  val JumbotronForegrounKind = "jumbotron-fg"

  val itemTagKinds = List(LinkKind, ImageKind, ImageResponsiveKind,
      JumbotronBackgroundKind)
  val otherTagKinds = List(EmbedPreKind, JumbotronForegrounKind)


}
