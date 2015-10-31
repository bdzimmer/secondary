// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.export.controller

// TODO: this is not so good. see TODO item below
import bdzimmer.secondary.export.model.{MetaItem, WorldItem, ParseSecTags, SecTag}
import bdzimmer.secondary.export.view.Tags


class RenderSecTags(world: List[WorldItem]) {

  val metaItems = WorldItem.filterList[MetaItem](world)

  // transform markdown text with special tags to HTML
  def transform(text: String): String = {

    // process special tags
    val updatedText = ParseSecTags.matcher.replaceAllIn(text, m => {
      val tag = ParseSecTags.getTag(m.group(1))
      processTag(tag)
    })

    val pp = ParseSecTags.getPegDown
    pp.markdownToHtml(updatedText)
  }

  // validate that a tag can be processed
  def processTag(tag: SecTag): String = {

    if (ParseSecTags.otherTagKinds.contains(tag.kind)) {
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


  // TODO: move the stuff that processes SecTags into a class in controller

  // generate text for tags that reference WorldItems
  def processItemTag(tag: SecTag, item: WorldItem): String = tag.kind match {

    case ParseSecTags.LinkKind => ExportPages.textLinkPage(item)
    case ParseSecTags.ImageKind => ExportPages.panel(ExportImages.imageLinkPage(item, metaItems, false, 320), true)
    case ParseSecTags.ImageResponsiveKind => ExportPages.panel(ExportImages.imageLinkPage(item, metaItems, true), false)
    case ParseSecTags.JumbotronBackgroundKind => jumbotronBackground(item, metaItems)

    // tags that aren't recognized are displayed along with links
    case _ => (s"""<b>${tag.kind.capitalize}: </b>"""
      + ExportPages.textLinkPage(item)
      + Tags.br)
  }


  // generate text for tag kinds that don't reference WorldItems
  def processOtherTag(tag: SecTag): String = tag.kind match {
    case ParseSecTags.JumbotronForegroundKind => jumbotronForeground(tag.value)
    case _ => tagString(tag)
  }

  // helper methods
  // TODO: put these helper methods in the companion object?


  def jumbotronBackground(item: WorldItem, metaItems: List[MetaItem]): String = {

    val imagePath = ExportImages.itemImagePath(item, metaItems)

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
