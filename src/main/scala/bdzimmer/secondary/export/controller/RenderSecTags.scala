// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.export.controller

import scala.util.matching.Regex

import org.pegdown.ast.AnchorLinkNode

import bdzimmer.secondary.export.model.{CharacterItem, MetaItem, WorldItem, ParseSecTags, SecTag}
import bdzimmer.secondary.export.view.{Markdown, Tags}


class RenderSecTags(val world: List[WorldItem], disableTrees: Boolean = false) {

  val metaItems = WorldItem.filterList[MetaItem](world)

  // transform markdown text with special tags to HTML
  def transform(text: String): String = {

    // process special tags
    val updatedText = ParseSecTags.matcher.replaceAllIn(text, m => {
      val tag = ParseSecTags.getTag(m.group(1))

      // $ and \ have special meaning in replacement strings, this quotes them
      // it may be desirable for performance to only apply below to results of certain tags
      // that are more likely to include slashes
      Regex.quoteReplacement(processTag(tag))
    })

    val pp = Markdown.getPegDown
    pp.markdownToHtml(updatedText)
  }


  // validate that a tag can be processed and process it
  def processTag(tag: SecTag): String = {

    if (ParseSecTags.OtherTagKinds.contains(tag.kind)) {
      RenderSecTags.processOtherTag(tag)
    } else {

      // if it's an item tag, match the tag value against the world
      val itemOption = matchItemTag(tag)

      itemOption match {
        case Some(item) => processItemTag(tag.kind, item, tag.args)
        case None => {
          println("\t\tinvalid item tag: " + tag.kind + " " + tag.value)
          RenderSecTags.tagString(tag)
        }
      }

    }

  }


  def matchItemTag(tag: SecTag): Option[WorldItem] = {

    // original: match tag value against WorldItem id
    // world.filter(_.id.equals(tag.value)).headOption

    // new: match tag against id first; if no matches match against name
    val idMatches = world.filter(_.id.equals(tag.value))
    idMatches.length match {
      case 1 => idMatches.headOption
      case _ => {

        val nameMatches = world.filter(_.name.equals(tag.value))
        nameMatches.length match {
          case 1 => nameMatches.headOption
          case _ => None
        }

      }
    }

  }



  // generate text for tags that reference WorldItems
  // TODO: version that replaces tags with pure text or nothing
  def processItemTag(kind: String, item: WorldItem, args: List[String]): String = kind match {

    case ParseSecTags.Link => RenderSecTags.link(item, args)
    case ParseSecTags.Image => RenderSecTags.image(item, RenderSecTags.parseArgs(args), metaItems)
    case ParseSecTags.ImageResponsive => ExportPages.panel(ExportImages.imageLinkPage(item, metaItems, true), false)
    case ParseSecTags.FamilyTree => familyTree(item)
    case ParseSecTags.Jumbotron => RenderSecTags.jumbotron(item, RenderSecTags.parseArgs(args), metaItems)

    // tags that aren't recognized are displayed along with links
    case _ => (s"""<b>${kind.capitalize}: </b>"""
      + ExportPages.textLinkPage(item)
      + Tags.br)
  }


  def familyTree(item: WorldItem): String = {

    // transform is called to prepare mouseover text for family trees. We don't want
    // family trees to be rendered in those results.

    if (disableTrees) {
      ""
    } else {
      item match {
        case character: CharacterItem => {
          val safeRender = new RenderSecTags(this.world, true)
          val characters = WorldItem.filterList[CharacterItem](world)
          val result = FamilyTree.TreeStyles + FamilyTree.getTreeJs(
              character, characters, safeRender)

          result
        }
        case _ => ""
      }
    }
  }


}



object RenderSecTags {

  def parseArgs(args: List[String]): Map[String, String] = {
    args.map(x => x.split("=").toList match {
      case fst :: snd :: xs => Some((fst, snd))
      case _ => None
    }).flatten.toMap
  }


  // generate text for tag kinds that don't reference WorldItems
  def processOtherTag(tag: SecTag): String = tag.kind match {
    case _ => RenderSecTags.tagString(tag)
  }


  // TODO: Image stuff really needs overhauling.
  def image(item: WorldItem, args: Map[String, String], metaItems: List[MetaItem]): String = {

    if (args.getOrElse("link", "true").equals("true")) {
      ExportPages.panel(ExportImages.imageLinkPage(item, metaItems, false, 320), true)
    } else {
      ExportPages.panel(ExportImages.imageLinkPage(item, metaItems, false, 320, false), true)
    }

  }



  // more flexible jumbotron tag renderer
  def jumbotron(item: WorldItem, args: Map[String, String], metaItems: List[MetaItem]): String = {

    val imagePath = ExportImages.itemImagePath(item, metaItems)
    val xPos = args.getOrElse("xpos", "0%")
    val yPos = args.getOrElse("ypos", "50%")
    val color = args.getOrElse("color", "black")

    s"""
<style>
  .jumbotron {
    background-image: url("${imagePath}");
    background-size: cover;
    background-position: ${xPos} ${yPos};
    color: ${color};
  }
</style>"""

  }

  // more flexible link renderer
  def link(item: WorldItem, args: List[String]): String = {

    if (args.length > 0) {
      // are there some conditions where this won't really work?
      val anchorText = args.mkString(" ")
      val anchorName = new AnchorLinkNode(anchorText).getName
      ExportPages.textLinkPage(item, anchorName, anchorText)
    } else {
      ExportPages.textLinkPage(item)
    }


  }


  def tagString(tag: SecTag): String = {
    s"""<b>${tag.kind.capitalize}: </b>""" + tag.value + Tags.br
  }


}
