// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.export.controller

import scala.util.matching.Regex

import org.pegdown.ast.AnchorLinkNode

import bdzimmer.util.{Result, Pass, Fail}

import bdzimmer.secondary.export.model.{CharacterItem, RefItem, WorldItem, ParseSecTags, SecTag, SecTags}
import bdzimmer.secondary.export.view.{Markdown, Tags}


class RenderSecTags(
    val itemByString: Map[String, WorldItem], // for generating links
    val characters: List[CharacterItem],      // for family trees
    disableTrees: Boolean = false) {

  // transform markdown text with special tags to HTML
  def transform(text: String): String = {

    // process special tags
    val updatedText = ParseSecTags.matcher.replaceAllIn(text, m => {
      val tag = ParseSecTags.getTag(m.group(1))

      // $ and \ have special meaning in replacement strings, this quotes them
      // it may be desirable for performance to only apply below to results of certain tags
      // that are more likely to include slashes
      Regex.quoteReplacement(processTag(tag) match {
        case Pass(s) => s
        case Fail(_) => RenderSecTags.tagString(tag)
      })
    })

    val pp = Markdown.getPegDown
    pp.markdownToHtml(updatedText)
  }


  // a simpler version of the processTag function
  // determines whether a tag is valid without rendering it
  def validateTag(tag: SecTag): Result[String, String] = {
    if (SecTags.OtherTagKinds.contains(tag.kind)) {
       Pass("valid non-item tag")
    } else {
      // if it's an item tag, match the tag value against the world
      itemByString.get(tag.value) match {
        case Some(item) => Pass("valid item tag")
        case None       => Fail(s"""invalid item "${tag.value}" in tag "${tag.kind}"""")
      }
    }
  }


  // process a tag
  def processTag(tag: SecTag): Result[String, String] = {
    if (SecTags.OtherTagKinds.contains(tag.kind)) {
      Pass(RenderSecTags.processOtherTag(tag))
    } else {
      // if it's an item tag, match the tag value against the world
      itemByString.get(tag.value) match {
        case Some(item) => Pass(processItemTag(tag.kind, item, tag.args))
        case None       => Fail(s"""invalid item "${tag.value}" in tag "${tag.kind}"""")
      }
    }
  }



  // generate text for tags that reference WorldItems
  def processItemTag(kind: String, item: WorldItem, args: List[String]): String = kind match {

    case SecTags.Link            => RenderSecTags.link(item, args)
    case SecTags.Image           => RenderSecTags.image(item, ParseSecTags.parseArgs(args))
    case SecTags.ImageResponsive => ExportPages.panel(ExportImages.imageLinkPage(item, true), false)
    case SecTags.FamilyTree      => familyTree(item)
    case SecTags.Jumbotron       => RenderSecTags.jumbotron(item, ParseSecTags.parseArgs(args))
    case SecTags.Marriage        => RenderSecTags.marriage(item, ParseSecTags.parseArgs(args))
    case SecTags.Timeline        => RenderSecTags.timeline(item, ParseSecTags.parseArgs(args))

    // tags that aren't recognized are displayed along with links
    case _ => RenderSecTags.genLink(kind.capitalize, item)
  }


  def familyTree(item: WorldItem): String = {

    // transform is called to prepare mouseover text for family trees. We don't want
    // family trees to be rendered in those results.

    if (disableTrees) {
      ""
    } else {
      item match {
        case character: CharacterItem => {
          val safeRender = new RenderSecTags(itemByString, characters, true)
          val result = FamilyTree.TreeStyles + FamilyTree.getJs(
              character, characters, safeRender)
          result
        }
        case _ => ""
      }
    }
  }

}



object RenderSecTags {

  // generate text for tag kinds that don't reference WorldItems
  def processOtherTag(tag: SecTag): String = tag.kind match {
    case SecTags.Demo => {
      val body = tag.args match {
        case x :: Nil => x
        case x :: xs  => s"$x | ${xs.mkString(" | ")}"
        case _        => "..."
      }
      s"{{${tag.value}: ${body}}}"
    }
    case SecTags.Birth => tagArgsString(tag)
    case SecTags.Death => tagArgsString(tag)
    case SecTags.Event => tagArgsString(tag)
    case _             => tagString(tag)
  }


  // TODO: image width / alignment args
  def image(item: WorldItem, args: Map[String, String]): String = {

    if (args.getOrElse("link", "true").equals("true")) {
      ExportPages.panel(ExportImages.imageLinkPage(item, false, 320), true)
    } else {
      ExportPages.panel(ExportImages.imageLinkPage(item, false, 320, false), true)
    }

  }


  // more flexible jumbotron tag renderer
  def jumbotron(item: WorldItem, args: Map[String, String]): String = {

    val imagePath = ExportImages.itemImagePath(item)

    val xPos  = args.getOrElse("xpos",  "0%")
    val yPos  = args.getOrElse("ypos",  "50%")
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


  def marriage(item: WorldItem, args: Map[String, String]): String = {
    genLink("Marriage", item)
  }

  def timeline(item: WorldItem, args: Map[String, String]): String = {
    val timeline = new Timeline(Timeline.DefaultMonths)
    val format = args.getOrElse("format", Timeline.MonthDayParagraphFormat)
    timeline.getHtml(item, format)
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
    genShow(tag.kind.capitalize, tag.value)
  }

  def tagArgsString(tag: SecTag): String = {
    val desc = tag.args match {
      case Nil => "empty " + tag.kind.capitalize
      case _   => tag.args.mkString(" ")
    }
    genShow(tag.value, desc)
  }

  def genShow(fst: String, snd: String): String = {
    s"""<b>${fst}: </b> """ + snd + Tags.brInline
  }

  def genLink(name: String, item: WorldItem): String = {
    s"""<b>${name}: </b>""" + ExportPages.textLinkPage(item) + Tags.brInline
  }

}
