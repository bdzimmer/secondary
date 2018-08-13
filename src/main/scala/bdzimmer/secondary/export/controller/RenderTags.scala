// Copyright (c) 2016 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.export.controller

import scala.util.matching.Regex

import scala.util.Random

import bdzimmer.secondary.export.model.{Tags, WorldItems}
import bdzimmer.secondary.export.model.WorldItems.{WorldItem, CharacterItem}
import bdzimmer.secondary.export.view.{Markdown, Html, Bootstrap}

import bdzimmer.orbits.{DateTime, CalendarDateTime}


case class FlightParams(
  ship: WorldItem,
  startLocation: String,
  endLocation: String,
  startDate: CalendarDateTime,
  endDate: CalendarDateTime,
  mass: Double,
  accel: Double,
  vel: Double,
  passengers: List[WorldItem],
  faction: String)


class RenderTags(
  val stringToTags: Map[String, Map[Int, Tags.ParsedTag]],
  val characters: List[CharacterItem],
  disableTrees: Boolean = false) {

  // transform markdown text with special tags to HTML
  def transform(text: String, tags: Map[Int, Tags.ParsedTag]): String = {

    // process special tags
    val updatedText = ExtractRawTags.matcher.replaceAllIn(text, m => {

      // don't need to reparse anymore
      // val tag = ParseSecTags.getTag(m.group(1))
      val tag = tags.getOrElse(
        m.start, Tags.GenError(s"tag not found in map for position '${m.start}'"))

      // $ and \ have special meaning in replacement strings, this quotes them
      // it may be desirable for performance to only apply below to results of certain tags
      // that are more likely to include slashes
      Regex.quoteReplacement(render(tag))
    })

    val pp = Markdown.getPegDown
    pp.markdownToHtml(updatedText)
  }

  // transform prose for the Markov text generator
  private def transformProse(text: String, tags: Map[Int, Tags.ParsedTag]): String = {
    ExtractRawTags.matcher.replaceAllIn(text, m => {
       val tag = tags.getOrElse(
         m.start, Tags.GenError(s"tag not found in map for position '${m.start}'"))
       // if the tag references an item, render the name, otherwise don't return anything.
       // Tags.item(tag).fold("")(_.name)
       Tags.items(tag).map(_.name).mkString(", ")
    })
  }


  // render a tag to a string
  def render(tag: Tags.ParsedTag): String = tag match {

    case link: Tags.Link => link.anchor match {
      case Some(anchor) => {
        val name = link.displayText.getOrElse(link.item.name)
        RenderPages.textLinkPage(link.item, anchor, name)
      }
      case None => RenderPages.textLinkPage(link.item)
    }

    case image: Tags.Image => {
      // TODO: improve this logic
      if (image.responsive) {
        RenderPages.panel(RenderImages.imageLinkPage(image.item, true), false)
      } else {
        if (image.link) {
          RenderPages.panel(RenderImages.imageLinkPage(image.item, false, 320), true)
        } else {
          RenderPages.panel(RenderImages.imageLinkPage(image.item, false, 320, false), true)
        }
      }
    }

    case tree: Tags.FamilyTree => {
      if (disableTrees) {
        ""
      } else {
        tree.root match {
          case character: CharacterItem => {
            val safeRender = new RenderTags(stringToTags, characters, true)
            val result = FamilyTree.TreeStyles + FamilyTree.getJs(
                character, characters, safeRender)
            result
          }
          case _ => ""
        }
      }
    }

    case jumbotron: Tags.Jumbotron => {

      val imagePath = RenderImages.itemImagePath(jumbotron.item)

      // TODO: move this style chunk somewhere else
      s"""
<style>
  .jumbotron {
    background-image: url("$imagePath");
    background-size: cover;
    background-position: ${jumbotron.xPos} ${jumbotron.yPos};
    color: ${jumbotron.color};
  }
</style>"""

    }

    case timeline: Tags.Timeline => {
      val tr = new Timeline(DateTime.DefaultParser, stringToTags)
      tr.getHtml(timeline.root, timeline.format)
    }

    case flight: Tags.Flight => {
      val fp = Flight.flightParams(flight, stringToTags)
      Flight.render(fp, RenderImages.tagImagePath(flight))
    }

    case event: Tags.EventTag => genShow(event.date, event.desc)

    case sp: Tags.SpacecraftProperty => genShow(sp.kind.capitalize, sp.value + " " + sp.unit)

    case task: Tags.Task => genShow(task.kind.capitalize, task.desc)

    case x: Tags.Ancestor   => genLink(x.kind.capitalize, x.character)
    case x: Tags.Descendant => genLink(x.kind.capitalize, x.character)
    case x: Tags.Marriage   => genLink("Marriage", x.character) // TODO: show marriage date

    case demo: Tags.Demo    => s"{{${demo.kind}: ${demo.body}}}"

    case x: Tags.MarkovText => {

      //// build a corpus from the content of the tags and subtags described

      // get all unique tags and subtags referenced
      val items = x.items.flatMap(x => WorldItems.collectionToList(x)).distinct

      val proseMatcher = "^[A-Z]".r

      val itemText = items.map(item => {
        val tagPositions = stringToTags.getOrElse(item.id, Map())
        val renderedText = transformProse(item.notes, tagPositions)

        // remove non-prose lines and markdown leftovers
        val filteredText = renderedText.split("\n")
          .filter(line => proseMatcher.pattern.matcher(line).find)
          .filter(line => line.contains("."))  // get rid of non-sentences
          .mkString(" ")
          .replace("*", "")

        filteredText

      }).mkString(" ")

      val model = MarkovText.fit(itemText, x.order)

      // model.foreach(println(_))

      //// generate sentences
      val rnd = new Random(x.seed)
      (0 until x.count).map(x => MarkovText.predict(model, rnd).capitalize).mkString(" ")

    }

    case x: Tags.WordCount => {
      val items = if (x.recursive) {
        WorldItems.collectionToList(x.item)
      } else {
        List(x.item)
      }

      val notes = items.map(item => {
        // calculating word count on notes transformed to prose is a
        // little bit more accurate that raw notes but probably isn't
        // necessary
        val tagPositions = stringToTags.get(item.id).getOrElse(Map())
        transformProse(item.notes, tagPositions)
      }).mkString("\n")

      notes.split("\\s+").length.toString
    }

    case x: Tags.BurnDown => {
      val items = x.recursive match {
        case true  => WorldItems.collectionToList(x.item)
        case false => List(x.item)
      }
      val tasks = items
        .flatMap(item => stringToTags.get(item.id))
        .flatMap(_.values.collect({case tag: Tags.Task => tag}))
      BurnDownImage.render(
          tasks,
          x.startDate.getOrElse(CalendarDateTime(2017, 1, 1, 0, 0, 0)),
          x.endDate.getOrElse(CalendarDateTime(2017, 1, 14, 0, 0, 0)),
          x.weekends)
    }

    case x: Tags.Anchor => Html.anchor(x.desc, x.id)

    case x: Tags.Sidenote => {
      // rendering sidenotes is handled at a higher level
      /*
      "</div>" +
      Bootstrap.column(
        Bootstrap.Column3,
        body = s"""<p class="sidenote">${x.desc}</p>""") +
      Bootstrap.columnOpen(Bootstrap.Column9)
      */
      ""
    }

    case x: Tags.Index => Index.render(
        WorldItems.collectionToList(x.item).drop(1))

    case x: Tags.Tasks => {
      Tasks.render(x.item, stringToTags)
    }

    case x: Tags.Stats => {
      Stats.render(x.item)
    }

    case x: Tags.Gallery => {
      x.item match {
        case collection: WorldItems.CollectionItem => {
          val images = (if (x.recursive) {
            WorldItems.collectionToList(collection).drop(1)
          } else {
            collection.children
          }).collect({case image: WorldItems.ImageItem => image})
          ImageGallery.render(images, x.size, x.showCaptions)
        }
        case _ => ""
      }
    }

    case x: Tags.GenError   => Html.b("{{Error: " + x.msg + "}}")
    case x: Tags.ParseError => Html.b("{{Parse error: " + x.msg + "}}")

    case _ => "" // TODO: get rid of this if possible
  }


  private def genShow(fst: String, snd: String): String = {
    s"""<b>$fst: </b> """ + snd + Html.brInline
  }


  private def genLink(name: String, item: WorldItem): String = {
    s"""<b>$name: </b>""" + RenderPages.textLinkPage(item) + Html.brInline
  }

}
