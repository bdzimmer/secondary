// Copyright (c) 2016 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.export.controller

import scala.collection.immutable.Seq
import scala.util.matching.Regex

import scala.util.Random

import bdzimmer.secondary.export.model.{Tags, SecTags, WorldItems}
import bdzimmer.secondary.export.model.WorldItems.{WorldItem, CharacterItem}
import bdzimmer.secondary.export.view.{Markdown, Html}

import bdzimmer.orbits.CalendarDateTime
import bdzimmer.util.{Result, Pass, Fail}


case class FlightParams(
    ship: WorldItem,
    startLocation: String,
    endLocation: String,
    startDate: CalendarDateTime,
    endDate: CalendarDateTime,
    mass: Double,
    accel: Double,
    passengers: List[WorldItem])


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
       Tags.item(tag).fold("")(_.name)
    })
  }


  // render a tag to a string
  def render(tag: Tags.ParsedTag): String = tag match {

    case link: Tags.Link => link.anchor match {
      case Some(anchor) => {
        val name = link.displayText.getOrElse(link.item.name)
        ExportPages.textLinkPage(link.item, anchor, name)
      }
      case None => ExportPages.textLinkPage(link.item)
    }

    case image: Tags.Image => {
      // TODO: improve this logic
      if (image.responsive) {
        ExportPages.panel(ExportImages.imageLinkPage(image.item, true), false)
      } else {
        if (image.link) {
          ExportPages.panel(ExportImages.imageLinkPage(image.item, false, 320), true)
        } else {
          ExportPages.panel(ExportImages.imageLinkPage(image.item, false, 320, false), true)
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

      val imagePath = ExportImages.itemImagePath(jumbotron.item)

      // TODO: move this style chunk somewhere else
      s"""
<style>
  .jumbotron {
    background-image: url("${imagePath}");
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

      val fp = flightParams(flight)

      val shipName = fp.ship.name
      val passengersString = if (fp.passengers.isEmpty) {
        ""
      } else {
        " with " + fp.passengers.map(x => ExportPages.textLinkPage(x)).mkString(", ")
      }

      // generate a text summary
      ExportPages.textLinkPage(fp.ship) + " travels from " + fp.startLocation + " to " + fp.endLocation + passengersString + "." +
      Html.listGroup(
        List(
          genShow(
            fp.startDate.dateTimeString, shipName + " departs from " + fp.startLocation + "."),
          genShow(
            fp.endDate.dateTimeString,   shipName + " arrives at "   + fp.endLocation   + ".")
        ).map(Html.listItem(_))
      )

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
        val tagPositions = stringToTags.get(item.id).getOrElse(Map())
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

    case x: Tags.GenError   => Html.b("{{Error: " + x.msg + "}}")
    case x: Tags.ParseError => Html.b("{{Parse error: " + x.msg + "}}")

    case _ => "" // TODO: get rid of this if possible
  }


  def flightParams(flight: Tags.Flight): FlightParams = {

    // TODO: handle units

    val mass = (for {
      shipTags <- stringToTags.get(flight.ship.id)
      massTag <- shipTags.values.collect({
        case x: Tags.SpacecraftProperty => x
      }).filter(_.kind.equals(SecTags.Mass)).headOption
    } yield {
      massTag.value
    }).getOrElse(1000.0) // tonnes

    val accel = (for {
      shipTags <- stringToTags.get(flight.ship.id)
      accelTag <- shipTags.values.collect({
        case x: Tags.SpacecraftProperty => x
      }).filter(_.kind.equals(SecTags.Acceleration)).headOption
    } yield {
      accelTag.value
    }).getOrElse(0.25) // AU / day^2

    val fp = FlightParams(
      flight.ship,
      flight.startLocation, flight.endLocation,
      flight.startDate, flight.endDate,
      mass, accel,
      flight.passengers)

    fp
  }


  private def genShow(fst: String, snd: String): String = {
    s"""<b>${fst}: </b> """ + snd + Html.brInline
  }


  private def genLink(name: String, item: WorldItem): String = {
    s"""<b>${name}: </b>""" + ExportPages.textLinkPage(item) + Html.brInline
  }

}
