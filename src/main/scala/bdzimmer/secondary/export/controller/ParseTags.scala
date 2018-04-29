// Copyright (c) 2016 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.export.controller

import bdzimmer.orbits.DateTime

import bdzimmer.util.StringUtils.StringConvertSafe


object ExtractRawTags {

  import bdzimmer.secondary.export.model.Tags.RawTag

  val matcher = "\\{\\{(.*?)\\}\\}".r

  def getAllTags(text: String): Map[Int, RawTag] = {
    matcher.findAllMatchIn(text).map(m => {
      (m.start, getTag(m.group(1)))
    }).toMap
  }


  // generate a tag from text
  def getTag(tagText: String): RawTag = {

    tagText.split(":\\s+").toList match {
      case kind :: text :: extra => text.split("\\s*\\|\\s*").toList match {
        case value :: args => RawTag(kind.toLowerCase, value, args)
        case _ => RawTag(kind.toLowerCase, text, List())
      }
      case _ => RawTag("link", tagText, List())
    }

  }


  def parseArgs(args: List[String]): Map[String, String] = {
    args.map(x => x.split("=").toList match {
      case fst :: snd :: xs => Some((fst, snd))
      case _ => None
    }).flatten.toMap
  }

}



object ParseTags {

  import scala.util.Try
  import org.pegdown.ast.AnchorLinkNode

  import bdzimmer.secondary.export.model.Tags._
  import bdzimmer.secondary.export.model.SecTags
  import bdzimmer.secondary.export.model.WorldItems._

  import bdzimmer.orbits.CalendarDateTime

  def parse(
      tag: RawTag,
      stringToItem: Map[String, WorldItem]): ParsedTag = {

    val args = ExtractRawTags.parseArgs(tag.args)

    def argBool(name: String, default: Boolean = true): Boolean = {
      args.get(name).map(x => "true".equals(x)).getOrElse(default)
    }

    tag.kind match {

      // TODO: verify anchors and return an error if invalid
      case SecTags.Link => {
        stringToItem.get(tag.value).map(item =>
          if (tag.args.length == 1) {
            val anchorText = tag.args.mkString(" ")
            val anchorName = new AnchorLinkNode(anchorText).getName
            Link(item, Some(anchorText), Some(anchorName))
          } else if (tag.args.length == 2) {
            Link(item, Some(tag.args(0)), Some(tag.args(1)))
          } else {
            Link(item, None, None)
          }).getOrElse(ParseError(tag, s"item '${tag.value}' does not exist"))
      }

      case SecTags.Image => {
        stringToItem.get(tag.value).flatMap(item => item match {
          case imageItem: ImageItem => Some({
            Image(imageItem, argBool("responsive", false), argBool("link"))
          })
          case _ => None
        }).getOrElse(ParseError(tag, s"image '${tag.value}' does not exist"))
      }

      case SecTags.FamilyTree => {
        stringToItem.get(tag.value).flatMap(item => item match {
          case character: CharacterItem => Some(FamilyTree(character))
          case _ => None
        }).getOrElse(ParseError(tag, s"character '${tag.value}' does not exist"))
      }

      case SecTags.Jumbotron => {
        stringToItem.get(tag.value).flatMap(item => item match {
          case imageItem: ImageItem => Some({
            val xPos  = args.getOrElse("xpos",  "0%")
            val yPos  = args.getOrElse("ypos",  "50%")
            val color = args.getOrElse("color", "black")
            Jumbotron(imageItem, xPos, yPos, color)
          })
          case _ => None
        }).getOrElse(ParseError(tag, s"image '${tag.value}' does not exist"))
      }

      case SecTags.Timeline => {
        stringToItem.get(tag.value).map(item => {
          Timeline(item, args.getOrElse(
              "format", bdzimmer.secondary.export.controller.Timeline.MonthDayParagraphFormat))
        }).getOrElse(ParseError(tag, s"item '${tag.value}' does not exist"))
      }

      // TODO: separate flight params extraction
      case SecTags.Flight => {
        stringToItem.get(tag.value).map(item => {
          val defaultStartDate = CalendarDateTime(2016, 1, 1, 12, 0, 0.0)
          val defaultEndDate   = CalendarDateTime(2016, 2, 1, 12, 0, 0.0)
          val startLocation = args.getOrElse("startloc",  "Earth")
          val endLocation   = args.getOrElse("endloc",    "Mars")
          val startDate     = args.get("startdate").map(DateTime.parse(_)).getOrElse(defaultStartDate)
          val endDate       = args.get("enddate").map(DateTime.parse(_)).getOrElse(defaultEndDate)
          val passengers    = args.getOrElse("passengers", "").split(";\\s+").toList.map(x => stringToItem.get(x)).flatten
          val faction       = args.getOrElse("faction", "none")

          Flight(item, startLocation, endLocation, startDate, endDate, passengers, faction)
        }).getOrElse(ParseError(tag, s"item '${tag.value}' does not exist"))
      }

      // TODO: convert dates strings to date objects
      case SecTags.Event => Event(tag.value, tag.args.mkString(" "))
      case SecTags.Birth => Birth(tag.value, tag.args.mkString(" "))
      case SecTags.Death => Death(tag.value, tag.args.mkString(" "))

      case SecTags.Mass | SecTags.Acceleration => {
        val parts = tag.value.split("\\s+")
        Try(parts(0).toDouble).toOption.map(value => {
          val unit = if (parts.length > 1) {
            parts(1)
          } else {
            ""
          }
          SpacecraftProperty(tag.kind, value, unit)
        }).getOrElse(ParseError(tag, s"can't parse double from '${tag.value}'"))
      }

      // TODO: convert date strings to date objects
      case SecTags.Thought | SecTags.Todo | SecTags.Started | SecTags.Done | SecTags.Blocked => {
        val log    = args.get("log")
        val start  = args.get("start")
        val done   = args.get("done")
        val points = args.get("points").map(_.toIntSafe(1)).getOrElse(1)
        Task(tag.kind, tag.value, log, start, done, points)
      }

      case SecTags.Father | SecTags.Mother | SecTags.Parent | SecTags.Ancestor => {
        stringToItem.get(tag.value).flatMap(item => item match {
          case character: CharacterItem => Some(Ancestor(tag.kind, character))
          case _ => None
        }).getOrElse(ParseError(tag, s"character '${tag.value}' does not exist"))
      }

      case SecTags.Son | SecTags.Daughter | SecTags.Child | SecTags.Descendant => {
        stringToItem.get(tag.value).flatMap(item => item match {
          case character: CharacterItem => Some(Descendant(tag.kind, character))
          case _ => None
        }).getOrElse(ParseError(tag, s"character '${tag.value}' does not exist"))
      }

      // TODO: parse marriage date
      case SecTags.Marriage => {
        stringToItem.get(tag.value).flatMap(item => item match {
          case character: CharacterItem => Some(Marriage(character, None))
          case _ => None
        }).getOrElse(ParseError(tag, s"character '${tag.value}' does not exist"))
      }

      case SecTags.Demo => {
        val body = tag.args match {
          case x :: Nil => x
          case x :: xs  => s"$x | ${xs.mkString(" | ")}"
          case _        => "..."
        }
        Demo(tag.value, body)
      }

      case SecTags.MarkovText => {
        val items = tag.value.split(";\\s+").toList.map(x => stringToItem.get(x)).flatten
        val order = args.get("order").flatMap(x => Try(x.toInt).toOption).getOrElse(2)
        val count = args.get("count").flatMap(x => Try(x.toInt).toOption).getOrElse(5)
        val seed  = args.get("seed").flatMap(x  => Try(x.toInt).toOption).getOrElse(0)
        MarkovText(items, List(), order, count, seed)
      }

      case SecTags.WordCount => {
        stringToItem.get(tag.value).map(item => {
          val recursive = args.get("recursive").map(_.toBooleanSafe).getOrElse(false)
          WordCount(item, recursive)
        }).getOrElse(ParseError(tag, s"item '${tag.value}' does not exist"))
      }

      case SecTags.BurnDown => {
        stringToItem.get(tag.value).map(item => {
          val startDate = args.get("startdate").map(DateTime.parse(_))
          val endDate   = args.get("enddate").map(DateTime.parse(_))
          val recursive = args.get("recursive").map(_.toBooleanSafe).getOrElse(false)
          val weekends  = args.get("weekends").map(_.toBooleanSafe).getOrElse(false)
          BurnDown(item, startDate, endDate, recursive, weekends)
        }).getOrElse(ParseError(tag, s"item '${tag.value}' does not exist"))
      }

      case SecTags.Anchor => {
        Anchor(tag.value, args.getOrElse("id", "anchor"))
      }

      case SecTags.Index => {
        stringToItem.get(tag.value).map(item => {
          Index(item)
        }).getOrElse(ParseError(tag, s"item '${tag.value}' does not exist"))
      }

      case SecTags.Tasks => {
        stringToItem.get(tag.value).map(item => {
          Tasks(item)
        }).getOrElse(ParseError(tag, s"item '${tag.value}' does not exist"))
      }

      case SecTags.Stats => {
        stringToItem.get(tag.value).map(item => {
          Stats(item)
        }).getOrElse(ParseError(tag, s"item '${tag.value}' does not exist"))
      }

      // TODO: function that builds the standard parse error
      case SecTags.Gallery => {
        stringToItem.get(tag.value).map(item => {
          val recursive = args.get("recursive").map(_.toBooleanSafe).getOrElse(false)
          val size = args.get("size").map(_.toIntSafe(4)).getOrElse(4)
          val showCaptions = args.get("showcaptions").map(_.toBooleanSafe).getOrElse(true)
          Gallery(item, size, showCaptions, recursive)
        }).getOrElse(ParseError(tag, s"item '${tag.value}' does not exist"))
      }

      case _ => ParseError(tag, s"invalid tag kind '${tag.kind}'")

    }
  }

}
