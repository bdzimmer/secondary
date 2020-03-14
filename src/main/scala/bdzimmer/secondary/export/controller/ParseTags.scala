// Copyright (c) 2016 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.export.controller

import bdzimmer.orbits.{DateTime, Vec3, Editor, AnimationSettings}

import bdzimmer.util.StringUtils.StringConvertSafe


object ExtractRawTags {

  import bdzimmer.secondary.export.model.Tags.RawTag

  // val matcher = "\\{\\{(.*?)\\}\\}".r  // original matcher
  val matcher = "(?s)\\{\\{(.*?)\\}\\}".r  // new multi-line capable matcher


  def getAllTags(text: String): Map[Int, RawTag] = {
    matcher.findAllMatchIn(text).map(m => {
      (m.start, getTag(m.group(1)))
    }).toMap
  }


  // generate a tag from text
  def getTag(tagText: String): RawTag = {

    val idx = tagText.indexOf(":")

    if (idx > -1) {
      val kind = tagText.substring(0, idx)
      val text = tagText.substring(idx + 1).trim()
      text.split("\\s*\\|\\s*").toList match {
        case value :: args => RawTag(kind.toLowerCase, value, args)
        case _ => RawTag(kind.toLowerCase, text, List())
      }
    } else {
      RawTag("link", tagText, List())
    }

  }


  def parseArgs(args: List[String]): Map[String, String] = {
    args.flatMap(x => x.split("=").toList match {
      case fst :: snd :: xs => Some((fst, snd))
      case _ => None
    }).toMap
  }

}



object ParseTags {

  import scala.util.Try

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
            // val anchorName = new AnchorLinkNode(anchorText).getName
            // Link(item, Some(anchorText), Some(anchorName))
            Link(item, Some(anchorText), Some(anchorText))
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

      // TODO: convert dates strings to date objects here
      case SecTags.Birth => Birth(tag.value, tag.args.mkString(" "))
      case SecTags.Death => Death(tag.value, tag.args.mkString(" "))
      case SecTags.Event => Event(tag.value, tag.args.mkString(" "))

      // ~~~~ ~~~~ space flight tags ~~~~

      // TODO: update documentation with defaults
      // important to have documentation of defaults for complex tags

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

      case SecTags.Mass | SecTags.Acceleration | SecTags.Velocity => {
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

      case SecTags.FlightEpoch => {
        val name = tag.value
        val defaultStartDate = CalendarDateTime(2016, 1, 1, 12, 0, 0.0)
        val defaultEndDate   = CalendarDateTime(2016, 2, 1, 12, 0, 0.0)
        val startDate = args.get("startdate").map(DateTime.parse(_)).getOrElse(defaultStartDate)
        val endDate   = args.get("enddate").map(DateTime.parse(_)).getOrElse(defaultEndDate)
        FlightEpoch(name, startDate, endDate)
      }

      case SecTags.FlightAnimation => {

        val camPosDefault =  Vec3(
          Editor.CameraSettingsDefault.xPos,
          Editor.CameraSettingsDefault.yPos,
          Editor.CameraSettingsDefault.zPos)
        val intDefault = 1.0 / 24.0
        val widthDefault = 1280
        val heightDefault = 720

        stringToItem.get(tag.value).map(item => {
          val epoch = args.getOrElse("epoch", "default")
          val width = args.get("width").map(_.toIntSafe(widthDefault)).getOrElse(widthDefault)
          val height = args.get("height").map(_.toIntSafe(heightDefault)).getOrElse(heightDefault)
          val camType = args.getOrElse("camtype", "follow")
          val camPos = args.get("campos").map(parseVec3(_, Vec3(0.0, 0.0, 0.0))).getOrElse(camPosDefault)
          val zViewPos = args.get("zviewpos").map(_.toDoubleSafe()).getOrElse(Editor.CameraSettingsDefault.zViewPos)
          val fps = args.get("fps").map(_.toIntSafe()).getOrElse(30)
          val interval = args.get("interval").map(_.toDoubleSafe(intDefault)).getOrElse(intDefault)  // one hour
          val damping = args.get("damping").map(_.toDoubleSafe()).getOrElse(Editor.Damping)
          val status = args.get("status").map(_.toIntSafe(1)).getOrElse(1)
          val style = args.getOrElse("style", "default")
          val visible = args.get("visible").map(_.split(";\\s+").toList).getOrElse(List())

          FlightAnimation(
            item, epoch,
            AnimationSettings(width, height, camType, camPos, zViewPos, fps, interval, damping),
            status,
            style,
            visible)

        }).getOrElse(ParseError(tag, s"item '${tag.value}' does not exist"))

      }

      // ~~~~ ~~~~ ~~~~ ~~~~

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
          val sections = args.get("sections").map(_.toBooleanSafe).getOrElse(false)
          WordCount(item, recursive, sections)
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

      case SecTags.Sidenote => {
        Sidenote(tag.value, args.getOrElse("id", ""))
      }

      case SecTags.Footnotes => {
        stringToItem.get(tag.value).map(item => {
          Footnotes(item, args.get("sections").map(_.toBooleanSafe).getOrElse(true))
        }).getOrElse(ParseError(tag, s"item '${tag.value}' does not exist"))
      }

      case SecTags.Snip => {
        Snip(tag.value, args.getOrElse("paragraphs", "1").toIntSafe(1))
      }

      case SecTags.Quote => {
        stringToItem.get(tag.value).map(item => {
          Quote(item, args.getOrElse("id", ""))
        }).getOrElse(ParseError(tag, s"item '${tag.value}' does not exist"))
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

  // TODO: function that builds the standard parse error
  // TODO: replace map + getOrElse with existss

  def parseVec3(x: String, default: Vec3): Vec3 = {
      val elems = x.split(",\\s+").map(_.toDoubleSafe(0.0))
      if (elems.size != 3) {
        default
      } else {
        Vec3(elems(0), elems(1), elems(2))
      }
  }

}
