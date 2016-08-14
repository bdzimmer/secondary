// Copyright (c) 2016 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.export.controller


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

      case SecTags.Link => {
        stringToItem.get(tag.value).map(item =>
          if (tag.args.length > 0) {
            val anchorText = tag.args.mkString(" ")
            val anchorName = new AnchorLinkNode(anchorText).getName
            Link(item, Some(anchorText), Some(anchorName))
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

          Flight(item, startLocation, endLocation, startDate, endDate, passengers)
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
      case SecTags.Thought | SecTags.Todo | SecTags.Done | SecTags.Blocked => {
        val log   = args.get("log")
        val start = args.get("start")
        val done  = args.get("done")
        Task(tag.kind, tag.value, log, start, done)
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

      case _ => ParseError(tag, s"invalid tag kind '${tag.kind}'")

    }
  }

}
