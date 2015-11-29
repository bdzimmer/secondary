// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Gather event tags from an item and its children, sort them by date,
// and render a timeline with links back to the original item.

// Timeline formatting is loosely based on the timelines found in the
// appendices of Return of the King.

package bdzimmer.secondary.export.controller

import bdzimmer.util.{Result, Pass, Fail}

import bdzimmer.secondary.export.model._
import bdzimmer.secondary.export.view.{Markdown, Tags}


class Timeline(months: List[String]) {

  type DateTuple = (Int, Option[Int], Option[Int])

  def getHtml(item: WorldItem, format: String): String = {

    val events = WorldItem.collectionToList(item).map(event => {
      event.tags.filter(_.kind.equals(ParseSecTags.Event)).map(tag => {
        // date, description, originating page
        (parseDateTuple(tag.value), tag.args.mkString(" "), event)
      })
    }).flatten.sortBy(_._1)

    format match {
      case Timeline.DayFormat => renderByDay(events)
      case Timeline.MonthDayTableFormat => renderByMonthDayTable(events)
      case _ => renderByMonthDayParagraph(events)
    }

  }


  private def renderByDay(events: List[(DateTuple, String, WorldItem)]): String = {
    events.groupBy(_._1._1).toList.sortBy(_._1).map({case(year, curYear) => {

      s"<h4>${year}</h4>" +
      curYear.groupBy(_._1._2).toList.sortBy(_._1).map({case(month, curMonth) => {

        month.map(x => Tags.p(Tags.b(months(x).capitalize))).getOrElse("") +
        Tags.table(curMonth.map({case(date, desc, src) => {
          List(date._3.map(_ + Tags.nbsp + "-" + Tags.nbsp).getOrElse(""),
               Markdown.processLine(desc) + Tags.nbsp + ExportPages.glyphLinkPage(src))
        }}), tdStyle = List("text-align: right; vertical-align: top", "vertical-align: top")) +
        Tags.br

      }}).mkString
    }}).mkString
  }


  // TODO: indent style, some little formatting fixes
  // http://stackoverflow.com/questions/2833068/text-indent-after-the-first-line-in-a-paragraph

  private def renderByMonthDayParagraph(events: List[(DateTuple, String, WorldItem)]): String = {
    events.groupBy(_._1._1).toList.sortBy(_._1).map({case(year, curYear) => {

      s"<h4>${year}</h4>" +
      curYear.groupBy(_._1._2).toList.sortBy(_._1).map({case(month, curMonth) => {

        """<p style="text-indent: -1em; padding-left: 1em">""" +
        month.map(x => Tags.b(months(x).capitalize) + Tags.nbsp).getOrElse("") +
        curMonth.map({case(date, desc, src) => {
          date._3.map(x => Tags.b(x + ". ")).getOrElse("") +
          Markdown.processLine(desc) + Tags.nbsp +
          ExportPages.glyphLinkPage(src) + Tags.nbsp
        }}).mkString + "</p>"

      }}).mkString + Tags.br
    }}).mkString
  }

  // not sure if I will use this one
  private def renderByMonthDayTable(events: List[(DateTuple, String, WorldItem)]): String = {
    events.groupBy(_._1._1).toList.sortBy(_._1).map({case(year, curYear) => {

      s"<h4>${year}</h4>" +
      Tags.table(curYear.map({case(date, desc, src) => {

        val dateString = date._2.map(x => months(x).capitalize).getOrElse("") + date._3.map(" " + _).getOrElse("")
        val dateStringWithSuffix = if (dateString.length > 0) {
          Tags.b(dateString) + Tags.nbsp + "-" + Tags.nbsp
        } else {
          ""
        }

        List(dateStringWithSuffix,
             Markdown.processLine(desc) + Tags.nbsp + ExportPages.glyphLinkPage(src))

      }}), tdStyle = List("text-align: right; vertical-align: top; white-space: nowrap", "vertical-align: top")) +
      Tags.br

    }}).mkString
  }


  private def renderNaive(events: List[(DateTuple, String, WorldItem)]): String = {
    events.map({case(date, desc, src) => {
      Tags.p(
          renderDateTuple(date) + " - "
          + Markdown.processLine(desc) + Tags.nbsp
          + ExportPages.glyphLinkPage(src))
    }}).mkString("")
  }


  private def parseDateTuple(date: String): DateTuple = {

    val parseResult = date.split("\\s*[,\\s]+").toList match {
      case month :: day :: year :: Nil => Result(
          (year.toInt, Some(parseMonth(month)), Some(day.toInt)))
      case month :: year :: Nil => Result(
          (year.toInt, Some(parseMonth(month)), None))
      case year :: Nil => Result((year.toInt, None, None))
      case _ => Fail("Invalid date specification")
    }

    parseResult match {
      case Pass(x) => x
      case Fail(msg) => (0, Some(0), Some(0)) // default date for now
    }

  }


  private def parseMonth(month: String): Int = {
    val index = months.indexWhere(x => x.contains(month.toLowerCase))
    if (index == -1) 0 else index
  }


  private def renderDateTuple(date: DateTuple): String = date match {
    case (year, Some(month), Some(day)) =>  months(month).capitalize + " " + day + ", " + year
    case (year, Some(month), None) => months(month).capitalize + " " + year
    case (year, None, None) => year.toString
    case _ => "Invalid DateTuple"
  }


}


object Timeline {

  val DefaultMonths = List(
      "january", "february", "march", "april",
      "may", "june", "july", "august",
      "september", "october", "november", "december")

  val DayFormat = "day"
  val MonthDayParagraphFormat = "monthDayParagraph"
  val MonthDayTableFormat = "monthDayTable"

}
