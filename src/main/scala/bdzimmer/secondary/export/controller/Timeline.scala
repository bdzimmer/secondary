// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.export.controller

import com.google.api.client.util.DateTime

import bdzimmer.util.{Result, Pass, Fail}

import bdzimmer.secondary.export.model._
import bdzimmer.secondary.export.view.{Markdown, Tags}


class Timeline(months: List[String]) {

  type DateTuple = (Int, Option[Int], Option[Int])


  def getTimelineHtml(item: WorldItem): String = {

    val events = WorldItem.collectionToList(item).map(x => {
      x.tags.filter(_.kind.equals(ParseSecTags.Event)).map(y => {
        // date, description, originating page
        (parseDateTuple(y.value), y.args.mkString(" "), x)
      })
    }).flatten.sortBy(_._1)

    // render the list
    // renderNaive(events, months)
    renderByDay(events)
  }


  // TODO: move timeline table style into Styles object
  private def renderByDay(events: List[(DateTuple, String, WorldItem)]): String = {
    events.groupBy(_._1._1).toList.sortBy(_._1).map({case(year, curYear) => {
      s"""<h4>${year}</h4>""" + curYear.groupBy(_._1._2).toList.sortBy(_._1).map({case(month, curMonth) => {

        month.map(x => Tags.p(s"""<b>${months(x).capitalize}</b>""")).getOrElse("") +
        Tags.table(curMonth.map({case(date, desc, src) => {
          List(date._3.map(_ + "&nbsp;-&nbsp;").getOrElse(""),
               Markdown.processLine(desc) + "&nbsp;" + ExportPages.glyphLinkPage(src))
        }}), tdStyle = List("text-align: right; vertical-align: top", "vertical-align: top")) +
        Tags.br

      }}).mkString("")
    }}).mkString("\n")
  }


  private def renderNaive(events: List[(DateTuple, String, WorldItem)]): String = {
    events.map({case(date, desc, src) => {
      Tags.p(
          renderDateTuple(date) + " - "
          + Markdown.processLine(desc) + "&nbsp;"
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

}
