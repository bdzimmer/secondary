// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Gather event tags from an item and its children, sort them by date,
// and render a timeline with links back to the original item.

// Timeline format choices are loosely based on the timelines found in the
// appendices of Return of the King.

// TODO: most of this is logic for HTML rendering, which maybe should be moved to a view class.

package bdzimmer.secondary.export.controller

import bdzimmer.util.{Result, Pass, Fail}

import bdzimmer.secondary.export.model.{Tags, WorldItems}
import bdzimmer.secondary.export.model.WorldItems.WorldItem
import bdzimmer.secondary.export.view.{Markdown, Html}

import bdzimmer.orbits.DateTupleParser
import bdzimmer.orbits.DateTime.DateTuple


class Timeline(
    dtp: DateTupleParser,
    val stringToTags: Map[String, Map[Int, Tags.ParsedTag]]) {

  // technically this doesn't need the full stringToTags, it only needs a map from id to tags.

  def getHtml(item: WorldItem, format: String): String = {

    val events = WorldItems.collectionToList(item).flatMap(it => {
      stringToTags.get(it.id).map(_.values.collect({
        case tag: Tags.EventTag => (dtp.parse(tag.date), tag.desc, it)
      })).getOrElse(List())
    }).sortBy(_._1)

    format match {
      case Timeline.DayTableFormat  => renderByDayTable(events)
      case Timeline.YearTableFormat => renderByYearTable(events)
      case _ => renderByMonthDayParagraph(events)
    }

  }


  private def renderByDayTable(events: List[(DateTuple, String, WorldItem)]): String = {
    events.groupBy(_._1._1).toList.sortBy(_._1).map({case(year, curYear) => {

     Html.h4(year.toString) +
      curYear.groupBy(_._1._2).toList.sortBy(_._1).map({case(month, curMonth) => {

        // the endline is required for the markdown processing
        month.map(x => Html.b(dtp.month(x).capitalize)).getOrElse("") + "\n\n\n" +
        Html.table(None, curMonth.map({case(date, desc, src) => {
          List(date._3.map(x => Html.b(x.toString) + Timeline.ColumnSeparator).getOrElse(""),
               Markdown.processLine(desc) + Html.nbsp + RenderPages.glyphLinkPage(src))
        }}), tdStyle = Some(Timeline.TableStyle), None, None) + Html.br

      }}).mkString
    }}).mkString
  }


  // http://stackoverflow.com/questions/2833068/text-indent-after-the-first-line-in-a-paragraph
  private def renderByMonthDayParagraph(events: List[(DateTuple, String, WorldItem)]): String = {
    events.groupBy(_._1._1).toList.sortBy(_._1).map({case(year, curYear) => {

      Html.h4(year.toString) +
      curYear.groupBy(_._1._2).toList.sortBy(_._1).map({case(month, curMonth) => {

        pIndent(
            month.map(x => Html.b(dtp.month(x).capitalize) + Html.nbsp).getOrElse("") +
            eventStrings(curMonth, date => date._3.map(x => Html.b(x + ". ")).getOrElse(""))
        )

      }}).mkString + Html.br
    }}).mkString
  }


  private def renderByYearTable(events: List[(DateTuple, String, WorldItem)]): String = {

    Html.table(None, events.groupBy(_._1._1).toList.sortBy(_._1).map({case(year, curYear) => {

      List(
          Html.b(year.toString) + Timeline.ColumnSeparator,
          eventStrings(curYear, date => {
            Html.b(date._2.map(x => dtp.month(x).capitalize + " ").getOrElse("") + date._3.map(_ + ". ").getOrElse(""))
          }))

    }}), tdStyle = Some(Timeline.TableStyle), None, None) + Html.br

  }


  private def eventStrings(events: List[(DateTuple, String, WorldItem)], dateFormat: DateTuple => String): String = {

    // group events together that ocurred on the same day
    events.groupBy(_._1).toList.sortBy(_._1).map({case(date, eventsOfDate) => {
      dateFormat(date) +
      eventsOfDate.map({case(date, desc, src) => {
        Markdown.processLine(desc) + Html.nbsp +
        RenderPages.glyphLinkPage(src) + Html.nbsp
      }}).mkString
    }}).mkString

  }


  private def pIndent(contents: String): String = {
    s"""<p style="text-indent: -1em; padding-left: 1em">${contents}</p>"""
  }


  private def renderNaive(events: List[(DateTuple, String, WorldItem)]): String = {
    events.map({case(date, desc, src) => {
      Html.p(
          dtp.render(date) + " - "
          + Markdown.processLine(desc) + Html.nbsp
          + RenderPages.glyphLinkPage(src))
    }}).mkString("")
  }

}


object Timeline {

  val DayTableFormat = "dayTable"
  val MonthDayParagraphFormat = "monthDayParagraph"
  val YearTableFormat = "yearTable"

  // TODO: move timeline table style into master style sheet
  val TableStyle = List("text-align: right; vertical-align: top; white-space: nowrap", "vertical-align: top")

  // val ColumnSeparator = Tags.nbsp + "-" + Tags.nbsp
  val ColumnSeparator = Html.nbsp + Html.nbsp + Html.nbsp
}
