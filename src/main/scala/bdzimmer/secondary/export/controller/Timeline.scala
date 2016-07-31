// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Gather event tags from an item and its children, sort them by date,
// and render a timeline with links back to the original item.

// Timeline format choices are loosely based on the timelines found in the
// appendices of Return of the King.

// TODO: most of this is logic for HTML rendering, which maybe should be moved to a view class.

package bdzimmer.secondary.export.controller

import bdzimmer.util.{Result, Pass, Fail}

import bdzimmer.secondary.export.model._
import bdzimmer.secondary.export.view.{Markdown, Tags}
import bdzimmer.secondary.export.controller.DateTime.DateTuple


class Timeline(dtp: DateTupleParser) {

  def getHtml(item: WorldItem, format: String): String = {

    val events = WorldItem.collectionToList(item).map(it => {
      it.tags.filter(x => SecTags.EventTagKinds.contains(x.kind)).map(tag => {
        // date, description, originating page
        val desc = tag.args match {
          case Nil => "empty " + tag.kind.capitalize
          case _ => tag.args.mkString(" ")
        }
        (dtp.parse(tag.value), desc, it)
      })
    }).flatten.sortBy(_._1)

    format match {
      case Timeline.DayTableFormat  => renderByDayTable(events)
      case Timeline.YearTableFormat => renderByYearTable(events)
      case _ => renderByMonthDayParagraph(events)
    }

  }


  private def renderByDayTable(events: List[(DateTuple, String, WorldItem)]): String = {
    events.groupBy(_._1._1).toList.sortBy(_._1).map({case(year, curYear) => {

     Tags.h4(year.toString) +
      curYear.groupBy(_._1._2).toList.sortBy(_._1).map({case(month, curMonth) => {

        // the endline is required for the markdown processing
        month.map(x => Tags.b(dtp.month(x).capitalize)).getOrElse("") + "\n\n\n" +
        Tags.table(None, curMonth.map({case(date, desc, src) => {
          List(date._3.map(x => Tags.b(x.toString) + Timeline.ColumnSeparator).getOrElse(""),
               Markdown.processLine(desc) + Tags.nbsp + ExportPages.glyphLinkPage(src))
        }}), tdStyle = Some(Timeline.TableStyle), None, None) + Tags.br

      }}).mkString
    }}).mkString
  }


  // http://stackoverflow.com/questions/2833068/text-indent-after-the-first-line-in-a-paragraph
  private def renderByMonthDayParagraph(events: List[(DateTuple, String, WorldItem)]): String = {
    events.groupBy(_._1._1).toList.sortBy(_._1).map({case(year, curYear) => {

      Tags.h4(year.toString) +
      curYear.groupBy(_._1._2).toList.sortBy(_._1).map({case(month, curMonth) => {

        pIndent(
            month.map(x => Tags.b(dtp.month(x).capitalize) + Tags.nbsp).getOrElse("") +
            eventStrings(curMonth, date => date._3.map(x => Tags.b(x + ". ")).getOrElse(""))
        )

      }}).mkString + Tags.br
    }}).mkString
  }


  private def renderByYearTable(events: List[(DateTuple, String, WorldItem)]): String = {

    Tags.table(None, events.groupBy(_._1._1).toList.sortBy(_._1).map({case(year, curYear) => {

      List(
          Tags.b(year.toString) + Timeline.ColumnSeparator,
          eventStrings(curYear, date => {
            Tags.b(date._2.map(x => dtp.month(x).capitalize + " ").getOrElse("") + date._3.map(_ + ". ").getOrElse(""))
          }))

    }}), tdStyle = Some(Timeline.TableStyle), None, None) + Tags.br

  }


  private def eventStrings(events: List[(DateTuple, String, WorldItem)], dateFormat: DateTuple => String): String = {

    /*
    events.map({case(date, desc, src) => {
      dateFormat(date) +
      Markdown.processLine(desc) + Tags.nbsp +
      ExportPages.glyphLinkPage(src) + Tags.nbsp
    }}).mkString
    */

    // group events together that ocurred on the same day
    events.groupBy(_._1).toList.sortBy(_._1).map({case(date, eventsOfDate) => {
      dateFormat(date) +
      eventsOfDate.map({case(date, desc, src) => {
        Markdown.processLine(desc) + Tags.nbsp +
        ExportPages.glyphLinkPage(src) + Tags.nbsp
      }}).mkString
    }}).mkString

  }


  private def pIndent(contents: String): String = {
    s"""<p style="text-indent: -1em; padding-left: 1em">${contents}</p>"""
  }


  private def renderNaive(events: List[(DateTuple, String, WorldItem)]): String = {
    events.map({case(date, desc, src) => {
      Tags.p(
          dtp.render(date) + " - "
          + Markdown.processLine(desc) + Tags.nbsp
          + ExportPages.glyphLinkPage(src))
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
  val ColumnSeparator = Tags.nbsp + Tags.nbsp + Tags.nbsp
}
