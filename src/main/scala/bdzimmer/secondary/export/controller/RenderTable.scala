// Copyright (c) 2020 Ben Zimmer. All rights reserved.

// Parse and render Markdown-like outlines into tables.

package bdzimmer.secondary.export.controller

import bdzimmer.secondary.export.view.Html
import bdzimmer.secondary.export.view.Markdown

import scala.collection.mutable.{Buffer => MutableBuffer}


object RenderTable {

  def render(
      table: List[List[String]],
      rownames: Boolean,
      colnames: Boolean,
      style: Option[String],
      tdStyle: Option[String],
      theadStyle: Option[String]): String = {

    val (head, tail, exampleRow) = if (colnames) {
      table match {
        case row :: rows => {
          val head = row.map(cell => Html.b(cell))
          (Some(head), rows, Some(head))
        }
        case _ => (None, List(), None)
      }
    } else {
      (None, table, table.headOption)
    }

    val body: List[List[String]] = if (rownames) {
      tail.map({
        case x :: xs => Html.b(x) :: xs
        case x => x
      })
    } else {
      tail
    }

    val tdStyleRow: Option[List[String]] = exampleRow.flatMap(x => {
      tdStyle.map(y => List.fill(x.length)(y))})

    Html.table(head, body, tdStyleRow, None, None, style, theadStyle)
  }


  def itemsToTable(items: List[(String, Int)]): List[List[String]] = {

    val rows: MutableBuffer[List[String]] = MutableBuffer()

    val STATE_OUTSIDE = 0
    val STATE_OUTER_LEVEL = 1
    val STATE_INNER_LEVEL = 2
    val STATE_INNER_INNER_LEVEL = 3

    var state = STATE_OUTSIDE
    var row: MutableBuffer[String] = MutableBuffer()

    items.foreach({case (item, indent) => {

      // state transitions
      if (state == STATE_OUTSIDE && indent == 0) {
        state = STATE_OUTER_LEVEL
      } else if (state == STATE_OUTER_LEVEL && indent == 1) {
        state = STATE_INNER_LEVEL
      } else if (state == STATE_INNER_LEVEL) {
        if (indent == 0) {
          state = STATE_OUTER_LEVEL
          rows.append(row.toList)
          row = MutableBuffer()
        } else if (indent > 1) {
          state = STATE_INNER_INNER_LEVEL
        }
      } else if (state == STATE_INNER_INNER_LEVEL) {
        if (indent == 0) {
          state = STATE_OUTER_LEVEL
          rows.append(row.toList)
          row = MutableBuffer()
        } else if (indent == 1) {
          state = STATE_INNER_LEVEL
        }
      }

      // actions performed given state
      // for now the only thing is adding to a row if it's in the inner level
      // could eventually do something with outer or inner inner levels
      // such as row names, mouseover notes, etc
      if (state == STATE_OUTER_LEVEL) {
        row += item
      } else if (state == STATE_INNER_LEVEL) {
        row += item
      }

    }})

    // append remaining row
    if (row.nonEmpty) {
      rows += row.toList
    }

    rows.toList
  }



  def parseItems(markdown: String): List[(String, Int)] = {

    // TODO: remove duplicate code between this and the implementation in Latex.scala

    // strip Secondary tags
    // val stripped = ExtractRawTags.matcher.replaceAllIn(markdown, _ => "")

    // ~~~~ convert per-line symbols

    val linesFixed = markdown.split("\n")

    // ~~~~ convert code blocks

    val STATE_OUTSIDE = 0
    val STATE_LIST = 2

    var state = STATE_OUTSIDE

    var listContents: MutableBuffer[(String, Int)] = MutableBuffer()
    var listIndentLevel: Int = 0

    linesFixed.foreach(line => {

      // going into a list
      if (state == STATE_OUTSIDE && MarkdownParse.MatcherUL.findFirstIn(line).isDefined) {
        state = STATE_LIST
        listIndentLevel = 0
      } else if (state == STATE_LIST && MarkdownParse.MatcherUL.findFirstIn(line).isEmpty) {
        state = STATE_OUTSIDE
        // listContents = MutableBuffer()
        listIndentLevel = -1
      }

      // do things depending on what state we are in
      if (state == STATE_LIST) {
        val matches = MarkdownParse.MatcherUL.findFirstMatchIn(line)
        matches.foreach(m => {
          listIndentLevel = m.group(1).length / 4
          val listItem = Markdown.processLine(m.group(3))
          val record = (listItem, listIndentLevel)
          listContents += record
        })

        if (matches.isEmpty) {
          println("invalid list element formatting - no matches")
        }

      }

    })

    // finish off any lists that remain open
    if (state == STATE_LIST) {
      // listContents = MutableBuffer()
      listIndentLevel = -1
    }

    listContents.toList
  }

}
