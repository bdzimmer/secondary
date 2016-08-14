// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Functions for more complex tracking of tasks.

package bdzimmer.secondary.export.controller

import scala.collection.immutable.Seq

import bdzimmer.secondary.export.model.WorldItems.WorldItem
import bdzimmer.secondary.export.model.Tags.Task
import bdzimmer.secondary.export.view.{Markdown, Html, WebResource}

// TODO: some kind of actual date class for dates
case class TaskTableItem(
    kind: String,
    desc: String,
    item: WorldItem,
    group: WorldItem,
    log: Option[String],
    start: Option[String],
    done: Option[String])


object Tasks {

   val TasksStyles =
     s"""<script src="${WebResource.Jquery.localRelFilename}"></script>""" + "\n" +
     s"""<script src="${WebResource.DataTablesJs.localRelFilename}" charset="utf-8"></script>""" + "\n" +
     s"""<link href="${WebResource.DataTablesCss.localRelFilename}" rel="stylesheet">""" + "\n"


   def createTask(s: Task, item: WorldItem, group: WorldItem): TaskTableItem = {
     TaskTableItem(s.kind, s.desc, item, group, s.log, s.start, s.done)
   }


   def table(tasks: Seq[TaskTableItem]): String = {

     val head = List(
         Html.b("Group"),
         Html.b("Item"),
         Html.b("Status"),
         Html.b("Description"),
         Html.b("Log"),
         Html.b("Start"),
         Html.b("Done")).map(_ + Html.nbsp + Html.nbsp)

     val body = tasks.sortBy(_.log).map(task => {
       List(
           ExportPages.textLinkPage(task.group),
           ExportPages.textLinkPage(task.item),
           Html.b(task.kind.capitalize),
           Markdown.processLine(task.desc),
           task.log.getOrElse(""),
           task.start.getOrElse(""),
           task.done.getOrElse("")).map(_ + Html.nbsp + Html.nbsp)
     }).toList

     val styles = List(
         "vertical-align: top; white-space: nowrap",
         "vertical-align: top; white-space: nowrap",
         "vertical-align: top; white-space: nowrap",
         "vertical-align: top",
         "vertical-align: top; white-space: nowrap",
         "vertical-align: top; white-space: nowrap",
         "vertical-align: top; white-space: nowrap")

     Html.table(Some(head), body, Some(styles), Some("tasks"), Some("dataTable display")) +
     TasksStyles +
     """<script>$(document).ready(function() {$('#tasks').dataTable({"pageLength": 30, "stateSave": true});});</script>"""
   }
}
