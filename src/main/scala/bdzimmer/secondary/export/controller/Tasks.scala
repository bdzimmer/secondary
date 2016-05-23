// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Functions for more complex tracking of tasks.

package bdzimmer.secondary.export.controller

import scala.collection.immutable.Seq

import bdzimmer.secondary.export.model.{ParseSecTags, SecTag, WorldItem}
import bdzimmer.secondary.export.view.{Markdown, Tags, WebResource}

// TODO: some kind of actual date class for dates
case class Task(
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


   def createTask(s: SecTag, item: WorldItem, group: WorldItem): Task = {
     val args = ParseSecTags.parseArgs(s.args)
     Task(s.kind, s.value, item, group, args.get("log"), args.get("start"), args.get("done"))
   }


   def table(tasks: Seq[Task]): String = {

     val head = List(
         Tags.b("Group"),
         Tags.b("Item"),
         Tags.b("Status"),
         Tags.b("Description"),
         Tags.b("Log"),
         Tags.b("Start"),
         Tags.b("Done")).map(_ + Tags.nbsp + Tags.nbsp)

     val body = tasks.sortBy(_.log).map(task => {
       List(
           ExportPages.textLinkPage(task.group),
           ExportPages.textLinkPage(task.item),
           Tags.b(task.kind.capitalize),
           Markdown.processLine(task.desc),
           task.log.getOrElse(""),
           task.start.getOrElse(""),
           task.done.getOrElse("")).map(_ + Tags.nbsp + Tags.nbsp)
     }).toList

     val styles = List(
         "vertical-align: top; white-space: nowrap",
         "vertical-align: top; white-space: nowrap",
         "vertical-align: top; white-space: nowrap",
         "vertical-align: top",
         "vertical-align: top; white-space: nowrap",
         "vertical-align: top; white-space: nowrap",
         "vertical-align: top; white-space: nowrap")

     Tags.table(Some(head), body, Some(styles), Some("tasks"), Some("dataTable display")) +
     TasksStyles +
     """<script>$(document).ready(function() {$('#tasks').dataTable({"pageLength": 30, "stateSave": true});});</script>"""
   }
}
