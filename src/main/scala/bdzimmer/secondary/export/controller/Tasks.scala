// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Functions for more complex tracking of tasks.

package bdzimmer.secondary.export.controller

import scala.collection.immutable.Seq

import bdzimmer.secondary.export.model.{ParseSecTags, SecTag, WorldItem}
import bdzimmer.secondary.export.view.{Markdown, Tags}

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

  // TODO: download these stylesheets in the Styles downloader and link to local copies

   val TasksStyles =
     // """<script src="https://code.jquery.com/jquery-1.12.0.min.js" charset="utf-8"></script>""" + "\n" +
     // """<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js"></script>""" + "\n" +
     // """<script src="https://cdn.datatables.net/1.10.11/js/jquery.dataTables.min.js" charset="utf-8"></script>""" + "\n" +
     // """<link href="https://cdn.datatables.net/1.10.11/css/jquery.dataTables.min.css" rel="stylesheet">""" + "\n"
     """<script src="styles/jquery.min.js"></script>""" + "\n" +
     """<script src="styles/jquery.dataTables.min.js" charset="utf-8"></script>""" + "\n" +
     """<link href="styles/jquery.dataTables.min.css" rel="stylesheet">""" + "\n"


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
