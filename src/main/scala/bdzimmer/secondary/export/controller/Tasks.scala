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
    recorded: Option[String],
    started: Option[String],
    finished: Option[String])


object Tasks {

   def createTask(s: SecTag, item: WorldItem): Task = {
     val args = ParseSecTags.parseArgs(s.args)
     Task(s.kind, s.value, item, args.get("recorded"), args.get("started"), args.get("finished"))
   }


   def table(tasks: Seq[Task]): String = {

     // TODO: proper task sorting

     val headers = List(
         Tags.b("Item"),
         Tags.b("Status"),
         Tags.b("Description"),
         Tags.b("Recorded"),
         Tags.b("Started"),
         Tags.b("Finished")).map(_ + Tags.nbsp + Tags.nbsp)

     Tags.table(
       headers +: tasks.sortBy(_.recorded).map(task => {
         List(
             ExportPages.textLinkPage(task.item),
             Tags.b(task.kind.capitalize),
             Markdown.processLine(task.desc),
             task.recorded.getOrElse(""),
             task.started.getOrElse(""),
             task.finished.getOrElse("")).map(_ + Tags.nbsp + Tags.nbsp)
       }).toList,
       List(
           "vertical-align: top; white-space: nowrap",
           "vertical-align: top; white-space: nowrap",
           "vertical-align: top",
           "vertical-align: top; white-space: nowrap",
           "vertical-align: top; white-space: nowrap",
           "vertical-align: top; white-space: nowrap")
     )

   }
}
