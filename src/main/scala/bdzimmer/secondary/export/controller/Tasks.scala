// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Functions for more complex tracking of tasks.

package bdzimmer.secondary.export.controller

import scala.collection.immutable.Seq

import bdzimmer.secondary.export.model.WorldItems.{WorldItem, CollectionItem}
import bdzimmer.secondary.export.model.WorldItems
import bdzimmer.secondary.export.model.Tags.Task
import bdzimmer.secondary.export.model.Tags
import bdzimmer.secondary.export.view.{Markdown, Html, Bootstrap, WebResource}


// TODO: actual date class for dates
case class TaskTableItem(
    kind: String,
    desc: String,
    item: WorldItem,
    group: WorldItem,
    log: Option[String],
    start: Option[String],
    done: Option[String])


object Tasks {

  val MatcherShorthand = "^\\s*([-|+])\\s+(.+)$".r

  val TasksStyles =
     s"""<script src="${WebResource.Jquery.localRelFilename}"></script>""" + "\n" +
     s"""<script src="${WebResource.DataTablesJs.localRelFilename}" charset="utf-8"></script>""" + "\n" +
     s"""<link href="${WebResource.DataTablesCss.localRelFilename}" rel="stylesheet">""" + "\n"

  def render(
      master: WorldItem,
      tagsMap: Map[Int, Map[Int, Tags.ParsedTag]],
      shorthand: Boolean,
      recursive: Boolean,
      mode: String
    ): String = {

    val (items, groups) = if (recursive) {
      val items = WorldItems.collectionToList(master)
      val groups = master match {
        case x: CollectionItem => x.children.flatMap(
            group => WorldItems.collectionToList(group).map(item => (item.uid, group))).toMap
        case _                 => Map[Int, WorldItem]()
      }
      (items, groups)
    } else {
      (List(master), Map[Int, WorldItem]())
    }

    val tasksFromTags = for {
      item    <- items
      taskTag <- tagsMap(item.uid).values.collect({case x: Tags.Task => x})
    } yield {
      (taskTag, item, groups.getOrElse(item.uid, master))
    }

    val allTasks = if (shorthand) {
      val tasksFromShorthand = for {
        item <- items
        line <- item.notes.split("\n")
        m <- MatcherShorthand.findAllMatchIn(line)
      } yield {
        val kind = if (m.group(1).equals("-")) "todo" else "started"
        val desc = m.group(2)
        (Tags.Task(kind, desc, None, None, None, 0), item, groups.getOrElse(item.uid, master))
      }
      tasksFromTags ++ tasksFromShorthand
    } else {
      tasksFromTags
    }

    // get invalid tags
    def getInvalidTags(item: WorldItem): List[String] = {
      tagsMap(item.uid).values.collect({
        case x: Tags.ParseError => s"${x.msg} in tag '${x.tag.kind}'"}).toList
    }

    def taskList(todoFunc: WorldItem => List[String]): String = {
        Html.listGroup(items
            .map(x => (x, todoFunc(x)))
            .filter(_._2.length > 0)
            .map(x => {
              Html.listItem(
                  RenderPages.textLinkPage(x._1) +
                  Html.listGroup(x._2.map(text => Html.listItem(Markdown.processLine(text)))))}))
    }

    if (mode.equals("count")) {
      allTasks.count(_._1.kind.equals("todo")) + " todo, " +
      allTasks.count(_._1.kind.equals("started")) + " started"
    } else {
      val extras = if (mode.equals("all")) {
        val emptyNotes = Bootstrap.column(
          Bootstrap.Column6,
          Html.h4("Empty Notes") +
            Html.listGroup(items
              .filter(_.notes.equals(""))
              .map(x => Html.listItem(RenderPages.textLinkPage(x)))))
        val invalidTags = Bootstrap.column(
          Bootstrap.Column6,
          Html.h4("Invalid Tags") +
            taskList(getInvalidTags))

        emptyNotes + invalidTags
      } else {
        ""
      }

      Bootstrap.row(
        Bootstrap.column(Bootstrap.Column12, Tasks.table(allTasks.map(x => Tasks.createTask(x._1, x._2, x._3)))) +
        extras
      )
    }

  }


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
          RenderPages.textLinkPage(task.group),
          RenderPages.textLinkPage(task.item),
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
