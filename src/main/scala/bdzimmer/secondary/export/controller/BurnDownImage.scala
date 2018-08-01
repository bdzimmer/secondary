// Copyright (c) 2017 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.export.controller

import java.util.Calendar
import java.util.concurrent.TimeUnit
import java.text.SimpleDateFormat

import java.io.ByteArrayOutputStream
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.awt.{Color, BasicStroke, RenderingHints}

import javax.xml.bind.DatatypeConverter
import java.nio.{ByteBuffer, ByteOrder}

import bdzimmer.secondary.export.model.Tags.Task
import bdzimmer.orbits.{DateTime, CalendarDateTime}

import bdzimmer.secondary.export.model.SecTags
import bdzimmer.secondary.export.view.Html


object BurnDownImage {

  def render(
      tasks: List[Task],
      startDate: CalendarDateTime,
      endDate: CalendarDateTime,
      weekends: Boolean): String = {

    val taskRanges = BurnDownImage.taskRanges(tasks, startDate, endDate)

    val (points, dates) = BurnDownImage.pointsAndDates(
        taskRanges, startDate, endDate, false)

    val (pointsFiltered, datesFiltered) = if (!weekends) {
      points.zip(dates).filter(x => {
        !BurnDownImage.isWeekend(BurnDownImage.toCalendar(x._2))
      }).unzip
    } else {
      (points, dates)
    }

    // there are a number of valid ways to compute workScheduled here

    // this makes a chart that doesn't go negative - use with addDuring = true above
    // val workScheduled = taskRanges.map(_._3.points).sum

    // this is more appropriate for scrum - only sum work scheduled on start date
    // use with addDuring = false above
    val workScheduled = taskRanges.filter(_._1.equals(startDate)).map(_._3.points).sum
    val workCompleted = taskRanges.map(_._3).filter(_.kind.equals(SecTags.Done)).map(_.points).sum

    println("work scheduled: " + workScheduled)
    println("work completed: " + workCompleted)

    BurnDownImage.image(
        pointsFiltered, datesFiltered, workScheduled, workCompleted)


  }

  def taskRanges(
      tasks: List[Task],
      startDate: CalendarDateTime,
      endDate: CalendarDateTime): List[(CalendarDateTime, Option[CalendarDateTime], Task)] = {

    val startCalendar = toCalendar(startDate)
    val endCalendar = toCalendar(endDate)
    val defaultDate = CalendarDateTime(2017, 1, 1, 0, 0, 0)

    val taskRangesAll = tasks.flatMap(task => {
      if (task.kind.equals(SecTags.Todo) | task.kind.equals(SecTags.Started)) {
        Some((
          task.log.map(x => DateTime.parse(x)).getOrElse(defaultDate),
          None,
          task))
      } else if (task.kind.equals(SecTags.Done)) {
        Some((
          task.log.map(x => DateTime.parse(x)).getOrElse(defaultDate),
          task.done.map(x => DateTime.parse(x)),
          task))
      } else {
        None
      }
    })

    // filter the ranges that lie inside of the burn down range
    val taskRanges = taskRangesAll.filter(
        r => diff(toCalendar(r._1), startCalendar) >= 0
        & r._2.map(x => diff(endCalendar, toCalendar(x)) >= 0).getOrElse(true))

    taskRanges
  }


  def pointsAndDates(
      taskRanges: List[(CalendarDateTime, Option[CalendarDateTime], Task)],
      startDate: CalendarDateTime,
      endDate: CalendarDateTime,
      addDuring: Boolean = true): (List[Int], List[CalendarDateTime])  = {

    val startCalendar = toCalendar(startDate)
    val endCalendar = toCalendar(endDate)
    val defaultDate = CalendarDateTime(2017, 1, 1, 0, 0, 0)

    val burnDownLength = diff(endCalendar, startCalendar).toInt + 1

    val points = scala.collection.mutable.ArrayBuffer.fill[Int](burnDownLength)(0)
    val dates = scala.collection.mutable.ArrayBuffer.fill[Calendar](burnDownLength)(toCalendar(defaultDate))

    val curCalendar = startCalendar.clone().asInstanceOf[Calendar]

    var idx = 0
    var curPoints = 0

    while (idx < burnDownLength) {

      for ((start, end, task) <- taskRanges) {
        if ((idx == 0 || addDuring) && diff(toCalendar(start), curCalendar) == 0) {
          curPoints += task.points
          println(idx + " " + toCalendarDateTime(curCalendar).dateString + " " + task + " logged " + curPoints)
        }
        if (task.kind.equals(SecTags.Done)) {
          if (end.map(x => diff(toCalendar(x), curCalendar) == 0).getOrElse(false)) {
            curPoints -= task.points
            println(idx + " " + toCalendarDateTime(curCalendar).dateString + " " + task + " done " + curPoints)
          }
        }
      }

      points(idx) = curPoints
      dates(idx) = curCalendar.clone().asInstanceOf[Calendar]

      idx += 1
      curCalendar.add(Calendar.DATE, 1)

    }

    (points.toList, dates.map(x => toCalendarDateTime(x)).toList)

  }


  def ascii(points: List[Int], dates: List[CalendarDateTime]): String = {
    // create range tuples of the tasks that contain their start, end, and points

    // TODO: logic for ranges that go negative
    val maxPoints = points.max

    val res = "<!--burn down in code block-->\n" + points.zip(dates).map(x =>
      ("    " + x._2.dateString + " "
          + "[=]" * x._1 + "---" * (maxPoints - x._1) + " "
          + x._1)
    ).mkString("\n") + "\n"

    res
  }


  def image(
      points: List[Int], dates: List[CalendarDateTime],
      workScheduled: Int, workCompleted: Int): String = {

    val image = new BufferedImage(640, 480, BufferedImage.TYPE_INT_ARGB)

    if (points.length > 0) {

      // val pointsDiff = points.zip(0 +: points).map(x => x._1 - x._2)
      // val workScheduled = pointsDiff.filter(x => x > 0).sum
      // val workCompleted = 0 - pointsDiff.filter(x => x < 0).sum
      val workMin = math.min(workScheduled - workCompleted, 0)

      val xmargin = 40
      val ymargin = 10
      val graphHeight = 380
      val graphWidth = 580

      val gr = image.createGraphics()
      gr.setRenderingHint(
          RenderingHints.KEY_ANTIALIASING,
          RenderingHints.VALUE_ANTIALIAS_ON)

      val dateX = 0.0 to graphWidth by (graphWidth / (dates.length - 1.0))
      def pointY(x: Int): Int = ((x - workMin) * (graphHeight / (workScheduled - workMin).toDouble)).toInt

      // draw axis lablels

      gr.rotate(math.Pi / 2)
      gr.setColor(Color.BLACK)
      for ((date, x) <- dates.zip(dateX)) {
        gr.drawString(date.dateString, ymargin + graphHeight + 2,  - xmargin - x.toInt + 3)
      }
      gr.rotate(0.0 - math.Pi / 2)

      for (point <- (workMin to workScheduled)) {
        gr.drawString(
            point.toString.padTo(4, " ").mkString,
            xmargin - 20, ymargin + graphHeight - pointY(point).toInt + 3)
      }

      // draw vertical and horizontal lines
      gr.setColor(Color.GRAY)
      for (x <- dateX) {
        gr.drawLine(xmargin + x.toInt, ymargin, xmargin + x.toInt, ymargin + graphHeight)
      }
      for (y <- (workMin to workScheduled).map(pointY)) {
        gr.drawLine(xmargin, ymargin + graphHeight - y.toInt, xmargin + graphWidth, ymargin + graphHeight - y.toInt)
      }

      // draw burn down lines

      gr.setStroke(new BasicStroke(3))

      // ideal
      gr.setColor(Color.BLUE)
      gr.drawLine(
          xmargin, ymargin + graphHeight - pointY(workScheduled),
          xmargin + graphWidth, ymargin + graphHeight - pointY(0))

      // actual
      gr.setColor(Color.RED)
      for (idx <- 0 until (points.length - 1)) {
        val x = dateX(idx)
        val y = pointY(points(idx))
        val nextx = dateX(idx + 1)
        val nexty = pointY(points(idx + 1))
        gr.drawLine(
            xmargin + x.toInt, ymargin + graphHeight - y.toInt,
            xmargin + nextx.toInt, ymargin + graphHeight - nexty.toInt)
      }

    }

    val imageString = imageToBase64(image, "png")
    Html.image("data:image/png;base64," + imageString)

  }


  // this won't work properly for leap years but I don't care
  def diff(x: Calendar, y: Calendar): Long = {
    TimeUnit.MILLISECONDS.toDays(
            math.abs(x.getTimeInMillis() - y.getTimeInMillis()))
  }


  def toCalendar(x: CalendarDateTime): Calendar = {
    val res = Calendar.getInstance
    res.set(Calendar.YEAR, x.year)
    res.set(Calendar.MONTH, x.month - 1)
    res.set(Calendar.DAY_OF_MONTH, x.day)
    res.set(Calendar.HOUR, x.hours)
    res.set(Calendar.MINUTE, x.minutes)
    res.set(Calendar.SECOND, x.seconds.toInt)
    res.set(Calendar.MILLISECOND, 0)
    res
  }


  def toCalendarDateTime(x: Calendar): CalendarDateTime = {
    return CalendarDateTime(
        x.get(Calendar.YEAR),
        x.get(Calendar.MONTH) + 1,
        x.get(Calendar.DAY_OF_MONTH),
        x.get(Calendar.HOUR),
        x.get(Calendar.MINUTE),
        x.get(Calendar.SECOND))
  }

  def isWeekend(x: Calendar): Boolean = {
    val dayOfWeek = x.get(Calendar.DAY_OF_WEEK)
    (dayOfWeek == Calendar.SUNDAY) || (dayOfWeek == Calendar.SATURDAY)
  }


  // super useful, not sure why I didn't write something like this much earlier
  def imageToBase64(image: BufferedImage, filetype: String): String = {
    val os = new ByteArrayOutputStream()
    ImageIO.write(image, filetype, os)
    val ba = os.toByteArray()
    val bb = ByteBuffer.allocate(ba.length)
    bb.put(ba)
    DatatypeConverter.printBase64Binary(bb.array)
  }

}
