// Copyright (c) 2019 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.export.controller

import scala.annotation.tailrec

import bdzimmer.secondary.export.model.WorldItems
import bdzimmer.secondary.export.model.WorldItems.WorldItem


object WordCount {

  def calculate(item: WorldItem, recursive: Boolean, sections: Boolean): Int = {
    val items = if (recursive) {
      WorldItems.collectionToList(item)
    } else {
      List(item)
    }
    val notes = items.map(item => {
      // calculating word count on notes transformed to prose is a
      // little bit more accurate that raw notes but probably isn't
      // necessary
      // val tagPositions = tagsMap.get(item.id).getOrElse(Map())
      // transformProse(item.notes, tagPositions)
      item.notes
    }).mkString("\n")
    count(notes)
  }


  def calculateSections(item: WorldItem): List[(String, Int)] = {
    val (titles, chunks, _) = Book.splitSections(item.notes)
    titles.zip(chunks.map(x => count(x._2)))
  }


  def count(x: String): Int = {
    x.split("\\s+").length
  }


  def interactive(getItem: () => Option[WorldItem]): Unit = {
    val startCount = getItem().map(WordCount.calculate(_, true, false)).getOrElse(0)
    println("Interactive Wordcount Mode")
    println("Enter to update; q to quit")
    val startTime = System.currentTimeMillis
    println("initial wordcount: " + startCount)

    @tailrec
    def loop(): Unit = {
      // val key = System.in.read()
      // if (key != 13) {
        // ignore carriage returns
        val curCount = getItem().map(WordCount.calculate(_, true, false)).getOrElse(0)
        val totalTime = (System.currentTimeMillis - startTime) / 1000.0 / 3600.0
        val wordChange = curCount - startCount
        val wph = wordChange / totalTime
        val wphColor = if (wph >= 0.0 && wph < 200.0) {
          Console.BLACK
        } else if (wph >= 200.0 && wph < 400.0) {
          Console.RED
        } else if (wph >= 400.0 && wph < 600.0) {
          Console.MAGENTA
        } else if (wph >= 600.0 && wph < 800.0) {
          Console.BLUE
        } else if (wph >= 800.0 && wph < 1000.0) {
          Console.CYAN
        } else if (wph >= 1000.0) {
          Console.GREEN
        }
        println(
          wordChange + " words " +
          "(" + curCount + " total) - " +
          round(totalTime, 2) + " hr - " +
          wphColor + round(wph, 2) + " wph" +
          Console.RESET)

      // if (key != 113) {
      //   loop()
      // }

      Thread.sleep(20000) // 20 seconds
      loop()
    }

    loop()

  }


  def round(x: Double, places: Int): Double = {
    val s = math.pow(10, places)
    math.round(x * s) / s
  }

}
