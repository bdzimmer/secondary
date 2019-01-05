// Copyright (c) 2019 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.export.controller

import scala.annotation.tailrec

import bdzimmer.secondary.export.model.WorldItems
import bdzimmer.secondary.export.model.WorldItems.WorldItem


object WordCount {

  def calculate(item: WorldItem, recursive: Boolean): Int = {
    val items = if (recursive) {
      WorldItems.collectionToList(item)
    } else {
      List(item)
    }
    val notes = items.map(item => {
      // calculating word count on notes transformed to prose is a
      // little bit more accurate that raw notes but probably isn't
      // necessary
      // val tagPositions = stringToTags.get(item.id).getOrElse(Map())
      // transformProse(item.notes, tagPositions)
      item.notes
    }).mkString("\n")
    notes.split("\\s+").length
  }


  def interactive(getItem: () => Option[WorldItem]): Unit = {
    val startCount = getItem().map(WordCount.calculate(_, true)).getOrElse(0)
    println("Interactive Wordcount Mode")
    println("Enter to update; q to quit")
    val startTime = System.currentTimeMillis
    println("initial wordcount: " + startCount)

    @tailrec
    def loop(): Unit = {
      val key = System.in.read()
      if (key != 13) {
        // ignore carriage returns
        val curCount = getItem().map(WordCount.calculate(_, true)).getOrElse(0)
        val totalTime = (System.currentTimeMillis - startTime) / 1000.0 / 3600.0
        val wordChange = curCount - startCount
        val wordsPerHour = wordChange / totalTime
        println(wordChange + " words - " + totalTime + " hr - " + wordsPerHour + " wph")
      }
      if (key != 113) {
        loop()
      }
    }

    loop()

  }

}
