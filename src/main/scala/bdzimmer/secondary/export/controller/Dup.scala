// Copyright (c) 2019 Ben Zimmer. All rights reserved.
// Duplicate word detection.

package bdzimmer.secondary.export.controller


object Dup {

  // val Matcher = "(\\b\\w+\\b)(\\s+\\1)+".r
  val Matcher = "(\\b\\S+)(?:\\s+\\1\\b)+".r

  def find(notes: String): List[(Int, (Int, Int))] = {
    notes.split("\n").toList.zipWithIndex.flatMap(
      x => Matcher.findAllMatchIn(x._1).map(m => (x._2, (m.start, m.end))))
  }

}
