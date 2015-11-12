// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Class to wrap Markdown processing.

package bdzimmer.secondary.export.view

import org.pegdown.PegDownProcessor

import org.pegdown.Extensions

object Markdown {

  // process a line of text (like a description or title) with PegDown
  // eliminating beginning and ending paragraph tags
  def processLine(line: String): String = {
    val pp = getPegDown
    val html = pp.markdownToHtml(line)
    html.stripPrefix("<p>").stripSuffix("</p>")
  }

  def getPegDown(): PegDownProcessor = {
    // new PegDownProcessor
    new PegDownProcessor(Extensions.SMARTYPANTS | Extensions.ANCHORLINKS)
  }

}
