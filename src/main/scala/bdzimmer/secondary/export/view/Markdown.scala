// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Class to wrap Markdown processing.

package bdzimmer.secondary.export.view

import org.pegdown.{PegDownProcessor, Extensions}


object Markdown {

  // process a line of text (like a description or title) with PegDown
  // eliminating beginning and ending paragraph tags

  // val PPLine = getPegDown(ebookMode = false)

  def processLine(line: String): String = {
    // val pp = getPegDown(ebookMode = false)
    val pp = getPegDown(ebookMode = false)
    val html = pp.markdownToHtml(line)
    html.stripPrefix("<p>").stripSuffix("</p>")
  }

  def process(text: String, ebookMode: Boolean): String = {
    val pp = getPegDown(ebookMode = ebookMode)
    val res = pp.markdownToHtml(text)
    res.replaceAll("&quot;", "&ldquo;")
  }

  private def getPegDown(ebookMode: Boolean): PegDownProcessor = {
    val flags = if (ebookMode) {
      Extensions.SMARTYPANTS
    } else {
      Extensions.SMARTYPANTS | Extensions.EXTANCHORLINKS
    }
    new PegDownProcessor(flags)
  }

}
