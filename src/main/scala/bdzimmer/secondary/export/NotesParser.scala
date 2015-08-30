// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Functionality for parsing markdown and special tags for Secondary functionality.

// 2015-08-30: Created.

package bdzimmer.secondary.export

import org.pegdown.PegDownProcessor

object NotesParser {

  def getPegDown(): PegDownProcessor = {
    // new PegDownProcessor(Extensions.HARDWRAPS)
    new PegDownProcessor
  }

  // transform markdown text with special tags to HTML
  def transform(text: String): String = {
    val pp = getPegDown
    pp.markdownToHtml(text)
  }

}
