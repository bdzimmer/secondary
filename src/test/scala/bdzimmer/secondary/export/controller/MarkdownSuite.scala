// Copyright (c) 2022 Ben Zimmer. All rights reserverd.

// Tests for markdown behavior.
// Trying to finally debug that quote matching behavior.


package bdzimmer.secondary.`export`.controller
import bdzimmer.secondary.`export`.view.Markdown

import org.scalatest.FunSuite


class MarkdownSuite extends FunSuite {

  test("quotes") {

    val pp = Markdown.getPegDown(false)

    val examples = List(

      // gut check basic markdown functionality
      ("test", "<p>test</p>"),
      ("test\ntest", "<p>test test</p>"),

      // quote behavior
      ("\"test\"", "<p>&ldquo;test&rdquo;</p>"),

      // multiple quote pairs within a paragraph
      (
        "\"test,\" he said. \"baloney.\"",
        "<p>&ldquo;test,&rdquo; he said. &ldquo;baloney.&rdquo;</p>"
      ),

      // paragraph without quote at end...this does not quite do what I want
      // the &quot in the first paragraph should be an &ldquo
      (
        "\"test\n\n\"test\"",
        // "<p>&ldquo;test</p>\n<p>&ldquo;test&rdquo;</p>"
        "<p>&quot;test</p>\n<p>&ldquo;test&rdquo;</p>"
      ),

      // open and closed quote, then open quote
      (
        "\"baloney,\" he said. \"test\n\n\"test\"",
        "<p>&ldquo;baloney,&rdquo; he said. &quot;test</p>\n<p>&ldquo;test&rdquo;</p>"
      ),

      // multiple open quotes
      (
        "\"test\n\n\"test\n\n\"test\"",
        "<p>&quot;test</p>\n<p>&quot;test</p>\n<p>&ldquo;test&rdquo;</p>"
      )

    )

    examples.foreach({ case (input, expected) => {
      val resultHTML = pp.markdownToHtml(input)

      println(input)
      println("~~~~")
      println(resultHTML)
      println("~~~~ ~~~~ ~~~~ ~~~~")
      println()

      assert(resultHTML == expected)
    }})

  }

}
