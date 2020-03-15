// Copyright (c) 2020 Ben Zimmer. All rights reserved.

// Tests for Markdown -> Latex conversion.

package bdzimmer.secondary.export.controller

import bdzimmer.secondary.export.view.Markdown
import org.scalatest.FunSuite


class LatexSuite extends FunSuite {

  test("code blocks") {

    val pp = Markdown.getPegDown(false)

    val examples = List(
      // no code blocks, result will end with newline
      ("test\n\ntest\n\ntest", "test\n\ntest\n\ntest\n"),

      // single line code block, surrounded by blank lines
      ("test\n\n    a\n\ntest", "test\n\n\\begin{lstlisting}\na\n\\end{lstlisting}\n\ntest\n"),

      // multiple line code block, surrounded by blank lines
      ("test\n\n    a\n    b\n\ntest", "test\n\n\\begin{lstlisting}\na\nb\n\\end{lstlisting}\n\ntest\n"),

      // multiple line code block containing blank line
      ("test\n\n    a\n\n    b\n\ntest", "test\n\n\\begin{lstlisting}\na\n\nb\n\\end{lstlisting}\n\ntest\n"),

      // multiple line code block ending with two blank lines
      ("test\n\n    a\n\n    b\n\n\ntest", "test\n\n\\begin{lstlisting}\na\n\nb\n\\end{lstlisting}\n\ntest\n"),

      // two blank lines
      ("test\n\n    a\n\n\n    b\n\ntest", "test\n\n\\begin{lstlisting}\na\n\n\nb\n\\end{lstlisting}\n\ntest\n"),

      // multiple code blocks
      ("test\n\n    a\n---\n\n    b\n\ntest",
       "test\n\n\\begin{lstlisting}\na\n\\end{lstlisting}\n\n---\n\n\\begin{lstlisting}\nb\n\\end{lstlisting}\n\ntest\n")

    )

    examples.foreach({case (input, expected) => {
      val result = Latex.convert(input)
      val resultHTML = pp.markdownToHtml(input)
      println(input)
      println("~~~~")
      println(result)
      println("~~~~")
      println(resultHTML)
      println("~~~~ ~~~~ ~~~~ ~~~~")
      println()

      assert(result == expected)
    }})
  }


  test("quote marks") {

    val examples = List(
      ("\"hello\", \"world\"", "``hello'', ``world''"), // pair of double-quoted words
      ("'hello', 'world'", "`hello', `world'"),         // pair of single-quoted words
      ("\"hello\", 'world'", "``hello'', `world'"),     // double then single
      ("'hello', \"world\"", "`hello', ``world''"),     // single then double
      ("\"hello, 'world'\"", "``hello, `world'''"),     // nested on left
      ("\"'hello,' world\"", "```hello,' world''")      // nested on right
      // TODO: more tests with addl comma placements
    )

    examples.foreach({case (input, expected) => {
      val result = Latex.convertLine(input)
      println(input)
      println("~~~~")
      println(result)
      println("~~~~ ~~~~ ~~~~ ~~~~")
      println()

      assert(result == expected)
    }})

  }

}
