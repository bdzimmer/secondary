// Copyright (c) 2020 Ben Zimmer. All rights reserved.

// Tests for Markdown -> Latex conversion.

package bdzimmer.secondary.export.controller

import bdzimmer.secondary.export.view.Markdown
import org.scalatest.FunSuite


class LatexSuite extends FunSuite {

  test("code blocks and lists") {

    val pp = Markdown.getPegDown(false)

    val examples = List(

      // code blocks examples

      // no code blocks, result will end with newline
      ("test\n\ntest\n\ntest", "test\n\ntest\n\ntest\n"),

      // single line code block, surrounded by blank lines
      ("test\n\n    a\n\ntest", "test\n\n\\begin{lstlisting}\na\n\\end{lstlisting}\n\ntest\n"),

      // single line code block with no terminating blank lines
      ("test\n\n    a\n", "test\n\n\\begin{lstlisting}\na\n\\end{lstlisting}\n\n"),

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
       "test\n\n\\begin{lstlisting}\na\n\\end{lstlisting}\n\n---\n\n\\begin{lstlisting}\nb\n\\end{lstlisting}\n\ntest\n"),

      // code block containing asterisks
      ("test\n\n    *test*\n\ntest",
       "test\n\n\\begin{lstlisting}\n*test*\n\\end{lstlisting}\n\ntest\n"),


      // unordered lists!

      // simple three-item list
      ("test\n\n* a\n* b\n* c\n\ntest",
       "test\n\n\\begin{itemize}[nolistsep]\n  \\item  a\n  \\item  b\n  \\item  c\n\\end{itemize}\n\ntest\n"),

      // three-item list with another indent level
      ("test\n\n* a\n* b\n    * c\n\ntest",
        "test\n\n\\begin{itemize}[nolistsep]\n  \\item  a\n  \\item  b\n  \\begin{itemize}[nolistsep]\n    \\item  c\n  \\end{itemize}\n\\end{itemize}\n\ntest\n"),

      // three-item list with another indent level and no termination
      ("test\n\n* a\n* b\n    * c\n",
        "test\n\n\\begin{itemize}[nolistsep]\n  \\item  a\n  \\item  b\n  \\begin{itemize}[nolistsep]\n    \\item  c\n  \\end{itemize}\n\\end{itemize}\n"),

      // three-item list with different indent level and no termination
      ("test\n\n* a\n    * b\n* c\n",
        "test\n\n\\begin{itemize}[nolistsep]\n  \\item  a\n  \\begin{itemize}[nolistsep]\n    \\item  b\n  \\end{itemize}\n  \\item  c\n\\end{itemize}\n")

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
      ("'hello', 'world'", "`hello', `world'"), // pair of single-quoted words
      ("'hello' and 'world'", "`hello' and `world'"), // pair of single-quoted words without comma
      ("\"hello\", 'world'", "``hello'', `world'"), // double then single
      ("'hello', \"world\"", "`hello', ``world''"), // single then double
      ("\"hello, 'world'\"", "``hello, `world'\\thinspace''"), // nested on left
      ("\"'hello,' world\"", "``\\thinspace`hello,' world''"), // nested on right

      ("test's test's", "test's test's"),
      ("\"test's test's\"", "``test's test's''"),
      ("'til the end, don't leave.", "'til the end, don't leave."),
      ("\"She said, 'Don't give up! Don't quit!'\"", "``She said, `Don't give up! Don't quit!'\\thinspace''"),

      // Some examples where the current result is not "correct."

      // The apostrophe at the end of "Heckin'" is taken as the closing single quote.
      // This is not an easy problem to solve, since we identify closing single quotes
      // by being followed with a non-word character. If we change that to "non-word
      // character and also not a space" that prevents single quoted words like above.
      // Solution is probably a special tag that translates to the correct thing
      // in both modes.

      ("'Heckin' birb!'", "`Heckin' birb!'"),

      // (This one actually works, due to asymmetry.)
      ("'Well, 'til we meet again!'", "`Well, 'til we meet again!'")

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
