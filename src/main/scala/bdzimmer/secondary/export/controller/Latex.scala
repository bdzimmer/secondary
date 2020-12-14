// Copyright (c) 2019 Ben Zimmer. All rights reserved.

// Generate LaTeX files for books.

package bdzimmer.secondary.export.controller

import scala.collection.immutable.Seq
import scala.util.matching.Regex

import org.apache.commons.io.{FileUtils, IOUtils}

import bdzimmer.secondary.export.controller.Book.SectionInfo
import bdzimmer.secondary.export.model.Tags.ParsedTag
import bdzimmer.secondary.export.model.WorldItems.BookItem


object Latex {

  // Regex stuff for converting markdown to latex.

  // TODO: move parsing stuff into a different object

  val MatcherDq: Regex = "\"([^\"]+)\"".r

  // Matching single pairs of single quotes is tricky due to apostrophes.

  // This will not match pairs of single quotes with apostrophes inside.
  // val MatcherSq: Regex = "(^|\\W)\'([^\']+)\'($|\\W)".r

  // This one handles apostrophes inside but will interpret apostrophes
  // at the beginning or ending of words as single quotes.
  val MatcherSq: Regex = "(^|\\W)\'(.*?)\'($|\\W)".r

  val MatcherDqSq: Regex = "`{3}".r
  val MatcherSqDq: Regex = "'{3}".r

  val MatcherBi: Regex = "\\*\\*\\*([^*]+)\\*\\*\\*".r
  val MatcherB: Regex  = "\\*\\*([^*]+)\\*\\*".r
  val MatcherI: Regex = "\\*([^*]+)\\*".r

  val MatcherH: Regex = "^#+\\s(.*)".r
  val MatcherCopyright: Regex = "&copy;".r
  val MatcherPercent: Regex = "%".r
  val MatcherEllipsis: Regex = "\\.\\.\\.".r

  val MatcherDqSingle: Regex = "\"".r

  val MatcherUL: Regex = "^(( {4})*)\\* (.*)".r

  // A lot of unused stuff here, for dealing with images.

  // Note that function is currently pretty similar to the version in Epub.
  // TODO: might want to rewrite in the future
  def export(
      filename: String,
      book: BookItem,
      tags: Map[Int, ParsedTag],
      config: Book.BookConfig,
      // renderTags: RenderTags,  // many tags do HTML specific stuff, so we won't render them
      localExportPath: String): Unit = {

    val (sections, coverImageTag) = Book.sections(book.notes, tags, None)
    // title is name of first section
    val title = sections.headOption.map(_.name).getOrElse("empty")
    val titlePage = sections.headOption.map(_.copy(name="Title Page"))
    // replace empty section names with "Content"
    val contentSections = sections.tail.map(x => if (x.name.equals("---")) x.copy(name="Content") else x)

    val (firstname, lastname) = Epub.authorNameParts(book.authorname)

    // cover page becomes new first section if cover image exists
    // val cover = coverImageTag.map(
    //   x => Epub.SectionInfo("cover", "Cover", Epub.coverPage(RenderImages.itemImagePath(x.item))))

    val allSections = titlePage.toList ++ contentSections
    // val allSections = cover.toList ++ titlePage.toList ++ contentSections

    Latex.export(
      filename,
      book.uniqueIdentifier,
      title,
      firstname,
      lastname,
      allSections,
      config
      // coverImageTag.map(x => RenderImages.itemImagePath(x.item)),
      // localExportPath
    )
  }


  def export(
      outputFilename: String,
      uniqueIdentifier: String,
      title: String,
      firstname: String,
      lastname: String,
      sections: Seq[SectionInfo],
      config: Book.BookConfig
      // coverImageFilename: Option[String],
      // imageDirname: String
    ): Unit = {

    val content = formatContentLatex(
      uniqueIdentifier,
      title,
      firstname,
      lastname,
      sections,
      config
    )

    val fileWriter = new java.io.FileWriter(outputFilename, false)
    fileWriter.write(content)
    fileWriter.close()

  }

  def formatContentLatex(
      uniqueIdentifier: String,
      title: String,
      firstname: String,
      lastname: String,
      sections: Seq[Book.SectionInfo],
      config: Book.BookConfig): String = {

    // first section is title page
    val firstSection :: remainingSections = sections

    // TODO: do something more clever with title page formatting?
    // add extra newlines to title page
    val titlepage = convert(firstSection.content).split("\n").map(line => {
        line + raw"\newline"
    }).mkString("\n")

    val chapters = remainingSections.map(section => {
      // the first line of the section is the chapter title header
      val trimmed = section.content.split("\n").tail.mkString("\n")
      val converted = convert(trimmed)
      val convertedStyled = if (config.unstyledSections.contains(section.name)) {
        println("\tnot using indented style for section '" + section.name + "'")
        "{\\parindent0pt\n" + converted + "}\n"
      } else {
        converted
      }
      s"\\chapter{${section.name}}\n$convertedStyled"
    }).mkString("\n")

    // val templateUrl = getClass.getResource("/latex/template.tex")
    // val template = FileUtils.readFileToString(new java.io.File(templateUrl.getPath))

    val inputStream = getClass.getResourceAsStream("/latex/template.tex")
    val template = IOUtils.toString(inputStream)
    inputStream.close()

    template.format(
      config.paperWidth, config.paperHeight,
      config.marginInner, config.marginOuter, config.marginTop, config.marginBottom,
      title, firstname, lastname, titlepage, chapters)

    // compile with:
    //  pdflatex -interaction=nonstopmode filename.tex

  }


  // Convert Markdown to LaTeX. Handles:
  // * strip secondary tags
  // * code blocks
  // * and parsing of individual lines (paragraphs)

  def convert(markdown: String): String = {

    // TODO: would be nice for this to have an HTML mode as well
    // I only use a subset of markdown; this could work for web mode as well

    // strip Secondary tags
    // TODO: eventually there may be a couple of tags for typesetting
    // we will need to render those here first

    val stripped = ExtractRawTags.matcher.replaceAllIn(markdown, _ => "")

    // ~~~~ convert per-line symbols

    val linesFixed = stripped.split("\n")

    // ~~~~ convert code blocks

    var result = ""
    // var inCodeBlock = false

    val STATE_OUTSIDE = 0
    val STATE_CODEBLOCK = 1
    val STATE_LIST = 2

    var state = STATE_OUTSIDE

    var codeBlockContents = ""
    var listContents = ""
    var listIndentLevel = 0

    linesFixed.foreach(line => {

      // going into a list
      if (state == STATE_OUTSIDE && MatcherUL.findFirstIn(line).isDefined) {
        listContents = listContents + "\\begin{itemize}[nolistsep]\n"
        state = STATE_LIST
        listIndentLevel = 0
      }

      // leaving a list
      if (state == STATE_LIST && MatcherUL.findFirstIn(line).isEmpty) {
        (listIndentLevel to 0 by -1).foreach(level => {
          listContents = listContents + "  " * level + "\\end{itemize}\n"
        })
        result = result + listContents

        state = STATE_OUTSIDE
        listContents = ""
        listIndentLevel = -1
      }

      // into a code block?
      if (state == STATE_OUTSIDE && line.startsWith("    ")) {
        // TODO: disentangle the formatting from identification of contents
        codeBlockContents = codeBlockContents + "\\begin{lstlisting}\n"
        state = STATE_CODEBLOCK
      }

      // are we leaving a code block?
      if (state == STATE_CODEBLOCK && !line.startsWith("    ") && !line.isEmpty) {
        // strip trailing whitespace, add back a single newline, and end the code block
        codeBlockContents = codeBlockContents.replaceAll("\\s+$", "")
        codeBlockContents = codeBlockContents + "\n\\end{lstlisting}\n\n"
        // add the code block to the result
        result = result + codeBlockContents

        // reset code block
        state = STATE_OUTSIDE
        codeBlockContents = ""
      }

      // do things depending on what state we are in
      if (state == STATE_CODEBLOCK) {
        if (line.isEmpty) {
          codeBlockContents += "\n"
        } else {
          codeBlockContents = codeBlockContents + line.substring(4) + "\n"
        }

      } else if (state == STATE_LIST) {
        val matches = MatcherUL.findFirstMatchIn(line)
        matches.foreach(m => {
          val newListIndentLevel = m.group(1).length / 4
          val padding = "  " * newListIndentLevel
          if (newListIndentLevel > listIndentLevel) {
            listContents = listContents + padding + "\\begin{itemize}[nolistsep]\n"
          } else if (newListIndentLevel < listIndentLevel) {
            listContents = listContents + "  " * listIndentLevel + "\\end{itemize}\n"
          }
          listIndentLevel = newListIndentLevel

          val listItem = convertLine(m.group(3))

          listContents = listContents + padding + "  \\item  " + listItem + "\n"
        })

        if (matches.isEmpty) {
          println("invalid list element formatting - no matches")
        }

      } else {
        result = result + convertLine(line) + "\n"

      }

    })

    // finish off any code blocks or lists that remain open
    if (state == STATE_CODEBLOCK) {

      codeBlockContents = codeBlockContents.replaceAll("\\s+$", "")
      codeBlockContents = codeBlockContents + "\n\\end{lstlisting}\n\n"
      result = result + codeBlockContents  // this was originally missing

    } else if (state == STATE_LIST) {

      (listIndentLevel to 0 by -1).foreach(level => {
        listContents = listContents + "  " * level + "\\end{itemize}\n"
      })
      result = result + listContents

      listContents = ""
      listIndentLevel = -1
    }

    result

  }


  // * Strip secondary tags
  // * Fix pairs of double quote marks in each paragraph
  // * Fix pairs of single quote marks in each paragraph
  // * Markdown bold / italics
  // * Markdown headers
  // * special symbols
  //    * &copy
  //    * %

  def convertLine(line: String): String = {

    // TODO: all of these Latex constants should be defined in an object.

    var res = line

    // single quotes first
    res = MatcherSq.replaceAllIn(res, m => m.group(1) + "`" + m.group(2) + "'" + m.group(3))

    // Matched pairs of double quotes -> left and right double quotes.
    // Afterwards, replace any remaining double quotes with left double quotes
    // and insert thin spaces between single / quote groups of three

    res = MatcherDq.replaceAllIn(res, m => "``" + m.group(1) + "''")
    res = MatcherDqSingle.replaceAllIn(res, _ => "``")
    res = MatcherDqSq.replaceAllIn(res, _ => raw"``\\thinspace`")
    res = MatcherSqDq.replaceAllIn(res, _ => raw"'\\thinspace''")

    // Matched pairs of *** -> bold and italic
    res = MatcherBi.replaceAllIn(res, m => raw"\\textbf{\\textit{" + m.group(1) + "}}")

    // Matched pairs of ** -> bold
    res = MatcherB.replaceAllIn(res, m => raw"\\textbf{" + m.group(1) + "}")

    // Matched pairs of *  -> italic
    res = MatcherI.replaceAllIn(res, m => raw"\\textit{" + m.group(1) + "}")

    // headers -> huge
    // res = MatcherH.replaceAllIn(res, m => raw"{\\huge\\noindent " + m.group(1) + "}")

    // headers -> huge bold (matches with chapter headers style)
    res = MatcherH.replaceAllIn(res, m => raw"{\\huge\\noindent \\textbf{" + m.group(1) + "}}")

    // copyright symbol
    res = MatcherCopyright.replaceAllIn(res, _ => raw"\\textcopyright\\")

    // percent sign
    res = MatcherPercent.replaceAllIn(res, _ => raw"\\%")

    // ellipsis
    res = MatcherEllipsis.replaceAllIn(res, _ => raw" \\ldots\\ ")

    res

  }

}
