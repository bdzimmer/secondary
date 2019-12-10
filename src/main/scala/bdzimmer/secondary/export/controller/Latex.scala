// Copyright (c) 2019 Ben Zimmer. All rights reserved.

// Generate LaTeX files for books.

package bdzimmer.secondary.export.controller

import scala.collection.immutable.Seq
import scala.util.matching.Regex

import org.apache.commons.io.FileUtils

import bdzimmer.secondary.export.controller.Book.SectionInfo
import bdzimmer.secondary.export.model.Tags.ParsedTag
import bdzimmer.secondary.export.model.WorldItems.BookItem


object Latex {

  // Regex stuff for converting markdown to latex.
  // TODO: unit tests to understand what these do and do not work on.

  // Note that the meaning of these is matched pairs with none of the
  // thing in between. There are probably cases that I'm not thinking of
  // where this will fail.

  val MatcherDq: Regex = "\\\"([^\\\"]+)\\\"".r
  // val MatcherSq: Regex = "\\\'([^\\\']+)\\\'".r

  val MatcherBi: Regex = "\\*\\*\\*([^\\*]+)\\*\\*\\*".r
  val MatcherB: Regex  = "\\*\\*([^\\*]+)\\*\\*".r
  val MatcherI: Regex = "\\*([^\\*]+)\\*".r

  val MatcherDqSingle: Regex = "\\\"".r

  // A lot of unused stuff here, for dealing with images.

  // Note that function is currently pretty similar to the version in Epub.
  // TODO: might want to rewrite in the future
  def export(
      filename: String,
      book: BookItem,
      tags: Map[Int, ParsedTag],
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
      allSections
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
      sections: Seq[SectionInfo]
      // coverImageFilename: Option[String],
      // imageDirname: String
    ): Unit = {

    val content = formatContentLatex(
      uniqueIdentifier,
      title,
      firstname,
      lastname,
      sections
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
      sections: Seq[Book.SectionInfo]): String = {

    // first section is title page
    val _ :: remainingSections = sections

    // TODO: extract additional information from title page?
    // eg copyright date, etc.

    val chapters = remainingSections.map(section => {
      // the first line of the section is the chapter title header
      val trimmed = section.content.split("\n").tail.mkString("\n")
      val converted = convert(trimmed)
      s"\\chapter{${section.name}}\n$converted"
    }).mkString("\n")

    val templateUrl = getClass.getResource("/template.tex")
    val template = FileUtils.readFileToString(new java.io.File(templateUrl.getPath))

    template.format(title, firstname, lastname, chapters)

    // compile with:
    //  pdflatex -interaction=nonstopmode uriels_revenge.tex

  }


  // Convert markdown to LaTeX. Planned functionality:
  // + Strip secondary tags
  // + Fix pairs of double quote marks in each paragraph
  // - Fix pairs of single quote marks in each paragraph
  // + Markdown bold / italics
  // + Markdown headers
  // + code blocks
  // - special symbols
  def convert(markdown: String): String = {
    val stripped = ExtractRawTags.matcher.replaceAllIn(markdown, _ => "")

    // convert per-line symbols

    val linesFixed = stripped.split("\n").map(line => {

      // val line1 = MatcherSq.replaceAllIn(line, m=> "`" + m.group(1) + "'")

      // Look for matched pairs of double quotes and replace with
      // left and right double quotes.
      // Afterwards, replace any remaining double quotes with left double quotes.
      val line1 = MatcherDq.replaceAllIn(line, m => "``" + m.group(1) + "''")
      val line2 = MatcherDqSingle.replaceAllIn(line1, _ => "``")

      // Look for matched pairs of *** and replace with bold and italic
      val line3 = MatcherBi.replaceAllIn(line2, m => raw"\\textbf{\\textit{" + m.group(1) + "}}")

      // Look for matched pairs of ** and replace with bold
      val line4 = MatcherB.replaceAllIn(line3, m => raw"\\textbf{" + m.group(1) + "}")

      // Look for matched pairs of * and replace with italic
      val line5 = MatcherI.replaceAllIn(line4, m => raw"\\textit{" + m.group(1) + "}")

      line5

      // TODO: deal properly with single quotes that are not apostrophes
      // not really sure how to do this at the moment
      // best solution honestly might be to convert LaTeX syntax back to markdown, lol
    })

    // convert code blocks

    var result = ""
    var inCodeBlock = false
    var codeBlockContents = ""

    linesFixed.foreach(line => {

      // are we going into a code block?

      if (line.startsWith("    ")) {
        if (!inCodeBlock) {
          codeBlockContents = codeBlockContents + "\\begin{lstlisting}\n"
          inCodeBlock = true
        }
      }

      // are we leaving a code block?
      if (!line.startsWith("    ") && !line.isEmpty && inCodeBlock) {
        // strip trailing whitespace, add back a single newline, and end the code block
        codeBlockContents = codeBlockContents.replaceAll("\\s+$", "")
        codeBlockContents = codeBlockContents + "\n\\end{lstlisting}\n\n"
        // add the code block to the result
        result = result + codeBlockContents

        // reset code block
        inCodeBlock = false
        codeBlockContents = ""
      }

      if (inCodeBlock) {
        if (line.isEmpty) {
          codeBlockContents += "\n"
        } else {
          codeBlockContents = codeBlockContents + line.substring(4) + "\n"
        }
      } else {
        result = result + line + "\n"
      }

    })

    // finish off any code blocks that remain open
    if (inCodeBlock) {
      codeBlockContents = codeBlockContents.replaceAll("\\s+$", "")
      codeBlockContents = codeBlockContents + "\n\\end{lstlisting}\n\n"
    }

    result

  }

}
