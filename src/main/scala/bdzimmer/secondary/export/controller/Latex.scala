// Copyright (c) 2019 Ben Zimmer. All rights reserved.

// Generate LaTeX files for books.


package bdzimmer.secondary.export.controller

import bdzimmer.secondary.export.controller.Book.SectionInfo
import bdzimmer.secondary.export.model.Tags.ParsedTag
import bdzimmer.secondary.export.model.WorldItems.BookItem

import scala.collection.immutable.Seq
import scala.util.matching.Regex


object Latex {

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

    val allSections = (titlePage.toList ++ contentSections).map(x =>
      x.copy(content = Latex.convert(x.content)))
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
  val titlePage :: remainingSections = sections

  // TODO: extract some additional information from title page

  val chapters = remainingSections.map(section => {
    // the first line of the section is the chapter title header
    val trimmed = section.content.split("\n").tail.mkString("\n")
    val converted = convert(trimmed)

    s"\\chapter{${section.name}}\n$converted"

  }).mkString("\n")

  val packages =
    "\\" + "usepackage{titling}" +
    "\\" + "usepackage[pagestyles]{titlesec}"

raw"""\documentclass{book}

$packages

\titleformat{\chapter}[display]{\normalfont\bfseries}{}{0pt}{\Huge}
\newpagestyle{mystyle}{
  \sethead[\thepage][\textbf{\theauthor}][]
          {}{\textbf{\thetitle}}{\thepage}}
\pagestyle{empty}

\title{$title}
\author{$firstname $lastname}
% \date{today} % this seems to be the default

\begin{document}

\frontmatter
\thispagestyle{empty}
\pagenumbering{empty} % no page numbers or headers in front matter

\maketitle
% \tableofcontents

\mainmatter
\pagestyle{mystyle}

$chapters

\end{document}
"""

  }


  // Convert markdown to LaTeX. Planned functionality:
  // + Strip secondary tags
  // - Fix pairs of quote marks in each paragraph
  // - Markdown bold / italics
  // - Markdown headers
  def convert(markdown: String): String = {
    val stripped = ExtractRawTags.matcher.replaceAllIn(markdown, _ => "")

    val dqMatcher = "\\\"([^\\\"]+)\\\"".r
   // val sqMatcher = "\\\'([^\\\']+)\\\'".r

    val quotesFixed = stripped.split("\n").map(line => {
      // val line1 = sqMatcher.replaceAllIn(line, m=> "`" + m.group(1) + "'")
      val line1 = dqMatcher.replaceAllIn(line, m => "``" + m.group(1) + "''")
      val line2 = "\\\"".r.replaceAllIn(line1, _ => "``")
      line2

      // TODO: deal properly with single quotes that are not apostrophes
      // not really sure how to do this at the moment
      // best solution honestly might be to convert LaTeX syntax back to markdown, lol
    }).mkString("\n")
    quotesFixed
  }

}
