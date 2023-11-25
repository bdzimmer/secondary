// Copyright (c) 2019 Ben Zimmer. All rights reserved.

// Generate LaTeX files for books.

package bdzimmer.secondary.export.controller

import scala.collection.immutable.Seq
import scala.util.matching.Regex

import org.apache.commons.io.IOUtils

import bdzimmer.secondary.export.controller.Book.SectionInfo
import bdzimmer.secondary.export.model.Tags.ParsedTag
import bdzimmer.secondary.export.model.WorldItems.BookItem


object Latex {

  val Newline = "\\newline"

  def export(
      filename: String,
      book: BookItem,
      tags: Map[Int, ParsedTag],
      config: Book.BookConfig,
      rtOption: Option[RenderTags]): Unit = {

    // Note that function is currently pretty similar to the version in Epub.
    // Optionally render tags, but don't convert markdown to HTML.

    val (sections, _) = Book.sections(book.notes, tags, rtOption, false)
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
      title,
      firstname,
      lastname,
      allSections,
      config)
  }


  def export(
      outputFilename: String,
      title: String,
      firstname: String,
      lastname: String,
      sections: Seq[SectionInfo],
      config: Book.BookConfig
    ): Unit = {

    // TODO: collapse firstname / lastname to author name tuple

    val content = formatContentLatex(
      title,
      firstname,
      lastname,
      sections,
      config)

    val fileWriter = new java.io.FileWriter(outputFilename, false)
    fileWriter.write(content)
    fileWriter.close()

  }

  def formatContentLatex(
      title: String,
      firstname: String,
      lastname: String,
      sections: Seq[Book.SectionInfo],
      config: Book.BookConfig): String = {

    // first section is title page
    val firstSection :: remainingSections = sections

    // I have no idea why this wants a thispagestyle instead of pagestyle
    // Seems like this won't work properly with multi-page TOCs but it works for now
    val toc = if (config.toc) {"\\tableofcontents\n\\thispagestyle{empty}"} else {""}

    val pageStyle = if (config.anthology) {
      // book title on one page, chapter title on facing page
      s"\\sethead[\\thepage][\\textbf{$title}][]\n           {}{\\textbf{\\thetitle}}{\\thepage}"
    } else {
      // author on one page, book title on facing page
      // a good configuration for chapters that don't have titles
      // TODO: different single author configuration for chapters with titles!
      "\\sethead[\\thepage][\\textbf{\\theauthor}][]\n          {}{\\textbf{\\thetitle}}{\\thepage}}"
    }


    // TODO: make this configurable
    val useChapterAuthor = false

    val firstSectionContent = if (useChapterAuthor) {
      firstSection.content
    } else {
      // replace title with formatted title + author name
      // I couldn't figure out how to do this with conditionals due to the nesting
      val withoutTitle = firstSection.content.split("\n").tail.mkString("\n")
      val subtitle = if (config.anthology) {
        "Edited by " + config.editor.getOrElse("")
      } else {
        firstname + " " + lastname
      }

      // derp derp derp
      // val customTitle = (
      //   s"{\\huge\\bfseries\\noindent{$title\\newline\\newline\\large\\textit{$subtitle}}}"
      // )

      val customTitle = s"\\ChapterAuthor{$title}{$subtitle}"

      customTitle + "\n\n" + withoutTitle

    }


    val titlePageSourceFormatted: String = (

      // was originally trying to figure out how to add vspace here

//      // this will align the top to the top margin but can't go beyond zero
//      // s"\\vspace{0in}\n\n" +
//
//      // this aligns from the op of the page exactly
//      // s"\\vspace*{2in} \\vspace{-\\topskip} \\vspace{-0.75in}\n\n" +
//
//      // using vspace* automatically add in topskip as well as the margin!
//      // So we need to subtract it out
//      // for vspace*{0in} to align with the top margin
//      // s"\\vspace*{0in} \\vspace{-\\topskip} \n\n" +

      // Initially I thought this was correct, like the 50pt measurement
      // is from topmargin + topskip.
      // Loking closer, it's close but not quite. Dangit...

      // s"\\vspace*{50pt} \\vspace{\\topskip}\n\n" + firstSectionContent

      // Did some investigation where I set margins to zero in titlespacing like this:
      //  \titlespacing*{\chapter}{0pt}{0pt}{0pt}
      // Possibly easier to see what's going on by enabling rigidchapters on titlesec
      // Seems like by default some vertical space is being inserted before the start
      // of the body text, I don't know where this is coming from.
      // Realized though, I can get the same behavior with ChapterAuthor. See above.

      firstSectionContent

    )

    val titlepage = convert(titlePageSourceFormatted)

    val chapters = remainingSections.map(section => {
      // the first line of the section is the chapter title heading
      val trimmed = section.content.split("\n").tail.mkString("\n")
      val converted = convert(trimmed)
      val convertedStyled = if (config.unstyledSections.contains(section.name)) {
        println("\tnot using indented style for section '" + section.name + "'")
        "{\\parindent0pt\n" + converted + "}\n"
      } else {
        converted
      }

      // prepare chapter title and TOC configuration using LatexOptions
      val chapterTitleStr: String = section.latexOptions.flatMap(_.chapterTitle).getOrElse(section.name)
      val chapterAuthorOp: Option[String] = section.latexOptions.flatMap(_.chapterAuthor) match {
        case Some(x) => Some(x)
        case None => section.author
      }
      val tocTitleStr: String = section.latexOptions.flatMap(_.tocTitle).getOrElse(section.name)
      val tocAuthorOp: Option[String] = section.latexOptions.flatMap(_.tocAuthor) match {
        case Some(x) => Some(x)
        case None => section.author
      }

      // title and author displayed in the chapter
      val chapter = chapterAuthorOp match {
        case Some(x) => s"\\ChapterAuthor{$chapterTitleStr}{$x}\n"
        case None => s"\\chapter*{$chapterTitleStr}\n"
      }

      // title displayed in the TOC
      val titleTOC = s"\\addcontentsline{toc}{chapter}{$tocTitleStr}\n"

      // author displayed in the TOC
      val authorTOC = tocAuthorOp match {
        case Some(x) => s"\\tocdata{toc}{$x}\n"
        case None => ""
      }

      // whether the page headers display the chapter name or not
      // is controlled by tocAuthorOp (latexOptions.tocAuthor / section.author)

      // title displayed in the header
      val titleHeader = tocAuthorOp match {
        case Some(_) => s"\\title{${section.name}}\n"
        case None => s"\\title{$title}\n"
      }

      // author displayed in the header
      val authorHeader = tocAuthorOp match {
        case Some(x) => s"\\author{$x}\n"
        case None => s"\\author{$firstname $lastname}\n"
      }

      (
        chapter +
        authorTOC +    // the order of these two seems important
        titleTOC +
        authorHeader +
        titleHeader +
        convertedStyled
      )

    }).mkString("\n")

    val inputStream = getClass.getResourceAsStream("/latex/template.tex")
    val template = IOUtils.toString(inputStream)
    inputStream.close()

    template.format(
      config.fontSize, config.fontFace,               // 0-1
      config.fixedFontScale, config.fixedFontFace,    // 2-3
      config.paperWidth, config.paperHeight,          // 4-5
      config.marginInner, config.marginOuter,         // 6-7
      config.marginTop, config.marginBottom,          // 8-9
      config.footSkip,                                // 10
      pageStyle,                                      // 11
      title, firstname, lastname,                     // 12-14
      titlepage,                                      // 15
      toc,                                            // 16
      chapters                                        // 17
    )

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
    // val stripped = ExtractRawTags.matcher.replaceAllIn(markdown, _ => "")
    // val stripped = markdown

    // do a multiline match to replace between paragraph breaks and add noindents
    // this is not ideal but the easiest way to handle this for now as far as I can tell
    // the matcher matches all surrounding newlines make it as specific as possible
    val stripped = MarkdownParse.MatcherNbspMulti.replaceAllIn(
        markdown, _ => "\n\n" + BlankLine + "\n\n" + NoIndent + " ")

    // ~~~~ convert per-line symbols

    val linesFixed = stripped.split("\n")

    // ~~~~ convert code blocks and lists

    var result = ""

    val STATE_OUTSIDE = 0
    val STATE_CODEBLOCK = 1
    val STATE_LIST = 2

    var state = STATE_OUTSIDE

    var codeBlockContents = ""
    var listContents = ""
    var listIndentLevel = 0

    linesFixed.foreach(line => {

      // going into a list
      if (state == STATE_OUTSIDE && MarkdownParse.MatcherUL.findFirstIn(line).isDefined) {
        listContents = listContents + ItemizeStart + "\n"
        state = STATE_LIST
        listIndentLevel = 0
      }

      // leaving a list
      if (state == STATE_LIST && MarkdownParse.MatcherUL.findFirstIn(line).isEmpty) {
        (listIndentLevel to 0 by -1).foreach(level => {
          listContents = listContents + "  " * level + ItemizeEnd + "\n"
        })
        result = result + listContents

        state = STATE_OUTSIDE
        listContents = ""
        listIndentLevel = -1
      }

      // into a code block?
      if (state == STATE_OUTSIDE && line.startsWith("    ")) {
        codeBlockContents = codeBlockContents + LstListingStart + "\n"
        state = STATE_CODEBLOCK
      }

      // are we leaving a code block?
      if (state == STATE_CODEBLOCK && !line.startsWith("    ") && !line.isEmpty) {
        // strip trailing whitespace, add back a single newline, and end the code block
        codeBlockContents = codeBlockContents.replaceAll("\\s+$", "")
        codeBlockContents = codeBlockContents + "\n" + LstListingEnd + "\n\n"
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
        val matches = MarkdownParse.MatcherUL.findFirstMatchIn(line)
        matches.foreach(m => {
          val newListIndentLevel = m.group(1).length / 4
          val padding = "  " * newListIndentLevel
          if (newListIndentLevel > listIndentLevel) {
            listContents = listContents + padding + ItemizeStart + "\n"
          } else if (newListIndentLevel < listIndentLevel) {
            listContents = listContents + "  " * listIndentLevel + ItemizeEnd + "\n"
          }
          listIndentLevel = newListIndentLevel

          val listItem = convertLine(m.group(3))

          listContents = listContents + padding + "  " + Item + "  " + listItem + "\n"
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
      // trim trailing whitespace
      codeBlockContents = codeBlockContents.replaceAll("\\s+$", "")
      // end the codeblock
      codeBlockContents = codeBlockContents + "\n" + LstListingEnd + "\n\n"
      result = result + codeBlockContents

    } else if (state == STATE_LIST) {
      (listIndentLevel to 0 by -1).foreach(level => {
        listContents = listContents + "  " * level + ItemizeEnd + "\n"
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

    var res = line

    // single quotes first
    res = MarkdownParse.MatcherSq.replaceAllIn(
      res, m => m.group(1) + LeftSq + m.group(2) + RightSq + m.group(3))

    // Matched pairs of double quotes -> left and right double quotes.
    // Afterwards, replace any remaining double quotes with left double quotes
    // and insert thin spaces between single / quote groups of three

    res = MarkdownParse.MatcherDq.replaceAllIn(res, m => LeftDq + m.group(1) + RightDq)
    res = MarkdownParse.MatcherDqSingle.replaceAllIn(res, _ => LeftDq)
    res = MarkdownParse.MatcherDqSq.replaceAllIn(res, _ => LeftDq + ThinSpace + LeftSq)
    res = MarkdownParse.MatcherSqDq.replaceAllIn(res, _ => RightSq + ThinSpace + RightDq)

    // Matched pairs of *** -> bold and italic
    res = MarkdownParse.MatcherBi.replaceAllIn(res, m => bold(italic(m.group(1))))

    // Matched pairs of ** -> bold
    res = MarkdownParse.MatcherB.replaceAllIn(res, m => bold(m.group(1)))

    // Matched pairs of *  -> italic
    res = MarkdownParse.MatcherI.replaceAllIn(res, m => italic(m.group(1)))

    // res = MatcherH.replaceAllIn(res, m => huge(m.group(1)))
    res = MarkdownParse.MatcherH.replaceAllIn(res, m => hugeBold(m.group(1)))

    // individual symbols
    res = MarkdownParse.MatcherCopyright.replaceAllIn(res, _ => CopyrightSymbol)
    res = MarkdownParse.MatcherPercent.replaceAllIn(res, _ => PercentSign)
    res = MarkdownParse.MatcherEllipsis.replaceAllIn(res, _ => Ellipsis)
    res = MarkdownParse.MatcherNbsp.replaceAllIn(res, _ => BlankLine)
    res = MarkdownParse.MatcherHr.replaceAllIn(res, _ => Rule)

    res = MarkdownParse.MatcherBr.replaceAllIn(res, _ => LineBreak)

    res

  }

  // functions and constants for Latex formatting

  def bold(x: String): String = raw"\\textbf{" + x + "}"
  def italic(x: String): String = raw"\\textit{" + x + "}"
  def huge(x: String): String = raw"{\\huge\\noindent " + x + "}"
  def hugeBold(x: String): String = raw"{\\huge\\noindent \\textbf{" + x + "}}"

  val ItemizeStart = "\\begin{itemize}[nolistsep]"
  val ItemizeEnd = "\\end{itemize}"
  val Item = "\\item"
  val LstListingStart = "\\begin{lstlisting}"
  val LstListingEnd = "\\end{lstlisting}"
  val LeftSq = "`"
  val RightSq = "'"
  val LeftDq = "``"
  val RightDq = "''"
  val ThinSpace = raw"\\thinspace"

  val CopyrightSymbol = raw"\\textcopyright\\"
  val PercentSign = raw"\\%"

  // val Ellipsis = raw" \\ldots\\ "
  val Ellipsis = raw"\\ldots "
  // val Ellipsis = raw"\\thinspace\\ldots\\thinspace "

  val BlankLine = raw"\\vskip\\baselineskip"

  val Rule = raw"\\hfil\\rule{0.25\\textwidth}{0.4pt}\\hfil"

  val LineBreak = raw"\\newline"

  val NoIndent = raw"\\noindent"

}



object MarkdownParse {

  // a very specific case for how section breaks are currently implemented
  val MatcherNbspMulti: Regex = "\\n\\n&nbsp;\\n\\n".r

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
  val MatcherNbsp: Regex = "^&nbsp;$".r

  val MatcherHr: Regex = "^---$".r

  val MatcherBr: Regex = "<br \\/>".r

  val MatcherDqSingle: Regex = "\"".r

  val MatcherUL: Regex = "^(( {4})*)\\* (.*)".r

}
