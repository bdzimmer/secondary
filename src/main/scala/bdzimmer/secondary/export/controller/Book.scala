// Copyright (c) 2019 Ben Zimmer. All rights reserved.

// Common functionality for book generation (EPUB or LaTeX).

package bdzimmer.secondary.export.controller

import bdzimmer.secondary.`export`.view.Markdown
import bdzimmer.secondary.export.model.Tags
import bdzimmer.util.StringUtils._


object Book {

  case class LatexOptions(
    tocTitle: Option[String],
    tocAuthor: Option[String],
    chapterTitle: Option[String],
    chapterAuthor: Option[String]
  )

  case class SectionInfo(
    id: String,
    name: String,
    author: Option[String],
    latexOptions: Option[LatexOptions],
    content: String
  )

  case class BookConfig(
    fontSize: String,
    fontFace: String,
    fixedFontScale: String,
    fixedFontFace: String,
    unstyledSections: Set[String],
    paperWidth: String,
    paperHeight: String,
    marginInner: String,
    marginOuter: String,
    marginTop: String,
    marginBottom: String,
    footSkip: String,
    toc: Boolean,
    editor: Option[String],
    anthology: Boolean
  ) {
    override def toString: String = {
      "fontSize:         " + fontSize + "\n" +
      "fontFace:         " + fontFace + "\n" +
      "fixedFontScale:   " + fixedFontScale + "\n" +
      "fixedFontFace:    " + fixedFontFace + "\n" +
      "unstyledSections: " + unstyledSections + "\n" +
      "paperWidth:       " + paperWidth + "\n" +
      "paperHeight:      " + paperHeight + "\n" +
      "marginInner:      " + marginInner + "\n" +
      "marginOuter:      " + marginOuter + "\n" +
      "marginTop:        " + marginTop + "\n" +
      "marginBottom:     " + marginBottom + "\n" +
      "footSkip:         " + footSkip + "\n" +
      "toc:              " + toc + "\n" +
      "editor:           " + editor + "\n" +
      "anthology:        " + anthology
    }
  }

  // ~~~~ ~~~~ ~~~~ ~~~~

  val BookConfigDefault: BookConfig = BookConfig(
    fontSize = "12pt",
    fontFace = "ebgaramond",
    fixedFontScale = "0.75",
    fixedFontFace = "DejaVuSansMono",
    unstyledSections = Set(),
    paperWidth = "5.25in",
    paperHeight = "8in",
    marginInner = "0.75in",
    marginOuter = "0.5in",
    marginTop = "0.5in",
    marginBottom = "0.5in",
    footSkip = "0.2in",
    toc = false,
    editor = None,
    anthology = false
  )

  // ~~~~ ~~~~ ~~~~ ~~~~

  // Split the notes of a book into sections, rendering the notes.
  // Returns:
  // - list of SectionInfo objects
  // - Option that may contain the book's cover image if it is present
  def sections(
      book: String,
      tags: Map[Int, Tags.ParsedTag],
      rtOption: Option[RenderTags],
      markdown: Boolean
      ): (List[SectionInfo], Option[Tags.Image]) = {

    val emptyParagraphMatcher = "<p>&nbsp;<\\/p>".r

    val (titles, chunks) = splitSections(book)

    val contents = chunks.map({case (startIdx, endIdx, chunk) => {

      val chunk_t = rtOption.map(rt => {
        val tagsMod = tags.map(x => (x._1 - startIdx, x._2))
        val chunk_t_t = rt.transformTagsOnly(chunk, tagsMod)
        if (markdown) {
          // ebook mode
          val res = Markdown.process(chunk_t_t, ebookMode = true)
          emptyParagraphMatcher.replaceAllIn(res, "<p class=\"empty\">&nbsp;</p>")
        } else {
          // print mode
          chunk_t_t
        }
      }).getOrElse(chunk)

      // find all config tags
      val configTags: List[Tags.Config] = tags
          .filter(x => x._1 >= startIdx && x._1 < endIdx)
          .values
          .collect({case x: Tags.Config => x}).toList

      // get author from the first "Chapter" config in this section
      val author: Option[String] = configTags
          .find(_.desc.startsWith("Chapter"))
          .flatMap(_.args.get("author"))

      // get latex options form the first "Latex" config in this section
      val latexOptions: Option[LatexOptions] = configTags
          .find(_.desc.startsWith("Latex"))
          .map(x => LatexOptions(
            tocTitle = x.args.get("toctitle"),
            tocAuthor = x.args.get("tocauthor"),
            chapterTitle = x.args.get("chaptertitle"),
            chapterAuthor = x.args.get("chapterauthor")
            ))

      (chunk_t, author, latexOptions)
    }})

    val sections = titles.zipWithIndex.zip(contents).map({case ((secTitle, secNumber), (secContent, secAuthor, secLatexOptions)) => {
      val secAuthorStr = secAuthor.getOrElse("(None)")
      println(s"${secTitle} - ${secAuthorStr}")
      secLatexOptions.foreach(x => println(s"\tLatex options: ${x}"))
      SectionInfo("section" + secNumber.toString, secTitle, secAuthor, secLatexOptions, secContent)
    }})

    // find all image tags
    val imageTags = tags.filter(x => x._2.isInstanceOf[Tags.Image]).collect({case x: (Int, Tags.Image) => x})

    // first image tag in first section is cover image
    val image = for {
      coverPageRange <- chunks.headOption.map(x => (x._1, x._2))
      coverImageTag <- imageTags.find(x => x._1 >= coverPageRange._1 && x._1 < coverPageRange._2)
    } yield {
      coverImageTag._2
    }

    image match {
      case Some(_) => (sections.tail, image)
      case None    => (sections, None)
    }
  }


  // Split the notes of a book into sections.
  // Returns:
  // - list of section titles
  // - list of tuples of start position, end position, and section contents
  def splitSections(book: String): (List[String], List[(Int, Int, String)]) = {

    val matcher = "\\#+ (.*)(\\r\\n|\\r|\\n)".r

    val matches = matcher.findAllMatchIn(book).map(m => (m.start, m.end, m.group(1))).toList

    // use _._2 if we want to exclude the chapter headings from the contents
    val allPositions = matches.map(_._1) ++ List(book.length)
    val chunkRanges = allPositions.sliding(2).map(x => (x(0), x(1))).toList
    val chunks = chunkRanges.map(x => (x._1, x._2, book.substring(x._1, x._2)))
    val titles = matches.map(_._3)

    (titles, chunks)
  }

  // Get configuration from a book (the first config tag whose description begins with "Book")
  def getConfig(
         tags: Map[Int, Tags.ParsedTag]): BookConfig = {

    val args: Map[String, String] = tags
        .values
        .collect({case x: Tags.Config => x}).toList
        .find(_.desc.startsWith("Book"))
        .map(_.args)
        .getOrElse(Map())

    val unstyledSections = args.get("unstyledsections").map(_.split(";\\s+").toSet)

    BookConfig(
      fontSize     = args.getOrElse("fontsize",     BookConfigDefault.fontSize),
      fontFace     = args.getOrElse("fontface",     BookConfigDefault.fontFace),
      fixedFontScale = args.getOrElse("fixedfontscale", BookConfigDefault.fixedFontScale),
      fixedFontFace  = args.getOrElse("fixedfontface",  BookConfigDefault.fixedFontFace),
      unstyledSections = unstyledSections.getOrElse(BookConfigDefault.unstyledSections),
      paperWidth   = args.getOrElse("paperwidth",   BookConfigDefault.paperWidth),
      paperHeight  = args.getOrElse("paperheight",  BookConfigDefault.paperHeight),
      marginInner  = args.getOrElse("margininner",  BookConfigDefault.marginInner),
      marginOuter  = args.getOrElse("marginouter",  BookConfigDefault.marginOuter),
      marginTop    = args.getOrElse("margintop",    BookConfigDefault.marginTop),
      marginBottom = args.getOrElse("marginbottom", BookConfigDefault.marginBottom),
      footSkip     = args.getOrElse("footskip",     BookConfigDefault.footSkip),
      toc          = args.get("toc").map(_.toBooleanSafe).getOrElse(BookConfigDefault.toc),
      editor       = args.get("editor"),
      anthology    = args.get("anthology").map(_.toBooleanSafe).getOrElse(BookConfigDefault.anthology)
    )
  }


}
