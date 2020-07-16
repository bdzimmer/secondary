// Copyright (c) 2019 Ben Zimmer. All rights reserved.

// Common functionality for book generation (EPUB or LaTeX).

package bdzimmer.secondary.export.controller

import bdzimmer.secondary.export.model.Tags


object Book {

  case class SectionInfo(
    id: String,
    name: String,
    content: String
  )

  case class BookConfig(
    unstyledSections: Set[String],
    paperWidth: String,
    paperHeight: String,
    marginInner: String,
    marginOuter: String,
    marginTop: String,
    marginBottom: String
  ) {
    override def toString: String = {
      "unstyledSections: " + unstyledSections + "\n" +
      "paperWidth:       " + paperWidth + "\n" +
      "paperHeight:      " + paperHeight + "\n" +
      "marginInner:      " + marginInner + "\n" +
      "marginOuter:      " + marginOuter + "\n" +
      "marginTop:        " + marginTop + "\n" +
      "marginBottom:     " + marginBottom + "\n"
    }
  }

  // ~~~~ ~~~~ ~~~~ ~~~~

  val BookConfigDefault: BookConfig = BookConfig(
    unstyledSections = Set(),
    paperWidth = "5.25in",
    paperHeight = "8in",
    marginInner = "0.75in",
    marginOuter = "0.5in",
    marginTop = "0.5in",
    marginBottom = "0.5in"
  )

  // ~~~~ ~~~~ ~~~~ ~~~~

  // Split the notes of a book into sections, rendering the notes.
  // Returns:
  // - list of SectionInfo objects
  // - Option that may contain the book's cover image if it is present
  def sections(
      book: String,
      tags: Map[Int, Tags.ParsedTag],
      rtOption: Option[RenderTags]): (List[SectionInfo], Option[Tags.Image]) = {

    val (titles, chunks, chunkRanges) = splitSections(book)

    val contents = chunks.map({case (startIdx, chunk) => {
      rtOption.map(rt => {
        val tagsMod = tags.map(x => (x._1 - startIdx, x._2))
        rt.transform(chunk, tagsMod)
      }).getOrElse(chunk)
    }})

    val sections = titles.zipWithIndex.zip(contents).map({case ((secTitle, secNumber), secContent) => {
      SectionInfo("section" + secNumber.toString, secTitle, secContent)
    }})

    // find all image tags
    val imageTags = tags.filter(x => x._2.isInstanceOf[Tags.Image]).collect({case x: (Int, Tags.Image) => x})

    // first image tag in first section is cover image
    val image = for {
      coverPageRange <- chunkRanges.headOption
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
  // - list of tuples of start position and section contents
  // - list of tuples of section start and end position
  def splitSections(book: String): (List[String], List[(Int, String)], List[(Int, Int)]) = {

    val matcher = "\\#+ (.*)(\\r\\n|\\r|\\n)".r

    val matches = matcher.findAllMatchIn(book).map(m => (m.start, m.end, m.group(1))).toList

    // use _._2 if we want to exclude the chapter headings from the contents
    val allPositions = matches.map(_._1) ++ List(book.length)
    val chunkRanges = allPositions.sliding(2).map(x => (x(0), x(1))).toList
    val chunks = chunkRanges.map(x => (x._1, book.substring(x._1, x._2)))
    val titles = matches.map(_._3)

    (titles, chunks, chunkRanges)
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
      unstyledSections = unstyledSections.getOrElse(BookConfigDefault.unstyledSections),
      paperWidth   = args.getOrElse("paperwidth",   BookConfigDefault.paperWidth),
      paperHeight  = args.getOrElse("paperheight",  BookConfigDefault.paperHeight),
      marginInner  = args.getOrElse("margininner",  BookConfigDefault.marginInner),
      marginOuter  = args.getOrElse("marginouter",  BookConfigDefault.marginOuter),
      marginTop    = args.getOrElse("margintop",    BookConfigDefault.marginTop),
      marginBottom = args.getOrElse("marginbottom", BookConfigDefault.marginBottom)
    )
  }


}
