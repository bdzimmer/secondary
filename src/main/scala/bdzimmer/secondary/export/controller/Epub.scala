// Copyright (c) 2018 Ben Zimmer. All rights reserved.

// Generate EPUB files.

// Based on examples from: https://en.wikipedia.org/wiki/EPUB

package bdzimmer.secondary.export.controller

import scala.collection.immutable.Seq

import java.io.{FileOutputStream, BufferedOutputStream}
import java.util.zip.{ZipOutputStream, ZipEntry, CRC32}
import org.apache.commons.io.IOUtils

import bdzimmer.secondary.export.model.Tags

object Epub {

  case class SectionInfo(
    id: String,
    name: String,
    content: String
  )

  def sections(book: String, tags: Map[Int, Tags.ParsedTag], rt: RenderTags): List[SectionInfo] = {

    val matcher = "\\#+ (.*)(\\r\\n|\\r|\\n)".r

    val matches = matcher.findAllMatchIn(book).map(m => (m.start, m.end, m.group(1))).toList

    // use _._2 if we want to exclude the chapter headings from the contents
    val allPositions = matches.map(_._1) ++ List(book.length - 1)
    val chunks = allPositions.sliding(2).map(x => (x(0), book.substring(x(0), x(1) - 1)))

    val contents = chunks.map({case (startIdx, chunk) => {
      val tagsMod = tags.map(x => (x._1 - startIdx, x._2))
      rt.transform(chunk, tagsMod)
    }}).toList

    matches.map(_._3).zipWithIndex.zip(contents).map(x =>
      SectionInfo(x._1._2.toString, x._1._1, "<body>" + x._2 + "</body>"))
  }


  val Mimetype = "application/epub+zip"

  val ContainerXml =
"""
<?xml version="1.0" encoding="UTF-8" ?>
<container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container">
  <rootfiles>
    <rootfile full-path="OEBPS/content.opf" media-type="application/oebps-package+xml"/>
  </rootfiles>
</container>
"""

  def formatContentOpf(
      uniqueIdentifier: String,
      title: String,
      firstname: String,
      lastname: String,
      sections: Seq[SectionInfo]
    ): String = {

    // format contents of content.opf file

    val manifestItems = sections.map(section => {
      s"""<item id="${section.id}" href="${section.id}.xhtml" media-type="application/xhtml+xml"/>"""
    })

    val manifest =
      "<manifest>" +
        manifestItems.mkString("") +
      """<item id="ncx" href="toc.ncx" media-type="application/x-dtbncx+xml"/>""" +
      "</manifest>"

    val spineItems = sections.map(section => {
      s"""<itemref idref="${section.id}" />"""
    })

    val spine =
      """<spine toc="ncx">""" +
      spineItems.mkString("") +
      """</spine>"""

s"""
<?xml version="1.0"?>
<package version="2.0" xmlns="http://www.idpf.org/2007/opf" unique-identifier="$uniqueIdentifier">

  <metadata xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:opf="http://www.idpf.org/2007/opf">
    <dc:title>$title</dc:title>
    <dc:language>en</dc:language>
    <dc:identifier id="$uniqueIdentifier" opf:scheme="NotISBN">$uniqueIdentifier</dc:identifier>
    <dc:creator opf:file-as="$lastname, $firstname" opf:role="aut">$firstname $lastname</dc:creator>
  </metadata>

  $manifest

  $spine

</package>"""

  }


  def formatTocNcx(
    uniqueIdentifier: String,
    title: String,
    firstname: String,
    lastname: String,
    sections: Seq[SectionInfo]): String = {

    // format contents of toc.ncx file"""

    val navmapItems = sections.zipWithIndex.map(x => {
      s"""<navPoint class="chapter" id="${x._1.id}" playOrder="${x._2}">""" +
      s"""<navLabel><text>${x._1.name}</text></navLabel>""" +
      s"""<content src="${x._1.id}.xhtml"/>""" +
      s"""</navPoint>"""
    })

    val navmap =
      "<navMap>" +
        navmapItems.mkString("") +
        "</navMap>"

    s"""
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE ncx PUBLIC "-//NISO//DTD ncx 2005-1//EN"
"http://www.daisy.org/z3986/2005/ncx-2005-1.dtd">

<ncx version="2005-1" xml:lang="en" xmlns="http://www.daisy.org/z3986/2005/ncx/">

  <head>
<!-- The following four metadata items are required for all NCX documents,
including those that conform to the relaxed constraints of OPS 2.0 -->

    <meta name="dtb:uid" content="$uniqueIdentifier"/> <!-- same as in .opf -->
    <meta name="dtb:depth" content="1"/> <!-- 1 or higher -->
    <meta name="dtb:totalPageCount" content="0"/> <!-- must be 0 -->
    <meta name="dtb:maxPageNumber" content="0"/> <!-- must be 0 -->
  </head>

  <docTitle>
    <text>$title</text>
  </docTitle>

  <docAuthor>
    <text>$lastname, $firstname</text>
  </docAuthor>

  $navmap

</ncx>
"""

  }

  def export(
      outputFilename: String,
      uniqueIdentifier: String,
      title: String,
      firstname: String,
      lastname: String,
      sections: Seq[SectionInfo]): Unit = {

    val contentOpf = formatContentOpf(
      uniqueIdentifier,
      title,
      firstname,
      lastname,
      sections)

    val tocNcx = formatTocNcx(
      uniqueIdentifier,
      title,
      firstname,
      lastname,
      sections)

    val fout = new FileOutputStream(outputFilename)
    val bout = new BufferedOutputStream(fout)
    val zout = new ZipOutputStream(bout)

    val mimetypeEntry = new ZipEntry("mimetype")
    mimetypeEntry.setMethod(ZipEntry.STORED)
    mimetypeEntry.setSize(Mimetype.length)
    mimetypeEntry.setCompressedSize(Mimetype.length)
    val crc = new CRC32()
    crc.update(Mimetype.getBytes)
    mimetypeEntry.setCrc(crc.getValue)
    zout.putNextEntry(mimetypeEntry)
    IOUtils.write(Mimetype, zout)
    zout.closeEntry()

    zout.putNextEntry(new ZipEntry("META-INF/container.xml"))
    IOUtils.write(ContainerXml, zout)
    zout.closeEntry()

    zout.putNextEntry(new ZipEntry("OEBPS/content.opf"))
    IOUtils.write(contentOpf, zout)
    zout.closeEntry()

    zout.putNextEntry(new ZipEntry("OEBPS/toc.ncx"))
    IOUtils.write(tocNcx, zout)
    zout.closeEntry()

    for (section <- sections) {
      zout.putNextEntry(new ZipEntry("OEBPS/" + section.id + ".xhtml"))
      IOUtils.write(section.content, zout)
      zout.closeEntry()
    }

    zout.close()

  }

}
