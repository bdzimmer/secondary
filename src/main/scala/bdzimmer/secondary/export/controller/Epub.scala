// Copyright (c) 2018 Ben Zimmer. All rights reserved.

// Generate EPUB files.

// Based on examples from: https://en.wikipedia.org/wiki/EPUB

package bdzimmer.secondary.export.controller

import scala.collection.immutable.Seq

import java.io.{File, FileOutputStream, BufferedOutputStream}
import java.util.zip.{ZipOutputStream, ZipEntry, CRC32}
import org.apache.commons.io.{FileUtils, IOUtils}

import bdzimmer.util.StringUtils._

import bdzimmer.secondary.export.model.{WorldItems}
import bdzimmer.secondary.export.model.Tags.ParsedTag
import bdzimmer.secondary.export.model.WorldItems.BookItem
import bdzimmer.secondary.export.view.Styles
import bdzimmer.secondary.export.controller.Book.SectionInfo


object Epub {

  def coverPage(imageFilename: String): String = {
    //s"""<body><img id="coverimage" src="$imageFilename" alt="cover image" /></body>"""
    val imageTag = s"""<img id="coverimage" src="$imageFilename" alt="cover image" />"""
    page("Cover", imageTag, false)
  }

  def page(title: String, content: String, chapter: Boolean): String = {

    val chapterStyle = if (chapter) {
      s"""<style type="text/css">${Styles.BookStyle}</style>"""
    } else {
      ""
    }

s"""<?xml version="1.0" encoding="UTF-8" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
  <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
    <head>
      <meta http-equiv="Content-Type" content="application/xhtml+xml; charset=utf-8" />
      <title>$title</title>
      $chapterStyle
    </head>
    <body>
      $content
    </body>
  </html>
"""
  }


  def authorNameParts(authorname: String): (String, String) = {
    val (name, parts) = WorldItems.cleanName(authorname)
    parts match {
      case None => ("", name)
      case Some(ps) => ps match {
        case fst :: rest => (fst, rest.mkString(" "))
        case _           => ("", name)
      }
    }
  }

  val Mimetype = "application/epub+zip"

  val ContainerXml =
"""<?xml version="1.0" encoding="UTF-8" ?>
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
      sections: Seq[SectionInfo],
      coverImageFilename: Option[String]
    ): String = {

    // format contents of content.opf file

    val cover = if (coverImageFilename.isDefined) {
      "<meta name=\"cover\" content=\"cover-image\"/>"
    } else {
      ""
    }

    val manifestItems = sections.map(section => {
      s"""<item id="${section.id}" href="${section.id}.xhtml" media-type="application/xhtml+xml"/>"""
    }) ++ coverImageFilename.map(x => {
      // assumes cover image is png
      s"""<item id="cover-image" href="$x" media-type="image/png"/>"""
    }).toList

    val manifest =
      "<manifest>\n" +
        manifestItems.mkString("\n") +
      """<item id="ncx" href="toc.ncx" media-type="application/x-dtbncx+xml"/>""" +
      "\n</manifest>"

    val spineItems = sections.map(section => {
      s"""<itemref idref="${section.id}" />"""
    })

    val spine =
      """<spine toc="ncx">""" +
      spineItems.mkString("") +
      """</spine>"""

s"""<?xml version="1.0"?>
<package version="2.0" xmlns="http://www.idpf.org/2007/opf" unique-identifier="$uniqueIdentifier">

  <metadata xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:opf="http://www.idpf.org/2007/opf">
    <dc:title>$title</dc:title>
    <dc:language>en</dc:language>
    <dc:identifier id="$uniqueIdentifier" opf:scheme="NotISBN">$uniqueIdentifier</dc:identifier>
    <dc:creator opf:file-as="$lastname, $firstname" opf:role="aut">$firstname $lastname</dc:creator>
    $cover
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


s"""<?xml version="1.0" encoding="UTF-8"?>
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
      filename: String,
      book: BookItem,
      tags: Map[Int, ParsedTag],
      renderTags: RenderTags,
      localExportPath: String): Unit = {

    val (sections, coverImageTag) = Book.sections(book.notes, tags, Some(renderTags))
    // title is name of first section
    val title = sections.headOption.map(_.name).getOrElse("empty")
    val titlePage = sections.headOption.map(_.copy(name="Title Page"))
    // replace empty section names with "Content"
    val contentSections = sections.tail.map(x => if (x.name.equals("---")) x.copy(name="Content") else x)

    val (firstname, lastname) = Epub.authorNameParts(book.authorname)

    // cover page becomes new first section if cover image exists
    // WIP: experimenting with disabling cover page; I think it causes a duplicate cover on KDP
    // But it might be necessary to enable for Kobo.

    // val cover = coverImageTag.map(
    //   x => Epub.SectionInfo("cover", "Cover", Epub.coverPage(RenderImages.itemImagePath(x.item))))

    Epub.export(
      filename,
      book.uniqueIdentifier,
      title,
      firstname,
      lastname,
      // cover.toList ++ titlePage.toList ++ contentSections,
      titlePage.toList ++ contentSections,
      coverImageTag.map(x => RenderImages.itemImagePath(x.item)),
      localExportPath)
  }


  def export(
      outputFilename: String,
      uniqueIdentifier: String,
      title: String,
      firstname: String,
      lastname: String,
      sections: Seq[SectionInfo],
      coverImageFilename: Option[String],
      imageDirname: String): Unit = {

    // expects the first page to be the title page
    // and remaining pages prose

    val contentOpf = formatContentOpf(
      uniqueIdentifier,
      title,
      firstname,
      lastname,
      sections,
      coverImageFilename)

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
    IOUtils.write(Mimetype, zout, "UTF-8")
    zout.closeEntry()

    zout.putNextEntry(new ZipEntry("META-INF/container.xml"))
    IOUtils.write(ContainerXml, zout, "UTF-8")
    zout.closeEntry()

    zout.putNextEntry(new ZipEntry("OEBPS/content.opf"))
    IOUtils.write(contentOpf, zout, "UTF-8")
    zout.closeEntry()

    zout.putNextEntry(new ZipEntry("OEBPS/toc.ncx"))
    IOUtils.write(tocNcx, zout, "UTF-8")
    zout.closeEntry()

    sections.zipWithIndex.foreach({case (section, idx) => {
      zout.putNextEntry(new ZipEntry("OEBPS/" + section.id + ".xhtml"))
      IOUtils.write(
        page(section.name, section.content, idx > 0),
        zout, "UTF-8")
      zout.closeEntry()
    }})

    // cover image
    coverImageFilename.foreach(x => {
      zout.putNextEntry(new ZipEntry("OEBPS/" + x))
      FileUtils.copyFile(new File(imageDirname / x), zout)
      zout.closeEntry()
    })

    zout.close()

  }

}
