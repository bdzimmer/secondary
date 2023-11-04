// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Simple functions for generating HTML tags.

// I'm using this in a context where the results will be fed into a markdown
// renderer, so there's some specific things going on with newline usage.

package bdzimmer.secondary.export.view

import scala.collection.immutable.Seq


object Html {

  val ImageMaxWidthDefault: Int = 480
  val ImageBgColorDefault: String = "#404040FF"


  def listGroup(items: Seq[String]): String = {
    """<ul>""" + "\n" + items.mkString("\n") +  """</ul>"""
  }


  def listItem(item: String, className: String = ""): String = {
    val classAttr = if (className.equals("")) {
      className
    } else {
      s""" class="${className}""""
    }

    s"""<li${classAttr}>${item}</li>"""
  }


  def link(text: String, link: String): String = {
    """<a href="%s">%s</a>""".format(link, text)
  }


  // TODO: maybe a better name for this?
  def anchor(desc: String, id: String): String = {
     """<span id="%s">%s</span>""".format(id, desc)
  }


  def image(
      file: String,
      responsive: Boolean = false,
      maxWidth: Int = ImageMaxWidthDefault,
      bgColor: Option[String] = None
      ): String = {

    val bgColorStyle = bgColor.map(x => s"background-color:$x").getOrElse("")

    if (!responsive) {
      val nonResponsiveStyle = s"max-width:${maxWidth}px;max-height:${maxWidth}px;height:auto"
      s"""<img src="$file" style="$nonResponsiveStyle;$bgColorStyle" />"""
    } else {
      s"""<img src="$file" class="img-responsive" style="$bgColorStyle" />"""
    }
  }

  def imageSprite(file: String, x: Int, y: Int, width: Int, height: Int): String = {
    val style = s"background-image:url('${file}');background-repeat:no-repeat;background-position: ${x} ${y};width:${width};height:${height};"
    s"""<div style="${style}"></div>"""
  }

  def centered(text: String): String = {
    """<p class="text-center">%s</p>""".format(text)
  }

  def p(text: String): String = {
    s"<p>${text}</p>"
  }

  def b(text: String): String = {
    s"<b>${text}</b>"
  }

  def h4(text: String): String = {
    s"<h4>${text}</h4>\n"
  }

  // TODO: probably do something different with styles
  def table(
      head: Option[Seq[String]],
      body: Seq[Seq[String]],
      tdStyle: Option[Seq[String]],
      id: Option[String],
      cssClass: Option[String],
      tableStyle: Option[String],
      theadStyle: Option[String]): String = {

    def styleAttribute(style: String): String = {
      s""" style="${style}""""
    }

    val tableRowTags = s"<tr>\n%s</tr>\n"
    val idString = id.map(x => s""" id="${x}"""").getOrElse("")
    val cssClassString = cssClass.map(x => s""" class="${x}"""").getOrElse("")
    val tableStyleString = tableStyle.map(styleAttribute).getOrElse("")
    val theadStyleString = theadStyle.map(styleAttribute).getOrElse("")

    val headStyled = tdStyle match {
      case Some(x) => head.map(row => row.zip(x.map(styleAttribute)))
      case None => head.map(row => row.map((_, "")))
    }

    val bodyStyled = tdStyle match {
      case Some(x) => body.map(row => row.zip(x.map(styleAttribute)))
      case None => body.map(row => row.map((_, "")))
    }

    val tableBody = bodyStyled.map(row => {
      tableRowTags.format(row.map({case (cell, style) => {
        s"""<td${style}>${cell}</td>\n"""
      }}).mkString)
    }).mkString

    val tableHead = headStyled.map(row => {
      tableRowTags.format(row.map({case (cell, style) => {
        s"""<td${style}>${cell}</td>\n"""
      }}).mkString)
    })

    s"<table${idString}${cssClassString}${tableStyleString}>\n" +
    tableHead.map(x => s"<thead${theadStyleString}>\n" + x + "</thead>\n").getOrElse("") +
    "<tbody>\n" + tableBody + "</tbody>\n" +
    "</table>\n"
  }

  val hr = "<hr />"

  val br = "<br />\n"
  val brInline = "<br />"

  val nbsp = "&nbsp;"

}
