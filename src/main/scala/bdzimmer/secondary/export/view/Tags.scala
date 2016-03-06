// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Simple functions for generating HTML tags.

// I'm using this in a context where the results will be fed into a markdown
// renderer, so there's some specific things going on with newline usage.

// Ben Zimmer



package bdzimmer.secondary.export.view

import scala.collection.immutable.Seq


object Tags {

  val Column3 = 3
  val Column4 = 4
  val Column6 = 6
  val Column8 = 8
  val Column12 = 12

  val ImageMaxWidthDefault = 480


  def container(body: String): String = {
    val content = """
<div class="container">
  %s
</div>
"""

    content.format(body)
  }


  def jumboTron(body: String): String = {
    val content = """
<div class="jumbotron">
  %s
</div>
"""

    content.format(body)
  }


  def column(size: Int, body: String): String = {
    val content = """
<div class="col-md-%d">
  %s
</div>"""

    content.format(size, body)
  }


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


  def anchor(name: String): String = {
    s"""<a name="${name}"></a>"""
  }


  def image(file: String, responsive: Boolean = false, maxWidth: Int = ImageMaxWidthDefault): String = {
    responsive match {
      // case false => s"""<img src="$file" style="max-width:$maxWidth;height:auto"/>"""
      // TODO: not sure if setting both max-width and max-height is doing what I think
      case false => s"""<img src="$file" style="max-width:${maxWidth}px;max-height:${maxWidth}px;height:auto"/>"""
      case true => s"""<img src="$file" class="img-responsive" />"""
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
      cssClass: Option[String] = None): String = {

    def styleAttribute(style: String): String = {
      s""" style="${style}""""
    }

    val tableRowTags = s"<tr>\n%s</tr>\n"
    val idString = id.map(x => s""" id="${x}"""").getOrElse("")
    val cssClassString = cssClass.map(x => s""" class="${x}"""").getOrElse("")

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

    s"<table%s%s>\n".format(idString, cssClassString) +
    tableHead.map(x => "<thead>\n" + x + "</thead>\n").getOrElse("") +
    "<tbody>\n" + tableBody + "</tbody>\n" +
    "</table>\n"
  }

  val hr = "<hr />"

  val br = "<br />\n"
  val brInline = "<br />"

  val nbsp = "&nbsp;"

}
