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
      contents: Seq[Seq[String]],
      tdStyle: Seq[String],
      id: Option[String]): String = {

    val tableTags = s"<table%s>\n%s\n</table>\n"
    val tableRowTags = s"<tr>\n%s\n</tr>"
    val idString = id.map(x => s""" id="${x}"""").getOrElse("")

    if (tdStyle.length > 0) {
      tableTags.format(idString, contents.map(row => {
        tableRowTags.format(row.zip(tdStyle).map({case (cell, style) => {
          s"""<td style="${style}">${cell}</td>"""
        }}).mkString)
      }).mkString)

    } else {
      tableTags.format(idString, contents.map(row => {
        tableRowTags.format(row.map(cell => {
          s"""<td>${cell}</td>"""
        }).mkString)
      }).mkString)
    }

  }

  val hr = "<hr />"

  val br = "<br />\n"
  val brInline = "<br />"

  val nbsp = "&nbsp;"

}
