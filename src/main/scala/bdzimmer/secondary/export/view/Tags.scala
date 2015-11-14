// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Page templates. Originally part of the old "Bootstrap" class.
// Ben Zimmer

// 2015-08-11: Created in refactor.

package bdzimmer.secondary.export.view


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


  def listGroup(items: List[String]): String = {
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
    """<div style= "background-image:url('%s');background-repeat:no-repeat;background-position: %d %d;width:%dpx;height:%dpx;"></div>""".format(file, x, y, width, height)
  }

  def centered(text: String): String = {
    """<p class="text-center">%s</p>""".format(text)
  }

  def p(text: String): String = {
    s"<p>${text}</p>"
  }

  val hr = """<hr />"""

  val br = """<br />"""

}
