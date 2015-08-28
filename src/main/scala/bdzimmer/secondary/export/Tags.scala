// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Page templates. Originally part of the old "Bootstrap" class.
// Ben Zimmer

// 2015-08-11: Created in refactor.

package bdzimmer.secondary.export


object Tags {



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


  def listItem(item: String): String = {
    """  <li>%s</li>""".format(item)
  }


  def link(text: String, link: String): String = {
    """  <a href="%s">%s</a>""".format(link, text)
  }

  def image(file: String, responsive: Boolean = false): String = {
    (responsive match {
      case false => """<img src="%s" />"""
      case true => """<img src="%s" class="img-responsive" />"""
    }).format(file)
  }


  def imageSprite(file: String, x: Int, y: Int, width: Int, height: Int): String = {
    """<div style= "background-image:url('%s');background-repeat:no-repeat;background-position: %d %d;width:%dpx;height:%dpx;"></div>""".format(file, x, y, width, height)
  }


  def centered(text: String): String = {
    """<p class="text-center">%s</p>""".format(text)
  }


  def hr(): String = {
    """<hr />"""
  }

  def br(): String = {
    """<br />"""
  }
}
