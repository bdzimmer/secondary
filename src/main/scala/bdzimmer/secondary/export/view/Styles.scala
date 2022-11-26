// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Functions for managing style sheets for an export.

// 2015-09-07: Created.

// scalastyle:off magic.number

package bdzimmer.secondary.export.view

import java.io.{File, FileOutputStream}
import scala.util.Try

import bdzimmer.util.StringUtils._


object Styles {

  val FontFace = "Libre Baskerville"
  val FontDescription = FontFace.replace(' ', '+') + ":400,400italic,700,700italic"
  val FontFallback = "serif"
  val HeadingSizes = (1 to 6) zip List(42, 36, 30, 24, 18, 14)
  val H1JumbotronSize = 72
  val BodyFontSize = 16
  val BodyLineHeight = 2
  val BodyVerticalMargin = 20


  val CollapsibleLists = """
/* swiveling arrow checkboxes and collapsible lists */

input.swivel[type=checkbox] { display:none; }
input.swivel[type=checkbox] ~ label:after {
  font-family: 'Glyphicons Halflings';
  font-size: 12px;
  display: inline-block;
  margin-left: 10px;
}
input.swivel[type=checkbox] ~ label:after { content: "\e258"; }
input.swivel[type=checkbox]:checked ~ label:after { content: "\e259"; }

li.swivel > input ~ ul { display: none; }
li.swivel > input:checked ~ ul { display: block; }
li.swivel label { margin-bottom: 0px; }
"""


  def styleSheet(): String = {

    val headingSizeStyle = HeadingSizes.map({case (level, size) => {
      s"""h${level}, .h${level} {font-size: ${size}px}"""
    }}).mkString("\n")


    s"""
/* Copyright (c) 2018 Ben Zimmer. All rights reserved. */

/* Set a custom font and increase the font size for everything. */

h1, h2, h3, h4, h5, h6, .h1, .h2, .h3, .h4, .h5, .h6 {
  font-family: '${FontFace}', ${FontFallback};
}

h1 a, h2 a, h3 a, h4 a, h5 a, h6 a, .h1 a, .h2 a, .h3 a, .h4 a, .h5 a, .h6 a {
  color: inherit;
}

${headingSizeStyle}

h4, .h4 {
  border-bottom: 1px solid #eee;
  margin: 20px 0px 20px 0px;
  padding: 20px 0px 20px 0px;
}

.jumbotron h1, .jumbotron .h1 {
  font-size: ${H1JumbotronSize}px;
}

p, div {
  font-family: '${FontFace}', ${FontFallback};
  font-size: ${BodyFontSize}px;
}

p {
  /* margin: ${BodyVerticalMargin}px 0px ${BodyVerticalMargin}px 0px; */
  margin: 0px 0px ${BodyVerticalMargin}px 0px;
}

p.sidenote {
  font-size: 12px;
  text-indent: 0px;
  margin: 0px 0px 15px 0px;
}

body {
  line-height: ${BodyLineHeight};
}
""" + CollapsibleLists

  }


  val BookStyle = """
  /* default paragraph style is not indented */
  p {
    margin: 0px 0px 0px 0px;
    text-indent: 0px;
  }

  /* paragraphs after other (non-empty) paragraphs
  are indented, as long as they are not the special
  noindent class */
  p:not(.empty) + p:not(.noindent) {
    margin: 0px 0px 0px 0px;
    text-indent: 40px;
  }

  /* special classes for forcing indentation */
  p.noindent {
    margin: 0px 0px 0px 0px;
    text-indent: 0px;
  }
  p.indent {
    margin: 0px 0px 0px 0px;
    text-indent: 40px;
  }
"""


}
