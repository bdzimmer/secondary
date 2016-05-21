// Copyright (c) 2016 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.export.controller

import org.scalatest.FunSuite

import bdzimmer.secondary.export.view.Styles


class FontsSuite extends FunSuite {

  test("fonts convert") {
    val good = (Fonts.convert(Styles.FontDescription))
    assert(good._1.length > 0 && good._2.length == 3)

    val bad = (Fonts.convert("Crapola::400,400italic,700,700italic"))
    assert(bad._1.length == 0 && bad._2.length == 0)
  }

}
