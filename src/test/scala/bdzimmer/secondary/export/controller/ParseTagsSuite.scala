// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Tests for ParseSecTags.

package bdzimmer.secondary.export.controller

import org.scalatest.FunSuite

import bdzimmer.secondary.export.model.Tags.RawTag

class ParseTagsSuite extends FunSuite {

  test("extract raw tags") {

    // if no kind is given, default to link
    assert(ExtractRawTags.getTag("Default Link")
        .equals(RawTag("link", "Default Link", List())))

    // kind given, no parameters
    assert(ExtractRawTags.getTag("familytree: Billy Bob")
        .equals(RawTag("familytree", "Billy Bob", List())))

    // kind with parameters
    assert(ExtractRawTags.getTag("jumbotron: Baloney Photo | color=white | xpos=0% | ypos=50%")
        .equals(RawTag("jumbotron", "Baloney Photo", List("color=white", "xpos=0%", "ypos=50%"))))

  }

}
