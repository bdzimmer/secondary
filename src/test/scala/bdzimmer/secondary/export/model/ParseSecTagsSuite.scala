// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Tests for ParseSecTags.

package bdzimmer.secondary.export.model

import org.scalatest.FunSuite

class ParseSecTagsSuite extends FunSuite {

  test("tag parsing") {

    // if no kind is given, default to link
    assert(ParseSecTags.getTag("Default Link")
        .equals(SecTag("link", "Default Link", List())))

    // kind given, no parameters
    assert(ParseSecTags.getTag("familytree: Billy Bob")
        .equals(SecTag("familytree", "Billy Bob", List())))

    // kind with parameters
    assert(ParseSecTags.getTag("jumbotron: Baloney Photo | color=white | xpos=0% | ypos=50%")
        .equals(SecTag("jumbotron", "Baloney Photo", List("color=white", "xpos=0%", "ypos=50%"))))

  }

}
