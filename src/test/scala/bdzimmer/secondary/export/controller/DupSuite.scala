// Copyright (c) 2019 Ben Zimmer. All rights reserved.

// Tests for duplicate word detection.

package bdzimmer.secondary.export.controller

import org.scalatest.FunSuite


class DupSuite extends FunSuite{

  test("duplicate") {
    val testLines = "the quick quick brown\nfox jumps over\nthe the lazy dog dog\nThat's something.."
    val lineNumbers = Dup.find(testLines)
    assert(lineNumbers == List((0, (4, 15)), (2, (0, 7)), (2, (13, 20))))
  }

}
