// Copyright (c) 2018 Ben Zimmer. All rights reserved.

// Tests for WorldLoader.

package bdzimmer.secondary.export.controller

import org.scalatest.FunSuite


class WorldLoaderSuite extends FunSuite {

  test("name to id") {
    assert(WorldLoaderFlat.nameToId("Test 1, 2, 3**").equals("test_1_2_3"))
    assert(WorldLoaderFlat.nameToId(" test").equals("_test"))
    assert(WorldLoaderFlat.nameToId("Ben | Zimmer").equals("ben_zimmer"))
  }

}