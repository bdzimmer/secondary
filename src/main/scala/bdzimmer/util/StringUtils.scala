// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Implicit classes for string extension methods.

package bdzimmer.util

object StringUtils {

  implicit class StringPath(val s: String) extends AnyVal {

    // method for joining path segments
    def /(x: String): String = {
      s + "/" + x
    }

  }

  val slash = "/"

}
