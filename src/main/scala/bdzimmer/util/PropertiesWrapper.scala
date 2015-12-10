// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Idiomatic Scala wrapper for Java Properties.

package bdzimmer.util

import java.io.{File, FileInputStream}
import java.util.Properties


class PropertiesWrapper(val filename: String) {

  val file = new File(filename)
  val prop = new Properties()

  if (file.exists) {
    prop.load(new FileInputStream(file))
  }

  def apply(key: String): Option[String] = Option(prop.getProperty(key))
  def set(k: String, v: String): Unit = prop.setProperty(k, v)

}
