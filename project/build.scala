// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Secondary project build.scala file

import sbt._
import Keys._

case class JvmSettings(javacSource: String, javacTarget: String, scalacTarget: String)