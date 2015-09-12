// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Object to hold information about how a project is structured on disk.

// 2015-09-12: Created.

package bdzimmer.secondary.export

object ProjectStructure {

  // these are directories within the project directory
  // (working directory for Secondary)

  val ContentDir = "content"
  val CacheDir = "cache"
  val WebDir = "web"

  // configuration file
  val ConfigurationFile = "secondary.properties"

  // timestamp files
  val MetaStatusFile = "meta_status.txt"
  val FileStatusFile = "file_status.txt"

}
