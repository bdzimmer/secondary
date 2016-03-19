// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Object to hold information about how a project is structured on disk.

// 2015-09-12: Created.

package bdzimmer.secondary.export.model

object ProjectStructure {

  // these are directories within the project directory
  // (working directory for Secondary)

  val ContentDir = "content"
  val WebDir = "web"

  // configuration file
  val ConfigurationFile = "secondary.properties"

  // status files
  val MetaStatusFile = "status_meta.txt"
  val RefStatusFile = "status_ref.txt"
  val ItemStatusFile = "status_item.txt"

}
