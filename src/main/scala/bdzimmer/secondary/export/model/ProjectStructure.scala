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

  // timestamp files
  val DriveMetaStatusFile = "drive_meta_status.txt"
  val DriveFileStatusFile = "drive_file_status.txt"

  val LocalMetaStatusFile = "local_meta_status.txt"
  val LocalFileStatusFile = "local_file_status.txt"

}