// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Project configuration.

// 2015-08-30: Created in refactor from Driver.
// 2015-09-12: Changes for per-project configs.

package bdzimmer.secondary.export.model

import java.io.File

import bdzimmer.util.PropertiesWrapper
import bdzimmer.util.StringUtils._

case class ConfigField(key: String, default: String, description: String)




class ProjectConfig(
  val projectDir: String,
  val mode: String,
  val driveClientIdFile: String,
  val driveAccessTokenFile: String,
  val driveInputPath: String,
  val driveOutputPath: String,
  val mappedContentPath: String,
  val masterName: String,
  val license: String) {


  // attributes derived from the above
  val driveInputPathList = driveInputPath.split(slash).toList
  val driveOutputPathList = driveOutputPath.split(slash).toList

  val localExportPath = projectDir  / ProjectStructure.WebDir
  val localContentPath = projectDir / ProjectStructure.ContentDir
  val localExportPathFile = new File(localExportPath)
  val localContentPathFile = new File(localContentPath)

  val mappedContentPathActual = if (mappedContentPath.equals("")) {
    localContentPath
  } else {
    mappedContentPath
  }

}



object ProjectConfig {

  val mode = ConfigField("mode", "drive", "Export mode")

  val driveClientIdFile = ConfigField("driveClientIdFile", "client_secret.json", "Drive client id file")
  val driveAccessTokenFile = ConfigField("driveAccessTokenFile", "access_token.json", "Drive access token file")

  val driveInputPath = ConfigField("driveInputPath", "secondary/content", "Drive input path")
  val driveOutputPath = ConfigField("driveOutputPath", "secondary/web", "Drive output path")
  val mappedContentPath = ConfigField("mappedContentPath", "", "Mapped content path")
  val masterName = ConfigField("masterName", "master", "Master name")
  val license = ConfigField("license", "Copyright &copy 2015. All rights reserved.", "License text")


  val requiredProperties =  List(
      mode,
      driveClientIdFile,
      driveAccessTokenFile,
      driveInputPath,
      driveOutputPath,
      mappedContentPath,
      masterName,
      license)


  def getProperties(projectDir: String): PropertiesWrapper = {
    val propFilename = projectDir / ProjectStructure.ConfigurationFile
    new PropertiesWrapper(propFilename)
  }

  def apply(projectDir: String): ProjectConfig = {

    val prop = getProperties(projectDir)

    val missing = requiredProperties.filter(x => {
      !prop.prop.keySet().contains(x.key)
    })

    missing foreach(x => {
      System.err.println(
          s"property ${x.key} missing from driver configuration\n" +
          s"\tusing default value: '${x.default}'")
    })

    def getProp(cf: ConfigField): String = {
      prop(cf.key).getOrElse(cf.default)
    }

    new ProjectConfig(
        projectDir = projectDir,
        mode                 = getProp(ProjectConfig.mode),
        driveClientIdFile    = getProp(ProjectConfig.driveClientIdFile),
        driveAccessTokenFile = getProp(ProjectConfig.driveAccessTokenFile),
        driveInputPath       = getProp(ProjectConfig.driveInputPath),
        driveOutputPath      = getProp(ProjectConfig.driveOutputPath),
        mappedContentPath    = getProp(ProjectConfig.mappedContentPath),
        masterName           = getProp(ProjectConfig.masterName),
        license              = getProp(ProjectConfig.license))


  }
}
