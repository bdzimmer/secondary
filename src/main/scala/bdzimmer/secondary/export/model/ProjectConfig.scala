// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Project configuration.

// 2015-08-30: Created in refactor from Driver.
// 2015-09-12: Changes for per-project configs.
// 2016-01-30: Some new properties.

package bdzimmer.secondary.export.model

import java.io.File

import bdzimmer.util.PropertiesWrapper
import bdzimmer.util.StringUtils._

import bdzimmer.secondary.export.model.ConfigurationModel._


class ProjectConfig(
    val projectDir: String,
    val mode: String,
    val driveClientIdFile: String,
    val driveAccessTokenFile: String,
    val driveInputPath: String,
    val driveOutputPath: String,
    val mappedContentPath: String,
    val masterName: String,
    val license: String,
    val navbars: Boolean,
    val editLinks: Boolean) {

  // additional attributes derived from the above
  val driveInputPathList = driveInputPath.split(slash).toList
  val driveOutputPathList = driveOutputPath.split(slash).toList

  val localExportPath  = projectDir / ProjectStructure.WebDir
  val localContentPath = projectDir / ProjectStructure.ContentDir
  val localExportPathFile = new File(localExportPath)
  val localContentPathFile = new File(localContentPath)

  val mappedContentPathActual = if (mode.equals("local")) {
    localContentPath
  } else {
    mappedContentPath
  }

}



object ProjectConfig {

  val mode = ChooseConfigField("mode", "drive", List("drive", "local"), "Export mode")

  val driveClientIdFile    = TextConfigField("driveClientIdFile", "client_secret.json", "Drive client id file")
  val driveAccessTokenFile = TextConfigField("driveAccessTokenFile", "access_token.json", "Drive access token file")
  val driveInputPath       = TextConfigField("driveInputPath", "secondary/content", "Drive input path")
  val driveOutputPath      = TextConfigField("driveOutputPath", "secondary/web", "Drive output path")
  val mappedContentPath    = TextConfigField("mappedContentPath", "", "Mapped content path")

  val masterName           = TextConfigField("masterName", "master", "Master name")

  val license              = TextConfigField("license", "Copyright &copy 2016. All rights reserved.", "License text")
  val navbars              = BoolConfigField("navbars", "true", "Navbars")
  val editLinks            = BoolConfigField("editlinks", "true", "Edit Links")

  val requiredProperties: List[ConfigField] =  List(
      mode,
      driveClientIdFile,
      driveAccessTokenFile,
      driveInputPath,
      driveOutputPath,
      mappedContentPath,
      masterName,
      license,
      navbars,
      editLinks)

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
        mode                 = getProp(mode),
        driveClientIdFile    = getProp(driveClientIdFile),
        driveAccessTokenFile = getProp(driveAccessTokenFile),
        driveInputPath       = getProp(driveInputPath),
        driveOutputPath      = getProp(driveOutputPath),
        mappedContentPath    = getProp(mappedContentPath),
        masterName           = getProp(masterName),
        license              = getProp(license),
        navbars              = getProp(navbars).toBooleanSafe,
        editLinks            = getProp(editLinks).toBooleanSafe)
  }
}
