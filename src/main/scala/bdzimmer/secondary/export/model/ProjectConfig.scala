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
    val masterName: String,
    val license: String,
    val navbars: Boolean,
    val hiddenItems: String) {

  val localExportPath      = projectDir / ProjectStructure.WebDir
  val localContentPath     = projectDir / ProjectStructure.ContentDir
  val localExportPathFile  = new File(localExportPath)
  val localContentPathFile = new File(localContentPath)

}



object ProjectConfig {

  val masterName  = TextConfigField("masterName", "master", "Master name")
  val license     = TextConfigField("license", "Copyright &copy 2018. All rights reserved.", "License text")
  val navbars     = BoolConfigField("navbars", "true", "Navbars")
  val hiddenItems = TextConfigField("hiddenitems", "", "Hidden items")

  val requiredProperties: List[ConfigField] =  List(
      masterName,
      license,
      navbars,
      hiddenItems)

      
  def getProperties(projectDir: String): PropertiesWrapper = {
    val propFilename = projectDir / ProjectStructure.ConfigurationFile
    new PropertiesWrapper(propFilename)
  }

  
  def apply(projectDir: String): ProjectConfig = {

    val prop = getProperties(projectDir)

    val missing = requiredProperties.filter(x => {
      !prop.prop.keySet().contains(x.key)
    })

    missing.foreach(x => {
      System.err.println(
          s"property ${x.key} missing from driver configuration\n" +
          s"\tusing default value: '${x.default}'")
    })

    def getProp(cf: ConfigField): String = {
      prop(cf.key).getOrElse(cf.default)
    }

    new ProjectConfig(
        projectDir  = projectDir,
        masterName  = getProp(masterName),
        license     = getProp(license),
        navbars     = getProp(navbars).toBooleanSafe,
        hiddenItems = getProp(hiddenItems))
  }
}
