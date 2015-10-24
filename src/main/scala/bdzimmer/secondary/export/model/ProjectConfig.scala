// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Project configuration.

// 2015-08-30: Created in refactor from Driver.
// 2015-09-12: Changes for per-project configs.

package bdzimmer.secondary.export.model

import java.io.{File, FileInputStream}
import java.util.Properties


case class ConfigField(key: String, default: String, description: String)


// idiomatic Scala for Java Properties
class PropertiesWrapper(val filename: String) {

  val file = new java.io.File(filename)
  val prop = new Properties()

  if (file.exists) {
    prop.load(new FileInputStream(file))
  }

  def apply(key: String): Option[String] = Option(prop.getProperty(key))
  def set(k: String, v: String): Unit = prop.setProperty(k, v)

}



class ProjectConfig(
  val projectDir: String,
  val driveClientIdFile: String,
  val driveAccessTokenFile: String,
  val driveInputPath: String,
  val driveOutputPath: String,
  val mappedContentPath: String,
  val masterName: String,
  val mainCollectionNames: String,
  val license: String) {


  // attributes derived from the above
  val mainCollections = mainCollectionNames.split(",").toList.map(_.trim)
  val driveInputPathList = driveInputPath.split("/").toList
  val driveOutputPathList = driveOutputPath.split("/").toList

  val localExportPath = projectDir  + File.separator + ProjectStructure.WebDir + File.separator
  val localContentPath = projectDir + File.separator + ProjectStructure.ContentDir + File.separator
  val localExportPathFile = new File(localExportPath)
  val localContentPathFile = new File(localContentPath)

  // TODO: fix awkward creation of mapped content path
  val mappedContentPathActual = if (mappedContentPath.equals("")) {
    localContentPath
  } else {
    mappedContentPath
  }

}



object ProjectConfig {

  val driveClientIdFile = ConfigField("driveClientIdFile", "client_secret.json", "Drive client id file")
  val driveAccessTokenFile = ConfigField("driveAccessTokenFile", "access_token.json", "Drive access token file")

  val driveInputPath = ConfigField("driveInputPath", "secondary/content", "Drive input path")
  val driveOutputPath = ConfigField("driveOutputPath", "secondary/web", "Drive output path")
  val mappedContentPath = ConfigField("mappedContentPath", "", "Mapped content path")
  val masterName = ConfigField("masterName", "master", "Master name")
  val mainCollectionNames = ConfigField("mainCollectionNames", "characters,locations,lore,images,tilesets,sprites", "Main collection names")
  val license = ConfigField("license", "", "License text")


  val requiredProperties =  List(
      driveClientIdFile,
      driveAccessTokenFile,
      driveInputPath,
      driveOutputPath,
      mappedContentPath,
      masterName,
      mainCollectionNames,
      license)


  def getProperties(projectDir: String): PropertiesWrapper = {
    val propFilename = projectDir + File.separator + ProjectStructure.ConfigurationFile
    new PropertiesWrapper(propFilename)
  }

  def apply(projectDir: String): ProjectConfig = {

    val prop = getProperties(projectDir)

    val missing = requiredProperties.filter(x => {
      !prop.prop.keySet().contains(x.key)
    })

    missing foreach(x => {
      System.err.println(
          "property " + x.key + " missing from driver configuration\n" +
          "\tusing default value " + x.default)
    })

    def getProp(cf: ConfigField): String = {
      prop(cf.key).getOrElse(cf.default)
    }

    new ProjectConfig(
        projectDir = projectDir,
        driveClientIdFile = getProp(ProjectConfig.driveClientIdFile),
        driveAccessTokenFile = getProp(ProjectConfig.driveAccessTokenFile),
        driveInputPath = getProp(ProjectConfig.driveInputPath),
        driveOutputPath = getProp(ProjectConfig.driveOutputPath),
        mappedContentPath = getProp(ProjectConfig.mappedContentPath),
        masterName = getProp(ProjectConfig.masterName),
        mainCollectionNames = getProp(ProjectConfig.mainCollectionNames),
        license = getProp(ProjectConfig.license))


  }
}
