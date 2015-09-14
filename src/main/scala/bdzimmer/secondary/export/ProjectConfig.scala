// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Project configuration.

// 2015-08-30: Created in refactor from Driver.
// 2015-09-12: Changes for per-project configs.

package bdzimmer.secondary.export

import java.io.{File, FileInputStream}
import java.util.Properties


case class ConfigField(key: String, default: String, description: String)


// idiomatic Scala for Java Properties
class PropertiesWrapper(filename: String) {

  val file = new java.io.File(filename)
  val prop = new Properties()

  if (file.exists) {
    prop.load(new FileInputStream(file))
  }

  def apply(key: String): Option[String] = Option(prop.getProperty(key))
  def set(k: String, v: String): Unit = prop.setProperty(k, v)

}


class ProjectConfig(val projectDir: String) {

  val propFilename = projectDir + File.separator + ProjectStructure.ConfigurationFile

  val prop = new PropertiesWrapper(propFilename)

  val missing = DriverConfig.requiredProperties.filter(x => {
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

  // attributes described in the companion object
  val driveClientIdFile = getProp(DriverConfig.driveClientIdFile)
  val driveAccessTokenFile = getProp(DriverConfig.driveAccessTokenFile)
  val driveInputPath = getProp(DriverConfig.driveInputPath)
  val driveOutputPath = getProp(DriverConfig.driveOutputPath)
  val masterName = getProp(DriverConfig.masterName)
  val mainCollectionNames = getProp(DriverConfig.mainCollectionNames)
  val license = getProp(DriverConfig.license)

  // attributes derived from the above
  val mainCollections = mainCollectionNames.split(",").toList.map(_.trim)
  val driveInputPathList = driveInputPath.split("/").toList
  val driveOutputPathList = driveOutputPath.split("/").toList

  val localExportPath = projectDir  + File.separator +  ProjectStructure.WebDir + File.separator
  val localContentPath = projectDir + File.separator + ProjectStructure.ContentDir + File.separator
  val localExportPathFile = new File(localExportPath)
  val localContentPathFile = new File(localContentPath)


}


object DriverConfig {

  val driveClientIdFile = ConfigField("driveClientIdFile", "client_secret.json", "Drive client id file")
  val driveAccessTokenFile = ConfigField("driveAccessTokenFile", "access_token.json", "Drive access token file")

  val driveInputPath = ConfigField("driveInputPath", "secondary/content", "Drive input path")
  val driveOutputPath = ConfigField("driveOutputPath", "secondary/web", "Drive output path")
  val masterName = ConfigField("masterName", "master", "Master name")
  val mainCollectionNames = ConfigField("mainCollectionNames", "characters,locations,lore,images,tilesets,sprites", "Main collection names")
  val license = ConfigField("license", "", "License text")

  val requiredProperties =  List(
      driveClientIdFile,
      driveAccessTokenFile,
      driveInputPath,
      driveOutputPath,
      masterName,
      mainCollectionNames,
      license)
}
