// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Driver configuration.

// 2015-08-30: Created in refactor from Driver.
// 2015-09-12: Changes for local rather than global configs.

// TODO: rename to ProjectConfig

package bdzimmer.secondary.export

import java.io.FileInputStream
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


class DriverConfig(val projectDir: String) {

  // val projectDir = System.getProperty("user.home")
  val propFilename = projectDir + "/" + "secondary.properties"

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

  // val localScratchPath = getProp(DriverConfig.localScratchPath)
  val driveClientIdFile = getProp(DriverConfig.driveClientIdFile)
  val driveAccessTokenFile = getProp(DriverConfig.driveAccessTokenFile)
  val driveInputPath = getProp(DriverConfig.driveInputPath)
  val driveOutputPath = getProp(DriverConfig.driveOutputPath)
  val mainCollectionNames = getProp(DriverConfig.mainCollectionNames)
  val license = getProp(DriverConfig.license)
  val localContentPath = getProp(DriverConfig.localContentPath)

}


object DriverConfig {

  // val localScratchPath = ConfigField("localScratchPath", "tmp", "Local scratch path")

  val driveClientIdFile = ConfigField("driveClientIdFile", "client_secret.json", "Drive client id file")
  val driveAccessTokenFile = ConfigField("driveAccessTokenFile", "access_token.json", "Drive access token file")

  val driveInputPath = ConfigField("driveInputPath", "secondary/content", "Drive input path")
  val driveOutputPath = ConfigField("driveOutputPath", "secondary/web", "Drive output path")
  val mainCollectionNames = ConfigField("mainCollectionNames", "characters,locations,lore,images,tilesets,sprites", "Main collection names")
  val license = ConfigField("license", "", "License text")
  val localContentPath = ConfigField("localContentPath", "", "Local content path")

  val requiredProperties =  List(
      // localScratchPath,
      driveClientIdFile,
      driveAccessTokenFile,
      driveInputPath,
      driveOutputPath,
      mainCollectionNames,
      license,
      localContentPath)
}
