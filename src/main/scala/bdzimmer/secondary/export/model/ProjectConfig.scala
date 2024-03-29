// Copyright (c) 2023 Ben Zimmer. All rights reserved.

// Project configuration.

// 2015-08-30: Created in refactor from Driver.
// 2015-09-12: Changes for per-project configs.
// 2016-01-30: Some new properties.

package bdzimmer.secondary.export.model

import java.io.File
import scala.util.Try
import scala.collection.JavaConverters._

import org.apache.commons.io.FileUtils

import bdzimmer.util.PropertiesWrapper
import bdzimmer.util.StringUtils._

import bdzimmer.secondary.export.model.ConfigurationModel._


class ProjectConfig(
    val projectDir: String,
    val masterName: String,
    val license: String,
    val navbars: Boolean,
    val subarticles: Boolean,
    val relativeLinks: Boolean,
    val hiddenItems: String,
    val unifiedJumbotron: Boolean,
    val search: Boolean,
    val contentDirs: Map[String, String]) {

  val localExportPath      = projectDir / ProjectStructure.WebDir
  val localContentPath     = projectDir / ProjectStructure.ContentDir
  val localExportPathFile  = new File(localExportPath)
  val localContentPathFile = new File(localContentPath)

}



object ProjectConfig {

  val masterName       = TextConfigField("masterName", "master", "Master name")
  val license          = TextConfigField("license", "Copyright &copy 2018. All rights reserved.", "License text")
  val navbars          = BoolConfigField("navbars", "true", "Navbars")
  val subarticles      = BoolConfigField("subarticles", "true", "Subarticles")
  val relativeLinks    = BoolConfigField("relativelinks", "true", "Relative links")
  val hiddenItems      = TextConfigField("hiddenitems", "", "Hidden items")
  val unifiedJumbotron = BoolConfigField("unifiedjumbotron", "false", "Unified Jumbotron")
  val search            = BoolConfigField("search", "true", "Search")

  val requiredProperties: List[ConfigField] =  List(
      masterName,
      license,
      navbars,
      subarticles,
      relativeLinks,
      hiddenItems,
      unifiedJumbotron,
      search)

      
  def getProperties(projectDir: String): PropertiesWrapper = {
    val propFilename = projectDir / ProjectStructure.ConfigurationFile
    new PropertiesWrapper(propFilename)
  }

  private def getContentDirs(projectDir: String): Map[String, String] = {
    val contentDirsFile = new File(projectDir / ProjectStructure.ContentDirsFile)
    if (!contentDirsFile.isFile) {
      Map()
    } else {
      val res = Try {
        val lines = FileUtils.readLines(contentDirsFile, "UTF-8").asScala
        lines.flatMap(x => {
          Try {
            val x_split = x.split(":")
            (x_split(0), x_split(1))
          }.toOption
        }).toMap
      }
      if (res.isFailure) {
        println(s"failed to read content dirs file: $contentDirsFile")
      }
      res.getOrElse(Map())
    }
  }


  def apply(projectDir: String): ProjectConfig = {

    val prop = getProperties(projectDir)

    val contentDirs = getContentDirs(projectDir)

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
        projectDir       = projectDir,
        masterName       = getProp(masterName),
        license          = getProp(license),
        navbars          = getProp(navbars).toBooleanSafe,
        subarticles      = getProp(subarticles).toBooleanSafe,
        relativeLinks    = getProp(relativeLinks).toBooleanSafe,
        hiddenItems      = getProp(hiddenItems),
        unifiedJumbotron = getProp(unifiedJumbotron).toBooleanSafe,
        search           = getProp(search).toBooleanSafe,
        contentDirs      = contentDirs)
  }
}
