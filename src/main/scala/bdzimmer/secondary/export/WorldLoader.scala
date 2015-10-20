// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Class for loading WorldItem hierarchies from YAML files.

// 2015-07-26: Refactored from WorldItem file.
// 2015-08-16: Reads beans, converts to immutable case classes.
// 2015-09-04: Switched to FileUtils instead of Source.
// 2015-10-20: Error handling for YAML parsing.

package bdzimmer.secondary.export

import java.io.File

import scala.collection.JavaConverters._
import scala.ref
import scala.reflect.ClassTag
import scala.util.{Try, Success, Failure}

import org.apache.commons.io.FileUtils
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor


object WorldLoader {

  // The parse error handling in here will toss collections that don't parse and build
  // the world from what can be parsed. However, it may make more sense to just have an
  // overall failure if any portion of the world doesn't parse. The current method could
  // potentially cause problems with links for items that are altered the same cycle a
  // referenced collection becomes invalid. I think this is a straightforward change
  // that would be easy to switch back and forth in the code.

  // TODO: fail loading world if any collection does not parse.

  def loadWorld(
      inputDir: String,
      masterName: String, mainCollectionNames: List[String],
      fileStatus: FileModifiedMap): Try[CollectionItem] = {

    def logParseError(filename: String, message: String): Unit = {
      println(s"***\nParsing error in ${filename}:\n\n${message}\n***")
    }

    val mainCollections = mainCollectionNames.map(collectionName => {

      val fileName = collectionName + ".yml"
      val collection = loadFile(fileName, inputDir, fileStatus)

      val prefix = collectionName + "_"
      val matchingFiles = new File(inputDir).listFiles.map(_.getName).filter(_.startsWith(prefix))
      val childCollections = matchingFiles.map(x => {
        (x, loadFile(x, inputDir, fileStatus))
      }).toList

      // print any errors from children
      ((fileName, collection) :: childCollections).collect({
        case (x, Failure(e)) => (x, e.getMessage)
      }).foreach(x => logParseError(x._1, x._2))

      val childCollectionsJava = childCollections.map(_._2.toOption).flatten.asJava
      collection.foreach(_.children.addAll(childCollectionsJava))
      collection

    }).map(_.toOption).flatten

    val masterYamlName = masterName + ".yml"
    val masterCollection = loadFile(masterYamlName, inputDir, fileStatus)

    // print errors from master
    masterCollection.recover({case e => logParseError(masterYamlName, e.getMessage)})

    masterCollection.map(x => {
      x.children = mainCollections.asJava.asInstanceOf[java.util.List[WorldItemBean]]
      x.getVal
    })

  }


  def loadWorld(projConf: ProjectConfig, fileStatus: FileModifiedMap): Try[CollectionItem] = {
    WorldLoader.loadWorld(
        projConf.localContentPath,
        projConf.masterName,
        projConf.mainCollections,
        fileStatus)
  }


  def loadWorld(projConf: ProjectConfig): Try[CollectionItem] = {
    WorldLoader.loadWorld(projConf, ExportPages.getEmptyFileModifiedMap)
  }


  // create a list of all world items in a hierarchy
  def collectionToList(worldItem: WorldItem): List[WorldItem] = worldItem match {
    case x: CollectionItem => x :: x.children.flatMap(x => collectionToList(x))
    case _ => List(worldItem)
  }


  def loadFile(
      filename: String,
      inputDir: String,
      fileStatus: FileModifiedMap): Try[CollectionItemBean] = {

    val yamlString = FileUtils.readFileToString(
        new java.io.File(inputDir + File.separator + filename),
        "UTF-8")

    val bean = Try(loadCollection(yamlString))
    bean.foreach(assignSrcYml(_, filename, fileStatus))
    bean
  }


  // Load a collection of WorldItems from a YAML document.
  def loadCollection(yamlString: String): CollectionItemBean = {
    val yaml = new Yaml(WorldItem.constructor)
    val collectionBean = yaml.load(yamlString).asInstanceOf[CollectionItemBean]
    collectionBean
  }


  // fix the srcyml / remote id for a collection
  def assignSrcYml(wi: WorldItemBean, srcyml: String, fileStatus: FileModifiedMap): Unit =  wi match {
    case x: CollectionItemBean => {
      x.srcyml = srcyml
      x.remoteid = fileStatus.get(srcyml).map(_._1).getOrElse("")
      x.children.asScala.toList.map(y => assignSrcYml(y, srcyml, fileStatus))
    }
    case _ => {
      wi.srcyml = srcyml
      wi.remoteid = fileStatus.get(srcyml).map(_._1).getOrElse("")
    }
   }

}
