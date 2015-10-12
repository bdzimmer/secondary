// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Class for loading WorldItem hierarchies from YAML files.

// 2015-07-26: Refactored from WorldItem file.
// 2015-08-16: Reads beans, converts to immutable case classes.
// 2015-09-04: Switched to FileUtils instead of Source.

package bdzimmer.secondary.export

import java.io.File
// import java.nio.charset.StandardCharsets

import scala.collection.JavaConverters._
import scala.ref
import scala.reflect.ClassTag

import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import org.apache.commons.io.FileUtils


object WorldLoader {


  // new loadMasterCollection function

  def loadWorld(
      inputDir: String,
      masterName: String, mainCollectionNames: List[String],
      fileStatus: FileModifiedMap): CollectionItem = {


    val masterYamlName = masterName + ".yml"
    val masterCollection = loadFile(masterYamlName, inputDir, fileStatus)

    val mainCollections = mainCollectionNames.map(collectionName => {

      // println(collectionName)

      val fileName = collectionName + ".yml"
      val collection = loadFile(fileName, inputDir, fileStatus)

      val prefix = collectionName + "_"
      val matchingFiles = new File(inputDir).listFiles.map(_.getName).filter(_.startsWith(prefix))

      // println("matching: " + matchingFiles.mkString(","))

      val childCollections = matchingFiles.map(loadFile(_, inputDir, fileStatus))
      val childCollectionsJava = childCollections.toList.asJava
      collection.children.addAll(childCollectionsJava)

      collection

    })


    masterCollection.children = mainCollections.asJava.asInstanceOf[java.util.List[WorldItemBean]]

    masterCollection.getVal

  }


  // create a list of all world items in a hierarchy
  def collectionToList(worldItem: WorldItem): List[WorldItem] = worldItem match {
    case x: CollectionItem => x :: x.children.flatMap(x => collectionToList(x))
    case _ => List(worldItem)
  }


  def loadFile(filename: String, inputDir: String, fileStatus: FileModifiedMap): CollectionItemBean = {

    val yamlString = FileUtils.readFileToString(
        new java.io.File(inputDir + File.separator + filename),
        "UTF-8")

    val collectionBean = loadCollection(yamlString)

    assignSrcYml(collectionBean, filename, fileStatus)
    collectionBean
  }


  // Load a collection of WorldItems from a YAML document.
  def loadCollection(yamlString: String): CollectionItemBean = {

    val yaml = new Yaml(WorldItem.constructor)
    val collectionBean = yaml.load(yamlString).asInstanceOf[CollectionItemBean]
    // println(yaml.dump(e))

    collectionBean
  }


  // fix the srcyml / remote id for a collection
  def assignSrcYml(wi: WorldItemBean, srcyml: String, fileStatus: FileModifiedMap): Unit =  wi match {
    case x: CollectionItemBean => {
      // println(srcyml)
      x.srcyml = srcyml
      // x.remoteid = fileStatus.get(srcyml).get._1
      x.remoteid = fileStatus.get(srcyml).map(_._1).getOrElse("")
      x.children.asScala.toList.map(y => assignSrcYml(y, srcyml, fileStatus))
    }
    case _ => {
     //  println(srcyml)
      wi.srcyml = srcyml
      // wi.remoteid = fileStatus.get(srcyml).get._1
      wi.remoteid = fileStatus.get(srcyml).map(_._1).getOrElse("")

    }
   }


}
