// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Class for loading WorldItem heirarchies from YAML files.

// 2015-07-26: Refactored from WorldItem file.
// 2015-08-16: Reads beans, converts to immutable case classes.

package bdzimmer.secondary.export

import java.io.File

import scala.collection.JavaConverters._
import scala.ref
import scala.reflect.ClassTag

import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor


object WorldLoader {


  // new loadMasterCollection function

  def loadMasterCollection(inputDir: String,
                           masterName: String, mainCollectionNames: List[String],
                           fileStatus: FileModifiedMap): CollectionItem = {


    // fix the srcyml / remote id for a collection
    def assignSrcYml(wi: WorldItemBean, srcyml: String): Unit =  wi match {
      case x: CollectionItemBean => {
        println(srcyml)
        x.srcyml = srcyml
        // x.remoteid = fileStatus.get(srcyml).get._1
        x.remoteid = fileStatus.get(srcyml).map(_._1).getOrElse("")
        x.children.asScala.toList.map(y => assignSrcYml(y, srcyml))
      }
      case _ => {
        println(srcyml)
        wi.srcyml = srcyml
        // wi.remoteid = fileStatus.get(srcyml).get._1
        wi.remoteid = fileStatus.get(srcyml).map(_._1).getOrElse("")

      }

    }



    def loadFile(filename: String): CollectionItemBean = {
      val yamlString = scala.io.Source.fromFile(inputDir + "/" + filename).getLines.mkString("\n")
      val collectionBean = loadCollection(yamlString)
      assignSrcYml(collectionBean, filename)
      collectionBean
    }



    val masterYamlName = masterName + ".yml"
    val masterCollection = loadFile(masterYamlName)

    // val mainCollections = mainCollectionNames.map(loadCollection(loadFile(_)))

    // 2015-07-11
    // making the main collection loading more dynamic
    // for each file listed in main collection names:
    // --load that file into a collection
    // --then load any files that have that prefix and add those to the
    //   collection

    val mainCollections = mainCollectionNames.map(collectionName => {

      println(collectionName)

      val fileName = collectionName + ".yml"

      val collection = loadFile(fileName)

      val prefix = collectionName + "_"
      val matchingFiles = new File(inputDir).listFiles.map(_.getName).filter(_.startsWith(prefix))

      println("matching: " + matchingFiles.mkString(","))

      val childCollections = matchingFiles.map(loadFile(_))

      val childCollectionsJava = childCollections.toList.asJava

      collection.children.addAll(childCollectionsJava)

      collection

    })


    masterCollection.children = mainCollections.asJava.asInstanceOf[java.util.List[WorldItemBean]]
    masterCollection.getVal

  }



  /**
   * Load a collection of WorldItems from a YAML document.
   *
   * @param yamlString  string containing YAML document
   */
  def loadCollection(yamlString: String): CollectionItemBean = {

    val yaml = new Yaml(WorldItem.constructor)
    val collectionBean = yaml.load(yamlString).asInstanceOf[CollectionItemBean]
    // println(yaml.dump(e))

    collectionBean
  }




}
