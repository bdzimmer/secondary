// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Class for loading WorldItem hierarchies from YAML files.

// 2015-07-26: Refactored from WorldItem file.
// 2015-08-16: Reads beans, converts to immutable case classes.
// 2015-09-04: Switched to FileUtils instead of Source.
// 2015-10-21: Error handling for YAML parsing.

package bdzimmer.secondary.export

import java.io.File

import scala.collection.JavaConverters._
import scala.ref
import scala.reflect.ClassTag
import scala.util.{Try, Success, Failure}

import com.google.api.client.util.DateTime
import org.apache.commons.io.FileUtils
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor



object WorldLoader {


  // load a world from YAML files in the file system
  def loadWorld(
      inputDir: String,
      masterName: String, mainCollectionNames: List[String],
      fileStatus: FileModifiedMap): Either[String, CollectionItem] = {

    // load all of the main collections
    val mainCollections = mainCollectionNames.map(collectionName => {

      val filename = collectionName + ".yml"
      val collection = loadFile(filename, inputDir, fileStatus)

      val prefix = collectionName + "_"
      val matchingFiles = new File(inputDir).listFiles.map(_.getName).filter(_.startsWith(prefix))
      val childCollections = matchingFiles.map(x => {
       loadFile(x, inputDir, fileStatus)
      }).toList

      addChildCollections(collection, childCollections)

    })

    val masterFilename = masterName + ".yml"
    val masterCollection = loadFile(masterFilename, inputDir, fileStatus)

    addChildCollections(masterCollection, mainCollections).right.map(_.getVal)
  }


  // combine possibly loaded main collection / sub collections, returning only
  // error messages if anything failed to load.
  def addChildCollections(
        collection: Either[String, CollectionItemBean],
        childCollections: List[Either[String, CollectionItemBean]]): Either[String, CollectionItemBean] = {

    // collapse errors from collections into a single string
    val parseErrors = (collection :: childCollections).collect({
      case Left(msg) => msg
    }).mkString("\n")

    // get child collections that loaded successfully as a Java list
    val childCollectionsJava = childCollections.collect({
      case Right(collection) => collection
    }).asJava

    // build the result
    val result: Either[String, CollectionItemBean] = collection.fold(
      x => Left(parseErrors),
      x => {
        x.children.addAll(childCollectionsJava)
        Right(x)
      }
    )

    // This will return just the messages if anything failed to parse.
    // To get behavior where you get the subset of the world that
    // parsed correctly, don't perform the conditional and always return
    // "result"
    if (result.isRight && childCollections.count(_.isLeft) == 0) {
      result
    } else {
      Left(parseErrors)
    }

  }


  def loadWorld(projConf: ProjectConfig, fileStatus: FileModifiedMap): Either[String, CollectionItem] = {
    WorldLoader.loadWorld(
        projConf.localContentPath,
        projConf.masterName,
        projConf.mainCollections,
        fileStatus)
  }


  def loadWorld(projConf: ProjectConfig): Either[String, CollectionItem] = {
    WorldLoader.loadWorld(projConf, getEmptyModifiedMap)
  }


  // create a list of all world items in a hierarchy
  def collectionToList(worldItem: WorldItem): List[WorldItem] = worldItem match {
    case x: CollectionItem => x :: x.children.flatMap(x => collectionToList(x))
    case _ => List(worldItem)
  }


  def loadFile(
      filename: String,
      inputDir: String,
      fileStatus: FileModifiedMap): Either[String, CollectionItemBean] = {

    val yamlString = FileUtils.readFileToString(
        new java.io.File(inputDir + File.separator + filename),
        "UTF-8")

    val bean = tryToEither(Try(loadCollection(yamlString)))
    bean.right.foreach(assignSrcYml(_, filename, fileStatus))
    bean.left.map(logParseError(filename, _))
  }

  def logParseError(filename: String, message: String): String = {
    s"***\nParsing error in ${filename}:\n\n${message}\n***\n"
  }

  def tryToEither(x: Try[CollectionItemBean]): Either[String, CollectionItemBean] = x match {
    case Success(x) => Right(x)
    case Failure(e) => Left(e.getMessage)
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


  //////////////////////

  // functions for loading and saving FileModifiedMaps

  // save a FileModifiedMap to a text file
  def saveModifiedMap(outputFile: String, map: FileModifiedMap): Unit = {
    val pw = new java.io.PrintWriter(new java.io.File(outputFile))
    // scalastyle:ignore regex
    map foreach (x =>  pw.println(x._1 + "\t" + x._2._1 + "\t" + x._2._2.getValue))
    pw.close
  }

  // load a FileModifiedMap from a text file
  def loadModifiedMap(inputFile: String): FileModifiedMap = {
    val lines = scala.io.Source.fromFile(inputFile).getLines
    lines.map(x => x.split("\t")).map(x => (x(0), (x(1), new DateTime(x(2).toLong)))).toMap
  }

  // load a FileModifiedMap from a text file, returning an empty map
  // if the file doesn't exist.
  def loadOrEmptyModifiedMap(inputFile: String): FileModifiedMap = {
    (new java.io.File(inputFile).exists) match {
      case true => loadModifiedMap(inputFile)
      case false => getEmptyModifiedMap
    }
  }

  // get an empty FileModifiedMap
  def getEmptyModifiedMap(): FileModifiedMap = {
    List.empty[(String, (String, DateTime))].toMap
  }

  // merge FileModifiedMaps keeping newer dates / ids.
  def mergeModifiedMaps(map1: FileModifiedMap, map2: FileModifiedMap): FileModifiedMap = {
    map1 ++ map2.map{case (k, v) => k -> {
      map1.get(k) match {
        case Some(x) => if (x._2.getValue > v._2.getValue) x else v
        case None => v
      }
    }}
  }

}
