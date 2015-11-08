// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Class for loading WorldItem hierarchies from YAML files.

// 2015-07-26: Refactored from WorldItem file.
// 2015-08-16: Reads beans, converts to immutable case classes.
// 2015-09-04: Switched to FileUtils instead of Source.
// 2015-10-21: Error handling for YAML parsing.
// 2015-11-06: YAML includes instead of main collections.

package bdzimmer.secondary.export.controller

import java.io.File

import scala.collection.JavaConverters._
import scala.ref
import scala.reflect.ClassTag
import scala.util.{Try, Success, Failure}

import com.google.api.client.util.DateTime
import org.apache.commons.io.FileUtils
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import bdzimmer.secondary.export.model._


object WorldLoader {


  def loadWorld(
      inputDir: String,
      masterName: String,
      fileStatus: FileModifiedMap): Either[String, CollectionItem] = {

    val masterFilename = masterName + ".yml"
    loadFile(masterFilename, inputDir, fileStatus, List(masterFilename)).right.map(_.getVal)

  }

  def loadWorld(projConf: ProjectConfig, fileStatus: FileModifiedMap): Either[String, CollectionItem] = {
    WorldLoader.loadWorld(
        projConf.localContentPath,
        projConf.masterName,
        fileStatus)
  }

  def loadWorld(projConf: ProjectConfig): Either[String, CollectionItem] = {
    WorldLoader.loadWorld(projConf, getEmptyModifiedMap)
  }


  // TODO: catch file doesn't exist
  def loadFile(
      filename: String,
      inputDir: String,
      fileStatus: FileModifiedMap,
      loadedFiles: List[String]): Either[String, CollectionItemBean] = {

    val yamlString = FileUtils.readFileToString(
        new java.io.File(inputDir + File.separator + filename),
        "UTF-8")

    val bean = tryToEither(Try(loadCollection(yamlString))).left.map(logParseError(filename, _))

    // set srcyml before getting new children
    bean.right.foreach(assignSrcYml(_, filename, fileStatus))

    for {
      coll <- bean.right
      newChildren <- getNewChildren(coll, inputDir, fileStatus, loadedFiles).right
    } yield {
      coll.children = new java.util.LinkedList
      coll.children.addAll(newChildren.asJava)
      coll
    }

  }

  def getNewChildren(
      bean: CollectionItemBean,
      inputDir: String,
      fileStatus: FileModifiedMap,
      loadedFiles: List[String]): Either[String, List[WorldItemBean]] = {

    val newChildrenLoads = bean.children.asScala.map(child => {
        child match {
          case x: YamlIncludeBean => if (loadedFiles.contains(x.filename)) {
            Left(s"""***\nitem "${bean.id}" contains circular reference to file "${x.filename}"\n***""")
          } else {
            loadFile(x.filename, inputDir, fileStatus, x.filename +: loadedFiles)
          }
          case _ => Right(child)
        }
    })

    // gather error messages from children into a list
    val parseErrors = newChildrenLoads.collect({case Left(x) => x})

    // get the child items that loaded into a list
    val newChildren = newChildrenLoads.collect({case Right(x) => x}).toList

    val result: Either[String, List[WorldItemBean]] = if (parseErrors.length > 0) {
      Left(parseErrors.mkString("\n"))
    } else {
      Right(newChildren)
    }

    result

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
  def saveModifiedMap(filename: String, map: FileModifiedMap): Unit = {
    val pw = new java.io.PrintWriter(new java.io.File(filename))
    // scalastyle:ignore regex
    map.foreach(x =>  pw.println(x._1 + "\t" + x._2._1 + "\t" + x._2._2.getValue))
    pw.close
  }

  // load a FileModifiedMap from a text file
  def loadModifiedMap(filename: String): FileModifiedMap = {
    val lines = FileUtils.readLines(new File(filename), "UTF-8").asScala
    lines.map(x => x.split("\t")).map(x => (x(0), (x(1), new DateTime(x(2).toLong)))).toMap
  }

  // load a FileModifiedMap from a text file, returning an empty map
  // if the file doesn't exist.
  def loadOrEmptyModifiedMap(filename: String): FileModifiedMap = {
    (new java.io.File(filename).exists) match {
      case true => loadModifiedMap(filename)
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
