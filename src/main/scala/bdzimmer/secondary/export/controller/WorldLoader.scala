// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Class for loading WorldItem hierarchies from YAML files.

// 2015-07-26: Refactored from WorldItem file.
// 2015-08-16: Reads beans, converts to immutable case classes.
// 2015-09-04: Switched to FileUtils instead of Source.
// 2015-10-21: Error handling for YAML parsing.
// 2015-11-06: YAML includes instead of main collections.
// 2016-04-09: Include flat format from YAML files; flat master.

package bdzimmer.secondary.export.controller

import java.io.{BufferedReader, File, FileReader}

import scala.collection.JavaConverters._
import scala.ref
import scala.reflect.ClassTag
import scala.util.{Try, Success, Failure}

import com.google.api.client.util.DateTime
import org.apache.commons.io.FileUtils
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import bdzimmer.util.{Result, Pass, Fail}
import bdzimmer.util.StringUtils._

import bdzimmer.secondary.export.model._


private object WorldLoaderYML {


  def loadWorld(
      inputDir: String,
      masterName: String,
      fileStatus: FileMap): Result[String, CollectionItem] = {

    val masterFilename = masterName + ".yml"

    for {
      // load the world from YAML files and validate
      world <- loadFile(masterFilename, inputDir, fileStatus, List(masterFilename))
      result <- WorldLoader.validate(world.getVal)
    } yield {
      result
    }
  }


  def loadFile(
      filename: String,
      inputDir: String,
      fileStatus: FileMap,
      loadedFiles: List[String]): Result[String, CollectionItemBean] = {

    val bean = for {
      inputFile <- Result.fromFilename(inputDir / filename)
      yamlString <- Result(FileUtils.readFileToString(inputFile, "UTF-8"))
      result <- Result(loadCollection(yamlString)).mapLeft(WorldLoader.logParseError(filename, _))
    } yield {
      result
    }

    // set srcyml before getting new children
    bean.foreach(assignSrcYml(_, filename, fileStatus))

    for {
      coll <- bean
      newChildren <- getNewChildren(coll, inputDir, fileStatus, loadedFiles)
    } yield {
      setChildren(coll, newChildren)
    }
  }


  def getNewChildren(
      bean: CollectionItemBean,
      inputDir: String,
      fileStatus: FileMap,
      loadedFiles: List[String]): Result[String, List[WorldItemBean]] = {

    val newChildrenLoads = bean.children.asScala.map(child => {
        child match {
          case x: YamlIncludeBean => if (loadedFiles.contains(x.filename)) {
            Fail(s"""***\nitem "${bean.id}" contains circular reference to file "${x.filename}"\n***""")
          } else {
            if (x.filename.endsWith(".yml")) {
              loadFile(x.filename, inputDir, fileStatus, x.filename +: loadedFiles)
            } else {
              for {
                collList <- WorldLoaderFlat.loadFile(x.filename, inputDir, fileStatus, x.filename +: loadedFiles)
                coll <- WorldLoaderFlat.wire(collList)
              } yield {
                coll
              }
            }
          }
          case coll: CollectionItemBean => for {
            newChildren <- getNewChildren(coll, inputDir, fileStatus, loadedFiles)
          } yield {
            setChildren(coll, newChildren)
          }

          case _ => Pass(child)
        }
    })

    // gather error messages from children into a list
    val parseErrors = newChildrenLoads.collect({case Fail(x) => x})

    // get the child items that loaded into a list
    val newChildren = newChildrenLoads.collect({case Pass(x) => x}).toList

    val result: Result[String, List[WorldItemBean]] = if (parseErrors.length > 0) {
      Fail(parseErrors.mkString("\n"))
    } else {
      Pass(newChildren)
    }

    result

  }


  // impure
  // reassign the children of a collection with the result of getNewChildren
  def setChildren(
      coll: CollectionItemBean,
      newChildren: List[WorldItemBean]): CollectionItemBean = {

    coll.children = new java.util.LinkedList
    coll.children.addAll(newChildren.asJava)
    coll

  }


  // Load a collection of WorldItems from a YAML document.
  def loadCollection(yamlString: String): CollectionItemBean = {
    val yaml = new Yaml(WorldItem.Constructor)
    val collectionBean = yaml.load(yamlString).asInstanceOf[CollectionItemBean]
    collectionBean
  }


  // fix the srcyml / remote id for a collection
  // 2016-01-04: also fix null children (arises when YAML contains an empty children field)
  def assignSrcYml(wi: WorldItemBean, srcyml: String, fileStatus: FileMap): Unit =  wi match {
    case x: CollectionItemBean => {
      x.srcyml = srcyml
      x.remoteid = fileStatus.get(srcyml).map(_._1).getOrElse("")
      if (x.children == null) {
        x.children = new java.util.LinkedList[WorldItemBean]();
      }
      x.children.asScala.toList.map(y => assignSrcYml(y, srcyml, fileStatus))
    }
    case _ => {
      wi.srcyml = srcyml
      wi.remoteid = fileStatus.get(srcyml).map(_._1).getOrElse("")
    }
  }


}



private object WorldLoaderFlat {

  def loadWorld(
      inputDir: String,
      masterName: String,
      fileStatus: FileMap): Result[String, CollectionItem] = {

    val masterFilename = masterName + ".sec"

    for {
      // load the world from sec files
      worldList <- loadFile(masterFilename, inputDir, fileStatus, List(masterFilename))
      world <- wire(worldList)                     // wire up using paths attributes and harden
      result <- WorldLoader.validate(world.getVal) // harden and validate
    } yield {
      result
    }

  }


  def loadFile(
      filename: String,
      inputDir: String,
      fileStatus: FileMap,
      loadedFiles: List[String]): Result[String, List[WorldItemBean]] = {

    for {
      inputFile <- Result.fromFilename(inputDir / filename)
      items <- parseFile(inputFile, fileStatus)

      // items that aren't YamlIncludeBeans
      realItems = items.filter(!_.isInstanceOf[YamlIncludeBean])
      includeItems = items.collect({case x: YamlIncludeBean => x})

      // load includes
      includeLoads = includeItems.map(x => if (loadedFiles.contains(x.filename)) {
        Fail(s"""***\n"${filename}" contains circular reference to file "${x.filename}"\n***""")
      } else {
        loadFile(x.filename, inputDir, fileStatus, x.filename +: loadedFiles)
      })

      // gather error messages
      parseErrors = includeLoads.collect({case Fail(x) => x})
      // get the sucessfully loaded items
      includePasses = includeLoads.collect({case Pass(x) => x}).toList.flatten

      // if any errors, yield an error
      result <- if (parseErrors.length > 0) {
        Fail(parseErrors.mkString("\n"))
      } else {
        Pass(realItems ++ includePasses)
      }

    } yield {
      result
    }

  }


  def parseFile(inputFile: File, fileStatus: FileMap): Result[String, List[WorldItemBean]] = {

    val inNothing = 0
    val inHeader = 1
    val inNotes = 2
    val Trimmer = """^(.*?)\s*$""".r

    val items = scala.collection.mutable.Buffer[WorldItemBean]()

    val ioTry = Try {

      // line by line state machine for parsing
      // should be fast and memory efficient!

      var state = inNothing
      var item: WorldItemBean = null
      val notes = scala.collection.mutable.Buffer[String]()

      val br = new BufferedReader(new FileReader(inputFile))
      var line = br.readLine()
      var lineCount = 1

      while (line != null) {

        val Trimmer(trimmed) = line

        if (state != inHeader && line.startsWith("!")) {
          if (item != null) {
            item.notes = notes.mkString("\n")
            items += item
            notes.clear()
          }

          // create a new item based on this line
          item = WorldLoaderFlat.itemBeanFactory(trimmed)
          item.srcyml = inputFile.getName
          item.remoteid = fileStatus.get(inputFile.getName).map(_._1).getOrElse("")
          state = inHeader

        } else if (state == inHeader && line.equals("")) {
          state = inNotes
        } else if (state == inHeader) {
          // set properties of the item
          setProperty(item, trimmed).map(x => {
            throw(new Exception(lineCount + ": " + line + "\n" + x))
          })
        } else if (state == inNotes) {
          notes += trimmed
        }
        line = br.readLine()
        lineCount = lineCount + 1
      }

      if (item != null) {
        items += item
      }

      items.toList
    }

    Result.fromTry(ioTry)

  }


  def itemBeanFactory(typeName: String): WorldItemBean = typeName match {
    case "!collection"  => new CollectionItemBean()
    case "!thing"       => new ThingItemBean()
    case "!item"        => new ThingItemBean()
    case "!place"       => new PlaceItemBean()
    case "!location"    => new PlaceItemBean()
    case "!character"   => new CharacterItemBean()
    case "!person"      => new CharacterItemBean()
    case "!image"       => new ImageItemBean()
    case "!tileset"     => new TilesetItemBean()
    case "!spritesheet" => new SpritesheetItemBean()
    case "!map"         => new MapItemBean()
    case "!include"     => new YamlIncludeBean()
    case _              => new ThingItemBean()
  }


  def setProperty(item: WorldItemBean,  property: String): Option[String] = {

    val splitted = property.split(":\\s+")
    if (splitted.length >= 2) {

      val field = splitted(0)
      val propVal = splitted.drop(1).mkString(": ")

      val result = field match {
        case "id"          => item.setId(propVal)
        case "name"        => item.setName(propVal)
        case "description" => item.setDescription(propVal)
        case "notes"       => item.setDescription(propVal)
        case "path"        => item.setPath(propVal)
        case "filename"    => item match {
          case x: RefItemBean => x.setFilename(propVal)
          case _ => field
        }
        case "tiletype" => item match {
          case x: TileRefItemBean => x.setTiletype(propVal)
          case _ => field
        }
        case _ => field
      }

      result match {
        case x: String => Some("invalid property '" + x + "'")
        case _ => None
      }

    } else {
      Some("invalid propery format")
    }
  }


  // wire up the world using the path fields of the beans
  // assumes that the head of the list is a collection
  def wire(worldList: List[WorldItemBean]): Result[String, CollectionItemBean] = {

    val worldMap = (worldList.map(x => (x.id, x)) ++ worldList.map(x => (x.name, x))).toMap

    if (worldMap.size < worldList.size) {
      Fail("Duplicate IDs!")                 // for now
    } else {

      val errors = worldList.filter(!_.path.equals("")).map(x => {

        // for now, the path property is the id of the parent
        worldMap.get(x.path) match {
          case Some(coll: CollectionItemBean) => {
            coll.children.add(x)
            Pass(x)
          }
          case Some(_) => Fail(s"item '${x.name}' in '${x.srcyml}' declares non-collection parent '${x.path}'")
          case None    => Fail(s"item '${x.name}' in '${x.srcyml}' declares non-visible parent '${x.path}'")
        }

      }).collect({case Fail(x) => x})

      if (errors.length > 0) {
        Fail(errors.mkString("\n"))
      } else {
        // worldMap.get("master") match {
        worldList.headOption match {
          case Some(master: CollectionItemBean) => Pass(master)
          case Some(_) => Fail("head item not a collection")
          case None    => Fail("head item not found")
        }
      }
    }
  }

}



object WorldLoader {


  def loadWorld(projConf: ProjectConfig, fileStatus: FileMap): Result[String, CollectionItem] = {

    // start with YAML or Flat loader depending on the extension of the master file

    Result.fromFilename(projConf.localContentPath / projConf.masterName + ".yml") match {
      case Pass(_) =>  WorldLoaderYML.loadWorld(projConf.localContentPath, projConf.masterName, fileStatus)
      case Fail(_) => WorldLoaderFlat.loadWorld(projConf.localContentPath, projConf.masterName, fileStatus)
    }
  }


  def loadWorld(projConf: ProjectConfig): Result[String, CollectionItem] = {
    loadWorld(projConf, WorldLoader.emptyFileMap)
  }


  def validate(world: CollectionItem): Result[String, CollectionItem] = {

    // check for duplicate IDs
    val worldList = WorldItem.collectionToList(world)
    val duplicateIDs = worldList.groupBy(_.id).toList.sortBy(_._1).filter(_._2.length > 1)
    duplicateIDs.length match {
      case 0 => Pass(world)
      case _ => Fail({
        "Duplicate ids found\n" +
        duplicateIDs.map(x =>
          "\tid: " + x._1 + "\n" +
          "\tpresent in files: " + x._2.map(_.srcyml).distinct.mkString(", ")).mkString("\n")
      })
    }

  }

  def logParseError(filename: String, message: String): String = {
    s"***\nParsing error in ${filename}:\n\n${message}\n***\n"
  }

  // functions for loading and saving FileMaps

  def saveFileMap(filename: String, map: FileMap): Unit = {
    val pw = new java.io.PrintWriter(new java.io.File(filename))
    // scalastyle:ignore regex
    map.foreach(x =>  pw.println(x._1 + "\t" + x._2._1 + "\t" + x._2._2.getValue))
    pw.close
  }

  def loadFileMap(filename: String): FileMap = {
    val lines = FileUtils.readLines(new File(filename), "UTF-8").asScala
    lines.map(x => x.split("\t")).map(x => (x(0), (x(1), new DateTime(x(2).toLong)))).toMap
  }

  def loadOrEmptyModifiedMap(filename: String): FileMap = Result.fromFilename(filename) match {
    case Pass(x) => loadFileMap(x.getPath)
    case Fail(_) => emptyFileMap
  }

  def emptyFileMap(): FileMap = {
    List.empty[(String, (String, DateTime))].toMap
  }

  def mergeFileMaps(map1: FileMap, map2: FileMap): FileMap = {
    map1 ++ map2.map{case (k, v) => k -> {
      map1.get(k) match {
        case Some(x) => if (x._2.getValue > v._2.getValue) x else v
        case None => v
      }
    }}
  }

  // functions for saving and loading ItemMaps

  def saveItemMap(filename: String, map: ItemMap): Unit = {
    val pw = new java.io.PrintWriter(new java.io.File(filename))
    // scalastyle:ignore regex
    map.foreach(x =>  pw.println(x._1 + "\t" + x._2._1 + "\t" + x._2._2))
    pw.close
  }

  def loadItemMap(filename: String): ItemMap = {
    val lines = FileUtils.readLines(new File(filename), "UTF-8").asScala
    lines.map(x => x.split("\t")).map(x => (x(0), (x(1), x(2).toInt))).toMap
  }

  def loadOrEmptyItemMap(filename: String): ItemMap = Result.fromFilename(filename) match {
    case Pass(x) => loadItemMap(x.getPath)
    case Fail(_) => emptyItemMap
  }

  def emptyItemMap(): ItemMap = {
    List.empty[(String, (String, Int))].toMap
  }

}
