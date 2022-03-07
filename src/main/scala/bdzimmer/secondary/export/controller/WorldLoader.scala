// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Load WorldItem hierarchies from the file system.

package bdzimmer.secondary.export.controller

import java.io.{BufferedReader, File, FileReader}

import scala.collection.JavaConverters._

import org.apache.commons.io.FileUtils

import bdzimmer.util.{Result, Pass, Fail}
import bdzimmer.util.StringUtils._

import bdzimmer.secondary.export.model.{ProjectConfig, WorldItems}
import bdzimmer.secondary.export.model.WorldItems._
import bdzimmer.secondary.export.model.WorldItemBeans._


object WorldLoaderFlat {

  val Trimmer = """^(.*?)\s*$""".r

  def loadWorld(
      inputDir: String,
      masterName: String,
      fileStatus: FileMap): Result[String, CollectionItem] = {

    val masterFilename = masterName + ".sec"

    for {
      // load the world from sec files
      worldList <- loadFile(masterFilename, inputDir, fileStatus, List(masterFilename))
      world <- wire(worldList)                     // wire up using paths attributes and harden
      checkedDuplicates <- WorldLoader.checkDuplicates(world.getVal) // validate
      checkedEmptyIds <- WorldLoader.checkEmptyIds(checkedDuplicates)
    } yield {
      checkedEmptyIds
    }

  }


  def loadFile(
      filename: String,
      inputDir: String,
      fileStatus: FileMap,
      loadedFiles: List[String]): Result[String, List[WorldItemBean]] = {

    for {
      inputFile <- Result.fromFilename(inputDir / filename)
      items     <- parseFile(inputFile, fileStatus)

      resultItems = items.map(item => item match {
        case x: SrcIncludeBean => if (loadedFiles.contains(x.filename)) {
          Fail(s"""'${filename}' contains circular reference to file '${x.filename}'""")
        } else {
          loadFile(x.filename, inputDir, fileStatus, x.filename +: loadedFiles)
        }
        case _                  => Pass(List(item))
      })

      // gather error messages
      parseFails = resultItems.collect({case Fail(x) => x})

      // if any errors, yield an error
      result <- if (parseFails.length > 0) {
        Fail(parseFails.mkString("\n"))
      } else {
        Pass(resultItems.collect({case Pass(x) => x}).flatten)
      }

    } yield {
      result
    }

  }


  def parseFile(inputFile: File, fileStatus: FileMap): Result[String, List[WorldItemBean]] = {

    val inNothing = 0
    val inHeader = 1
    val inNotes = 2

    val items = scala.collection.mutable.Buffer[WorldItemBean]()

    Result({

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
            if (item.notes.isEmpty && notes.nonEmpty) {
              item.notes = notes.mkString("\n")
            }
            if (item.id.equals("") && !item.name.equals("")) {
              item.id = nameToId(item.name)
            }
            items += item
            notes.clear()
          }

          // create a new item based on this line
          item = WorldLoaderFlat.itemBeanFactory(trimmed)
          item.srcfilename = inputFile.getName
          item.remoteid = fileStatus.get(inputFile.getName).map(_._1).getOrElse("")
          state = inHeader

        } else if (state == inHeader && line.equals("")) {
          state = inNotes
        } else if (state == inHeader) {
          // set properties of the item
          setProperty(item, trimmed).map(x => {
            throw(new Exception(inputFile.getName + " " + lineCount + ": '" + line + "'\n\t" + x))
          })
        } else if (state == inNotes) {
          notes += trimmed
        }
        line = br.readLine()
        lineCount = lineCount + 1
      }

      if (item != null) {
        if (item.notes.isEmpty && notes.nonEmpty) {
          item.notes = notes.mkString("\n")
        }
        if (item.id.equals("") && !item.name.equals("")) {
          item.id = nameToId(item.name)
        }
        items += item
      }

      br.close()

      items.toList
    })

  }

  def nameToId(id: String): String = {
    // remove all punctuation and trim trailing whitespace
    // (there shouldn't be any leading whitespace)
    val noPunctuation = id.replaceAll("[^A-Za-z0-9\\s]", "")
    val Trimmer(trimmed) = noPunctuation // annoying syntax
    // replace whitespace with underscore; convert to lowercase
    trimmed.replaceAll("\\s+", "_").toLowerCase
  }

  def itemBeanFactory(typeName: String): WorldItemBean = typeName match {
    case "!collection"  => new CollectionItemBean()
    case "!thing"       => new ThingItemBean()
    case "!item"        => new ThingItemBean()
    case "!place"       => new PlaceItemBean()
    case "!location"    => new PlaceItemBean()
    case "!character"   => new CharacterItemBean()
    case "!person"      => new CharacterItemBean()
    case "!image"       => new ImageFileItemBean()
    case "!tileset"     => new TilesetItemBean()
    case "!spritesheet" => new SpritesheetItemBean()
    case "!map"         => new MapItemBean()
    case "!include"     => new SrcIncludeBean()
    case "!trip"        => new TripItemBean()
    case "!book"        => new BookItemBean()
    case _              => new ThingItemBean()
  }


  def setProperty(item: WorldItemBean, property: String): Option[String] = {

    val splitted = property.split(":\\s+")
    if (splitted.length >= 2) {

      val field = splitted(0)
      val propVal = splitted.drop(1).mkString(": ")

      val result = field match {

        // fields present in all item types
        case "id"          => item.setId(propVal)
        case "name"        => item.setName(propVal)
        case "description" => item.setDescription(propVal)
        case "notes"       => item.setNotes(propVal)
        case "path"        => item.setPath(propVal)
        case "parent"      => item.setPath(propVal)

        // fields which may or may not be present for certain item types
        case "filename"    => item match {
          case x: RefItemBean => x.setFilename(propVal)
          case _              => field
        }
        case "tiletype" => item match {
          case x: TileRefItemBean => x.setTiletype(propVal)
          case _                  => field
        }
        case "uniqueid" => item match {
          case x: BookItemBean => x.setUniqueIdentifier(propVal)
          case _               => field
        }
        case "authorname" => item match {
          case x: BookItemBean => x.setAuthorname(propVal)
          case _               => field
        }
        case "config" => item match {
          case x: BookItemBean => x.setConfig(propVal)
          case _               => field
        }
        case _ => field
      }

      result match {
        case x: String => Some("invalid property '" + x + "'")
        case _         => None
      }

    } else {
      Some("invalid property format")
    }
  }


  // wire up the world using the path fields of the beans
  def wire(worldList: List[WorldItemBean]): Result[String, CollectionItemBean] = {

    val worldMap = (worldList.map(x => (x.id, x)) ++ worldList.map(x => (x.name, x))).toMap

    if (worldMap.size < worldList.size) {
      Fail("Duplicate IDs!")                 // for now
    } else {

      val errors = worldList.filter(!_.path.equals("")).map(x => {

        // for now, the path property is the id or name of the parent
        worldMap.get(x.path) match {
          case Some(coll: CollectionItemBean) => {
            coll.children.add(x)
            Pass(x)
          }
          case Some(_) => Fail(s"item '${x.name}' in '${x.srcfilename}' declares non-collection parent '${x.path}'")
          case None    => Fail(s"item '${x.name}' in '${x.srcfilename}' declares non-visible parent '${x.path}'")
        }

      }).collect({case Fail(x) => x})

      if (errors.length > 0) {
        Fail(errors.mkString("\n"))
      } else {
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
    WorldLoaderFlat.loadWorld(projConf.localContentPath, projConf.masterName, fileStatus)
  }


  def loadWorld(projConf: ProjectConfig): Result[String, CollectionItem] = {
    loadWorld(projConf, WorldLoader.emptyFileMap)
  }


  def checkDuplicates(world: CollectionItem): Result[String, CollectionItem] = {
    // check for duplicate IDs
    val worldList = WorldItems.collectionToList(world)
    val duplicateIDs = worldList.groupBy(_.id).toList.sortBy(_._1).filter(_._2.length > 1)
    duplicateIDs.length match {
      case 0 => Pass(world)
      case _ => Fail(
        "Duplicate ids found\n" +
        duplicateIDs.map(x =>
          "\tid: " + x._1 + "\n" +
          "\tpresent in files: " + x._2.map(_.srcfilename).distinct.mkString(", ")).mkString("\n")
      )
    }

  }


  def checkEmptyIds(world: CollectionItem): Result[String, CollectionItem] = {
    // check for empty IDs
    val worldList = WorldItems.collectionToList(world)
    val emptyIds = worldList.filter(_.id.length == 0)
    emptyIds.length match {
      case 0 => Pass(world)
      case _ => Fail(
        "Empty ids found\n" +
        "\tpresent in files: " + emptyIds.map(_.srcfilename).distinct.mkString(", ")
      )
    }
  }


  def logParseError(filename: String, message: String): String = {
    s"***\nParsing error in ${filename}:\n\n${message}\n***\n"
  }

  // functions for loading and saving FileMaps

  def saveFileMap(filename: String, map: FileMap): Unit = {
    val pw = new java.io.PrintWriter(new java.io.File(filename))
    // scalastyle:ignore regex
    map.foreach(x =>  pw.println(x._1 + "\t" + x._2._1 + "\t" + x._2._2))
    pw.close
  }

  def loadFileMap(filename: String): FileMap = {
    val lines = FileUtils.readLines(new File(filename), "UTF-8").asScala
    lines.map(x => x.split("\t")).map(x => (x(0), (x(1), x(2).toLong))).toMap
  }

  def loadOrEmptyModifiedMap(filename: String): FileMap = Result.fromFilename(filename) match {
    case Pass(x) => loadFileMap(x.getPath)
    case Fail(_) => emptyFileMap
  }

  def emptyFileMap(): FileMap = {
    List.empty[(String, (String, Long))].toMap
  }

  def mergeFileMaps(map1: FileMap, map2: FileMap): FileMap = {
    map1 ++ map2.map{case (k, v) => k -> {
      map1.get(k) match {
        case Some(x) => if (x._2 > v._2) x else v
        case None    => v
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
