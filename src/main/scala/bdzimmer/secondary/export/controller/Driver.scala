// Copyright (c) 2018 Ben Zimmer. All rights reserved.

// Entry point for Secondary application.

package bdzimmer.secondary.export.controller

import java.awt.Desktop    // scalastyle:ignore illegal.imports
import java.io.{BufferedReader, File, InputStreamReader}

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.sys.process._
import scala.util.Try

import org.apache.commons.io.FileUtils

import bdzimmer.util.{Result, Pass, Fail}
import bdzimmer.util.StringUtils._

import bdzimmer.secondary.export.model.{ProjectConfig, ProjectStructure, WorldItems}
import bdzimmer.secondary.export.view.{ConfigurationGUI, ScreenshotUtility}
import bdzimmer.secondary.export.model.Tags

import bdzimmer.pixeleditor.model.{AssetMetadataUtils, Experiment}
import bdzimmer.orbits.{Editor, ConstVelFlightFn}



object Driver {

  val Version = "2023.06.12"
  val Title: String = "Secondary - create worlds from text - v" + Version
  val DefaultCommand: String = DriverCommands.Interactive
  val ServerRefreshSeconds = 10


  def main(argv: Array[String]): Unit = {
    val driver = new Driver()
    driver.run(argv)
  }


  // show help at the command line
  private def showUsage(): Unit = {
    println(Title)
    println()
    println("Usage:")
    println("  secondary <command>")
    println()
    println("Commands:")
    showCommands()
    println("If no command is provided, Secondary will start in interactive mode.")
  }


  // print the list of commands / descriptions
  private def showCommands(): Unit = {
    println()
    val maxLength = DriverCommands.CommandsDescriptions.map(_._1.length).max
    DriverCommands.CommandsDescriptions.foreach(x => {
      val command = x._1
      val description = x._2
      println("  " + command + " " * (maxLength - command.length + 2) + description)
    })
    println()
  }

}



class Driver {

  // project directory is current working directory
  val projectDir: String = System.getProperty("user.dir")
  val projConf: ProjectConfig = ProjectConfig(projectDir)

  var loadedWorld: Result[String, WorldItems.CollectionItem] = Fail("World not loaded!")

  def run(argv: Array[String]): Unit = {
    val (command, args) = argv.toList match {
      case x :: xs => (x, xs)
      case Nil     => (Driver.DefaultCommand, List())
    }
    command match {
      case DriverCommands.Interactive => runInteractive()
      case DriverCommands.Help        => Driver.showUsage()
      case _                          => runCommand(command, args)
    }
  }


  // start the interactive shell
  private def runInteractive(): Unit = {

    println(Driver.Title)
    val br = new BufferedReader(new InputStreamReader(System.in))
    readEval()

    @tailrec
    def readEval() {
      print("> ")
      val (command, args) = br.readLine().split("\\s+").toList match {
        case x :: xs => (x, xs)
        case Nil     => ("", List())
      }

      command match {
        case "exit" | "quit" | "q" => () // quit
        case "" => readEval() // do nothing
        case _  => {
          runCommand(command, args)
          readEval()
        }
      }
    }

  }


  // run a command
  private def runCommand(command: String, args: List[String]): Unit = command match {

    case DriverCommands.Animate => {
      val startTime = System.currentTimeMillis
      val res = for {
        stuff <- getRenderStuff(false, "default")
        (stringToItem, rt, tagsMap) = stuff
        itemId = args.mkString(" ")
        item <- Result.fromOption(
          stringToItem.get(itemId),
          "invalid item name or id '" + itemId + "'")
      } yield {
        Flight.animationToDisk(item, tagsMap, projConf.localExportPath)
        val totalTime = (System.currentTimeMillis - startTime) / 1000.0
        println("exported animation(s) in " + totalTime + " sec")
      }
      res.mapLeft(msg => println(msg))
    }

    case DriverCommands.Browse => browseLocal(findItem(args.mkString(" ")))

    case DriverCommands.Basic | DriverCommands.Epub | DriverCommands.Latex => {
      val startTime = System.currentTimeMillis

      val itemId = args.headOption.getOrElse("")
      val mode = args.lift(1).getOrElse("default")
      val imCompQuality = args.lift(2).flatMap(x => Try(x.toFloat).toOption)

      println("mode: " + mode)
      println("im comp quality: " + imCompQuality)
      println()

      val suffix = if (mode.equals("default")) "" else "_" + mode

      val res = for {
        stuff <- getRenderStuff(true, mode)
        (stringToItem, rt, _) = stuff
        item <- Result.fromOption(
          stringToItem.get(itemId),
          "invalid item name or id '" + itemId + "'")
      } yield {
        val tags = item.tags.mapValues(tag => ParseTags.parse(tag, stringToItem))

        val outputFilename = command match {
          case DriverCommands.Basic => {
            val filename = item.id + suffix + ".htm"
            Basic.export(filename, item.notes, item.name, tags, rt, projConf.localExportPath)
            Pass(filename)
          }
          case DriverCommands.Epub => {
            Result(item.asInstanceOf[WorldItems.BookItem]).map(book => {
              val filename = book.id + suffix + ".epub"
              val tags = book.tags.mapValues(tag => ParseTags.parse(tag, stringToItem))
              val config = Book.getConfig(tags)
              println("Book configuration:")
              println("-------------------")
              println(config)
              Epub.export(
                filename, book, config.editor, tags, rt,
                config.unstyledSections, imCompQuality,
                projConf.localExportPath)
              filename
            }).mapLeft(_ => "'" + itemId + "' not a book")
          }
          case DriverCommands.Latex => {
            Result(item.asInstanceOf[WorldItems.BookItem]).map(book => {
              val filename = book.id + suffix + ".tex"
              val tags = book.tags.mapValues(tag => ParseTags.parse(tag, stringToItem))
              val config = Book.getConfig(tags)
              println("Book configuration:")
              println("-------------------")
              println(config)
              Latex.export(filename, book, tags, config, Some(rt))
              filename
            }).mapLeft(_ => "'" + itemId + "' not a book")
          }

        }

        outputFilename match {
          case Pass(x) => {
            val totalTime = (System.currentTimeMillis - startTime) / 1000.0
            println("exported " + x + " in " + totalTime + " sec")
          }
          case Fail(msg) => {
            // TODO: collapse this into outer message
            println("export failed:" + msg)
          }
        }

      }
      res.mapLeft(msg => println(msg))
    }
    case DriverCommands.Configure => {
      val prop = ProjectConfig.getProperties(projConf.projectDir)
      new ConfigurationGUI(
          prop, ProjectConfig.requiredProperties, "Secondary - Project Configuration")
      println("You must restart Secondary for configuration changes to take effect.")
    }
    case DriverCommands.Content => {
      println("default content dir: " + projConf.localContentPath)
      if (projConf.contentDirs.nonEmpty) {
        println("extra content dirs:")
        projConf.contentDirs.foreach(pair => {
          val (name, path) = pair
          val path_file = new File(path)
          println(
            "\t" + name + "\t" +
            path_file.getAbsolutePath + "\t" + (if (path_file.isDirectory) {"(exists)"} else {"(not found!)"}))
        })
      }
    }
    case DriverCommands.Duplicate => {
      val name = args.mkString(" ")
      val res = for {
        item <- Result.fromOption(findItem(name), "invalid item name or id '" + name + "'")
      } yield {
        val dups = Dup.find(item.notes)
        val lines = item.notes.split("\n")
        dups.foreach({case (lineIdx, (start, end)) => {
          val line = lines(lineIdx)
          val extractStart = Math.max(start - 10, 0)
          val prefix = if (start > 0) "..." else ""
          val extractEnd = Math.min(end + 10, line.length)
          val suffix = if (end < line.length) "..." else ""
          println(lineIdx + ":" + prefix + line.substring(extractStart, extractEnd) + suffix)
        }})
      }
      res.mapLeft(msg => println(msg))
    }
    case DriverCommands.Edit => {
      val name = args.mkString(" ")
      editItemByName(name)
    }
    case DriverCommands.Editor => {
      WorldLoader.loadWorld(projConf) match {
        case Pass(master) => {
          val outputFilename = "assetmetadata.txt"
          AssetMetadataUtils.saveAssetMetadata(outputFilename, WorldItems.assetMetadata(master))

          // new Main(projConf.localContentPath, "Secondary Editor", outputFilename)
          Experiment.main(Array())

        }
        case Fail(msg) => println(msg)
      }
    }
    case DriverCommands.Explore => explore()
    case DriverCommands.Export => {
      val startTime = System.currentTimeMillis
      if (args.mkString("").equals("force")) {
        // delete status files
        new File(ProjectStructure.ItemStatusFile).delete()
        new File(ProjectStructure.MetaStatusFile).delete()
        new File(ProjectStructure.RefStatusFile).delete()
      }
      val result = new ExportProcess(projConf).run()
      val totalTime = (System.currentTimeMillis - startTime) / 1000.0
      println("completed in " + totalTime + " sec")
      loadedWorld = result.mapLeft(_ => "Failed to load world during export.")
    }
    case DriverCommands.Orbits => {
      WorldLoader.loadWorld(projConf) match {
        case Pass(master) => {

          val world = WorldItems.collectionToList(master)
          val stringToItem = (world.map(x => (x.id, x)) ++ world.map(x => (x.name, x))).toMap
          val tagsMap = world.map(x => (x.uid, x.tags.mapValues(tag => ParseTags.parse(tag, stringToItem)))).toMap

          // TODO: move this logic somewhere else

          // all trips
          val trips: List[WorldItems.TripItem] = world.collect({
            case x: WorldItems.TripItem => x})

          // all flights
          val flights = trips.flatMap(tripItem => {
            tagsMap.getOrElse(tripItem.uid, Map()).values.collect({
              case x: Tags.Flight => Flight.flightParams(x, tagsMap)
            }).toList
          }).sortBy(_.startDate.julian) // probably a way to avoid this flatten

          // collect two kinds of epochs

          val tripEpochs: List[(String, Double, Double)] = trips.map(tripItem => {
            val flightTags: List[Tags.Flight] = tagsMap.getOrElse(tripItem.uid, Map()).values.collect({
              case x: Tags.Flight => x
            }).toList
            (
              tripItem.name,
              flightTags.map(_.startDate.julian).min,
              flightTags.map(_.endDate.julian).max
            )
          })

          val tagEpochs: List[(String, Double, Double)] = trips.flatMap(tripItem => {
            val epochTags: List[Tags.FlightEpoch] = tagsMap.getOrElse(tripItem.uid, Map()).values.collect({
              case x: Tags.FlightEpoch => x
            }).toList
            epochTags.map(x => (x.name, x.startDate.julian, x.endDate.julian))
          })

          val allEpochs = tripEpochs ++ tagEpochs
          // TODO: add a total epoch?

          val ships = world.filter(x => {
            // probably a better way to write this
            // ships are all items that have at least one spacecraft property tag
            tagsMap.getOrElse(x.uid, Map()).values.collect({case x: Tags.SpacecraftProperty => x}).headOption.isDefined})
          val orbitsShips = (for {
            shipItem <- ships
            (mass, accel, vel) = Flight.spacecraft(shipItem, tagsMap)
          } yield {
            if (vel > ConstVelFlightFn.VelMin) {
              (shipItem.id, bdzimmer.orbits.ConstVelCraft(shipItem.name, vel))
            } else {
              (shipItem.id, bdzimmer.orbits.ConstAccelCraft(shipItem.name, mass, accel))
            }
          }).toMap

          // TODO: use logic from secondary - Flight module

          // a flight will not appear if the ship, orig, or destination is invalid
          val orbitsFlights = for {
            x <- flights
            flightShip <- orbitsShips.get(x.ship.id)
            // orig <- bdzimmer.orbits.MeeusPlanets.Planets.get(x.startLocation)
            // dest <- bdzimmer.orbits.MeeusPlanets.Planets.get(x.endLocation)
            orig <- bdzimmer.orbits.Locations.StatesMap.get(x.startLocation)
            dest <- bdzimmer.orbits.Locations.StatesMap.get(x.endLocation)
          } yield {
            bdzimmer.orbits.SimpleFlightParams(
              ship=flightShip,
              origName=x.startLocation,
              destName=x.endLocation,
              orig=orig,
              dest=dest,
              startDate=x.startDate,
              endDate=x.endDate,
              passengers=x.passengers.map(_.name),
              faction=x.faction,
              description="")
          }

          val styles = bdzimmer.orbits.IO.loadStyles(Editor.StylesFilename)
          val factions = bdzimmer.orbits.IO.loadFactions(Editor.FactionsFilename)

          new Editor(
            orbitsFlights,
            orbitsShips.values.toList,
            allEpochs,
            styles,
            factions)
        }
        case Fail(msg) => println(msg)
      }

    }
    case DriverCommands.Screenshot => new ScreenshotUtility(projConf.localContentPath)
    case DriverCommands.Server => serverMode(Driver.ServerRefreshSeconds)
    case DriverCommands.Sprint => WordCount.interactive(() => findItem(args.mkString(" ")))
    case DriverCommands.Styles => ExportPipeline.addStyles(projConf)
    case DriverCommands.WordCount => {
      val item = findItem(args.mkString(" "))
      val msg = item match {
        case Some(x) => WordCount.calculate(x, recursive = true, sections = false)
        case None => s"Item '$item' not found."
      }
      println(msg)
    }
    case DriverCommands.Help   => Driver.showCommands()
    case _                     => println("Invalid command. Use 'help' for a list of commands.")
  }


  private def findItem(name: String): Option[WorldItems.WorldItem] = {
    val masterOption = WorldLoader.loadWorld(projConf) match {
      case Pass(x) => Some(x)
      case Fail(_) => None
    }
    masterOption.flatMap(master => {
      val world = WorldItems.collectionToList(master)
      world.find(item => item.id.equals(name) || item.name.equals(name))
    })
  }


  private def getRenderStuff(ebookMode: Boolean, mode: String): Result[String, (Map[String, WorldItems.WorldItem], RenderTags, Map[Int, Map[Int, Tags.ParsedTag]])] = {
    for {
      master <- WorldLoader.loadWorld(projConf)
      world = WorldItems.collectionToList(master)
      stringToItem = (world.map(x => (x.id, x)) ++ world.map(x => (x.name, x))).toMap
      tagsMap = world.map(x => (x.uid, x.tags.mapValues(tag => ParseTags.parse(tag, stringToItem)))).toMap
      rt = new RenderTags(
        tagsMap, world.collect({ case x: WorldItems.CharacterItem => x }),
        disableTrees = true,
        ebookMode = ebookMode,
        mode = mode
      )
    } yield {
      (stringToItem, rt, tagsMap)
    }

  }


  // browse to the local copy of the project website
  private def browseLocal(item: Option[WorldItems.WorldItem]): Unit = {
    val pagefilename = item.map(_.id + ".html").getOrElse("index.html")
    val filename = projConf.projectDir / ProjectStructure.WebDir / pagefilename
    val uri = new File(filename).toURI
    Try(Desktop.getDesktop.browse(uri))
  }

  // edit the source file associated with an object by name or id
  private def editItemByName(name: String): Unit = loadedWorld match {
    case Pass(master) => {
      val world = WorldItems.collectionToList(master)
      world.find(item => item.id.equals(name) || item.name.equals(name)) match {
        case Some(item) => editItem(item)
        case None       => println("No such item!")
      }
    }
    case Fail(msg) => println(msg)
  }

  // edit an item's source file locally or in Drive
  private def editItem(item: WorldItems.WorldItem): Unit = {
    val srcFile = new File(projConf.localContentPath / item.srcfilename)
    val idMatcher =  s"\\s*id:\\s*${item.id}"
    val lineNumber = FileUtils.readLines(srcFile).asScala.zipWithIndex.find(x => {
      x._1.matches(idMatcher)
    }).fold(1)(_._2)
    // Desktop.getDesktop.open(srcFile)
    val nppCommand = s"""notepad++ "${srcFile.getPath}" -n$lineNumber"""
    Try(nppCommand.!!)
  }

  // explore the project's source directory
  private def explore(): Unit = Desktop.getDesktop.open(projConf.localContentPathFile)

  private def serverMode(seconds: Int): Unit = {
    while(true) {
      runCommand(DriverCommands.Export, List())
      Thread.sleep(seconds * 1000)
    }
  }

}



object DriverCommands {

  val Animate     = "animate"
  val Basic       = "basic"
  val Browse      = "browse"
  val Configure   = "configure"
  val Content     = "content"
  val Duplicate   = "duplicate"
  val Edit        = "edit"
  val Editor      = "editor"
  val Epub        = "epub"
  val Explore     = "explore"
  val Export      = "export"
  val Latex       = "latex"
  val Orbits      = "orbits"
  val Screenshot  = "screenshot"
  val Server      = "server"
  val Sprint      = "sprint"
  val Styles      = "styles"
  val WordCount   = "wc"

  val Interactive = "interactive"
  val Help        = "help"

  val CommandsDescriptions = List(
    (Animate,     "render flight animation"),
    (Basic,       "export an item to basic HTML"),
    (Browse,      "browse exported project web site"),
    (Configure,   "edit project configuration"),
    (Content,     "verify content directories"),
    (Duplicate,   "detect duplicate words"),
    (Edit,        "edit the source file for an item"),
    (Editor,      "pixel editor (alpha)"),
    (Epub,        "export a book to EPUB"),
    (Explore,     "explore project content dir"),
    (Export,      "export website"),
    (Latex,       "export a book to LaTeX"),
    (Orbits,      "orbits editor (alpha)"),
    (Screenshot,  "screenshot utility"),
    (Server,      "server mode"),
    (Sprint,      "interactive writing sprint tool"),
    (Styles,      "add stylesheets"),
    (WordCount,   "count words recursively in an item"),
    (Help,        "show usage / commands"))
}
