// Copyright (c) 2018 Ben Zimmer. All rights reserved.

// Entry point for Secondary application.

package bdzimmer.secondary.export.controller

import java.awt.Desktop    // scalastyle:ignore illegal.imports
import java.net.URI
import java.io.{BufferedReader, File, InputStreamReader}

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.sys.process._
import scala.util.Try

import org.apache.commons.io.FileUtils

import bdzimmer.util.{Result, Pass, Fail, PropertiesWrapper}
import bdzimmer.util.StringUtils._

import bdzimmer.secondary.export.model.{ProjectConfig, ProjectStructure, WorldItems}
import bdzimmer.secondary.export.view.{ConfigurationGUI, ScreenshotUtility}
import bdzimmer.secondary.export.model.Tags

import bdzimmer.pixeleditor.view.Main
import bdzimmer.pixeleditor.model.AssetMetadataUtils
import bdzimmer.orbits.Editor


class Driver {

  // project directory is current working directory
  val projectDir = System.getProperty("user.dir")
  val projConf = ProjectConfig(projectDir)

  var loadedWorld: Result[String, WorldItems.CollectionItem] = Fail("World not loaded!")

  def run(args: Array[String]): Unit = {
    val command = args.headOption.getOrElse(Driver.DefaultCommand)
    command match {
      case DriverCommands.Interactive => runInteractive
      case DriverCommands.Help        => Driver.showUsage
      case _                          => runCommand(command, List())
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
    case DriverCommands.Browse      => browseLocal
    case DriverCommands.Configure => {
      val prop = ProjectConfig.getProperties(projConf.projectDir)
      new ConfigurationGUI(
          prop, ProjectConfig.requiredProperties, "Secondary - Project Configuration").startup(Array())
      println("You must restart Secondary for configuration changes to take effect.")
    }
    case DriverCommands.Edit => {
      val name = args.mkString(" ")
      editItemByName(name)
    }
    case DriverCommands.Editor => {
      val master = WorldLoader.loadWorld(projConf) match {
        case Pass(master) => {
          val outputFilename = "assetmetadata.txt"
          AssetMetadataUtils.saveAssetMetadata(outputFilename, WorldItems.assetMetadata(master))
          new Main(projConf.localContentPath, "Secondary Editor", outputFilename)
        }
        case Fail(msg) => println(msg)
      }
    }
    case DriverCommands.Explore => explore()
    case DriverCommands.Export => {
      val startTime = System.currentTimeMillis
      val result = new ExportProcess(projConf).run()
      val totalTime = (System.currentTimeMillis - startTime) / 1000.0
      println("completed in " + totalTime + " sec")
      loadedWorld = result.mapLeft(_ => "Failed to load world during export.")
    }
    case DriverCommands.Orbits => {
      val master = WorldLoader.loadWorld(projConf) match {
        case Pass(master) => {

          val world = WorldItems.collectionToList(master)
          val stringToItem = (world.map(x => (x.id, x)) ++ world.map(x => (x.name, x))).toMap
          val stringToTags = world.map(x => (x.id, x.tags.mapValues(tag => ParseTags.parse(tag, stringToItem)))).toMap

          val flights = world.collect({
            case tripItem: WorldItems.TripItem => {
              stringToTags.getOrElse(tripItem.id, Map()).values.collect({
                case x: Tags.Flight => Flight.flightParams(x, stringToTags)
              }).toList
            }
          }).flatten.sortBy(_.startDate.julian) // probably a way to avoid this flatten

          val ships = world.filter(x => {
            // probably a better way to write this
            // ships are all items that have at least one spacecraft property tag
            stringToTags.getOrElse(x.id, Map()).values.collect({case x: Tags.SpacecraftProperty => x}).headOption.isDefined
          })

          val orbitsShips = ships.map(x => (x, Flight.spacecraft(x, stringToTags))).map(x => (x._1.id, bdzimmer.orbits.Spacecraft(x._1.name, x._2._1, x._2._2))).toMap
          // val defaultShip = bdzimmer.orbits.Spacecraft("undefined", 0.0, 0.0)

          // a flight will not appear if the ship, orig, or destination is invalid
          val orbitsFlights = for {
            x <- flights
            flightShip <- orbitsShips.get(x.ship.id)
            orig <- bdzimmer.orbits.MeeusPlanets.Planets.get(x.startLocation)
            dest <- bdzimmer.orbits.MeeusPlanets.Planets.get(x.endLocation)
          } yield {
            bdzimmer.orbits.FlightParams(
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

          new Editor(orbitsFlights, orbitsShips.values.toList)
        }
        case Fail(msg) => println(msg)
      }

    }
    case DriverCommands.Screenshot => new ScreenshotUtility(projConf.localContentPath)
    case DriverCommands.Server => serverMode(Driver.ServerRefreshSeconds)
    case DriverCommands.Styles => ExportPipeline.addStyles(projConf)
    case DriverCommands.Help   => Driver.showCommands
    case _                     => println("Invalid command. Use 'help' for a list of commands.")
  }


  // browse to the local copy of the project website
  private def browseLocal(): Unit = {
    val filename = projConf.projectDir / ProjectStructure.WebDir / "index.html"
    val uri = new File(filename).toURI
    Try(Desktop.getDesktop.browse(uri))
  }

  // edit the source file associated with an object by name or id
  private def editItemByName(name: String): Unit = loadedWorld match {
    case Pass(master) => {
      val world = WorldItems.collectionToList(master)
      world.filter(item => item.id.equals(name) || item.name.equals(name)).headOption match {
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
    val nppCommand = s"""notepad++ "${srcFile.getPath}" -n${lineNumber}"""
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



object Driver {

  val Title = "Secondary - create worlds from text - v2018.01.01"
  val DefaultCommand = DriverCommands.Interactive
  val ServerRefreshSeconds = 10

  def main(args: Array[String]): Unit = {
    val driver = new Driver()
    driver.run(args)
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



object DriverCommands {

  val Browse      = "browse"
  val Configure   = "configure"
  val Edit        = "edit"
  val Editor      = "editor"
  val Explore     = "explore"
  val Export      = "export"
  val Orbits      = "orbits"
  val Screenshot  = "screenshot"
  val Server      = "server"
  val Styles      = "styles"

  val Interactive = "interactive"
  val Help        = "help"

  val CommandsDescriptions = List(
      (Browse,      "browse exported project web site"),
      (Configure,   "edit project configuration"),
      (Edit,        "edit the source file for an item"),
      (Editor,      "pixel editor (alpha)"),
      (Explore,     "explore project content dir"),
      (Export,      "export"),
      (Orbits,      "orbits editor (alpha)"),
      (Screenshot,  "screenshot utility"),
      (Server,      "server mode"),
      (Styles,      "add stylesheets"),
      (Help,        "show usage / commands"))
}
