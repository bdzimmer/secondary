// Copyright (c) 2018 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.export.controller

import java.io.File

import bdzimmer.orbits.{Animation, CalendarDateTime, ConstVelFlightFn, Editor, IO, Style}
import bdzimmer.secondary.export.model.{SecTags, Tags, WorldItems}
import bdzimmer.secondary.export.view.Html
import bdzimmer.util.StringUtils._


object Flight {

  val AnimationsDir = "animations"

  def render(fp: FlightParams, imagePath: String): String = {
     val shipName = fp.ship.name
      val passengersString = if (fp.passengers.isEmpty) {
        ""
      } else {
        " with " + fp.passengers.map(x => RenderPages.textLinkPage(x)).mkString(", ")
      }

      // generate image and text summary
      Html.image(imagePath, responsive = true) + "\n" +
      RenderPages.textLinkPage(fp.ship) + " travels from " + fp.startLocation + " to " + fp.endLocation + passengersString + ".\n" +
      Html.listGroup(
        List(
          genShow(
            fp.startDate.dateTimeString, shipName + " departs from " + fp.startLocation + "."),
          genShow(
            fp.endDate.dateTimeString,   shipName + " arrives at "   + fp.endLocation   + ".")
        ).map(Html.listItem(_))
      )
  }


  def flightParams(
      flight: Tags.Flight,
      tagsMap: Map[Int, Map[Int, Tags.ParsedTag]]): FlightParams = {

    val (mass, accel, vel) = spacecraft(flight.ship, tagsMap)

    FlightParams(
      flight.ship,
      flight.startLocation,
      flight.endLocation,
      flight.startDate,
      flight.endDate,
      mass,
      accel,
      vel,
      flight.passengers,
      flight.faction)
  }


  def flightParamsOrbits(fp: FlightParams): Option[bdzimmer.orbits.FlightParams] = {

    val ship = if (fp.vel > ConstVelFlightFn.VelMin) {
      bdzimmer.orbits.ConstVelCraft(fp.ship.name, fp.vel)
    } else {
      bdzimmer.orbits.ConstAccelCraft(fp.ship.name, fp.mass, fp.accel)
    }

    for {
      // orig <- bdzimmer.orbits.MeeusPlanets.Planets.get(fp.startLocation)
      // dest <- bdzimmer.orbits.MeeusPlanets.Planets.get(fp.endLocation)
      orig <- bdzimmer.orbits.Locations.StatesMap.get(fp.startLocation)
      dest <- bdzimmer.orbits.Locations.StatesMap.get(fp.endLocation)
    } yield {
      bdzimmer.orbits.SimpleFlightParams(
        ship=ship,
        origName=fp.startLocation,
        destName=fp.endLocation,
        orig=orig,
        dest=dest,
        startDate=fp.startDate,
        endDate=fp.endDate,
        passengers=fp.passengers.map(_.name),
        faction=fp.faction,
        description="")
    }

  }


  def spacecraft(
      ship: WorldItems.WorldItem,
      tagsMap: Map[Int, Map[Int, Tags.ParsedTag]]): (Double, Double, Double) = {

    // TODO: handle specification of different units

    val mass = (for {
      shipTags <- tagsMap.get(ship.uid)
      massTag <- shipTags.values.collect({
        case x: Tags.SpacecraftProperty => x
      }).find(x => x.kind.equals(SecTags.Mass))
    } yield {
      massTag.value
    }).getOrElse(1000.0) // tonnes

    val accel = (for {
      shipTags <- tagsMap.get(ship.uid)
      accelTag <- shipTags.values.collect({
        case x: Tags.SpacecraftProperty => x
      }).find(x => x.kind.equals(SecTags.Acceleration))
    } yield {
      accelTag.value
    }).getOrElse(0.25) // AU / day^2

    val vel = (for {
      shipTags <- tagsMap.get(ship.uid)
      velTag <- shipTags.values.collect({
        case x: Tags.SpacecraftProperty => x
      }).find(x => x.kind.equals(SecTags.Velocity))
    } yield {
      velTag.value
    }).getOrElse(0.0) // AU / day

    (mass, accel, vel)

  }


  def epoch(
      item: WorldItems.WorldItem,
      name: String,
      tagsMap: Map[Int, Map[Int, Tags.ParsedTag]]): Option[Tags.FlightEpoch] = {

    val epochs = tagsMap.getOrElse(item.uid, Map()).values.collect({case x: Tags.FlightEpoch => x})
    epochs.find(_.name.equals(name))

  }


  def animationToDisk(
      item: WorldItems.WorldItem,
      tagsMap: Map[Int, Map[Int, Tags.ParsedTag]],
      location: String): Unit = {

    val tags = tagsMap.getOrElse(item.uid, Map()).toList.sortBy(x => x._1).map(_._2)
    val flightTags = tags.collect({case x: Tags.Flight => x})
    val animationTags = tags.collect({case x: Tags.FlightAnimation => x})

    val styles = IO.loadStyles(Editor.StylesFilename)
    val factions = IO.loadFactions(Editor.FactionsFilename)

    animationTags.foreach(anim => {

      val epoch = Flight.epoch(anim.item, anim.epoch, tagsMap).getOrElse(
        Tags.FlightEpoch(
          "default",
          CalendarDateTime(2016, 1, 1, 12, 0, 0.0),
          CalendarDateTime(2016, 2, 1, 12, 0, 0.0)))
      val uniqueName = animationName(anim)
      val outputDirname = location / AnimationsDir / uniqueName
      new File(outputDirname).mkdirs()

      // This must always be updated to be a *complete* printout
      // of the stuff in anim. These settings are complex, and it's
      // important to be able to verify that I'm rendering what I
      // think I am.

      println("-----------------------")
      println("item: " + anim.item.name + " (" + anim.item.id + ")")
      println("epoch:")
      println("\tname:      " + epoch.name)
      println("\tstartDate: " + epoch.startDate.dateTimeString)
      println("\tendDate:   " + epoch.endDate.dateTimeString)
      println("settings:")
      println("\twidth:    " + anim.settings.width)
      println("\theight:   " + anim.settings.height)
      println("\tcamType:  " + anim.settings.camType)
      println("\tcamPos:   [ " +
        anim.settings.camPos.x + " " +
        anim.settings.camPos.y + " " +
        anim.settings.camPos.z + " ]")
      println("\tzViewPos: " + anim.settings.zViewPos)
      println("\tfps:      " + anim.settings.fps)
      println("\tinterval: " + anim.settings.interval)
      println("\tdamping:  " + anim.settings.damping)
      println("status: " + anim.status)
      println("visible:")
      anim.visible.foreach(x => println("\t" + x))
      println("style:  " + anim.style)
      println()
      println("output directory: " + outputDirname)
      println("-----------------------")

      val viewerSettings = styles.getOrElse(anim.style, Style.ViewerSettingsDefault)

      // load flights from the item
      val flights = flightTags.flatMap(
        tag => Flight.flightParamsOrbits(Flight.flightParams(tag, tagsMap)))

      Animation.animateFlights(
        flights,
        epoch.startDate,
        epoch.endDate,
        factions,
        Editor.ShowSettingsDefault.copy(
          flightStatus=anim.status
          // orbitInfo=true // just for testing
        ),
        viewerSettings,
        anim.settings,
        outputDirname
      )

    })

  }


  def animationName(anim: Tags.FlightAnimation): String = {
    "flightanimation_" + anim.hashCode.toHexString
  }


  private def genShow(fst: String, snd: String): String = {
    s"""<b>$fst: </b> """ + snd + Html.brInline
  }


}

