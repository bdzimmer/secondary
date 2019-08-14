// Copyright (c) 2018 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.export.controller

import bdzimmer.orbits.ConstVelFlightFn
import bdzimmer.secondary.export.model.{SecTags, Tags, WorldItems}
import bdzimmer.secondary.export.view.Html


object Flight {

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
      stringToTags: Map[String, Map[Int, Tags.ParsedTag]]): FlightParams = {

    val (mass, accel, vel) = spacecraft(flight.ship, stringToTags)

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
      orig <- bdzimmer.orbits.MeeusPlanets.Planets.get(fp.startLocation)
      dest <- bdzimmer.orbits.MeeusPlanets.Planets.get(fp.endLocation)
    } yield {
      bdzimmer.orbits.FlightParams(
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
      stringToTags: Map[String, Map[Int, Tags.ParsedTag]]): (Double, Double, Double) = {

    // TODO: handle specification of different units

    val mass = (for {
      shipTags <- stringToTags.get(ship.id)
      massTag <- shipTags.values.collect({
        case x: Tags.SpacecraftProperty => x
      }).find(x => x.kind.equals(SecTags.Mass))
    } yield {
      massTag.value
    }).getOrElse(1000.0) // tonnes

    val accel = (for {
      shipTags <- stringToTags.get(ship.id)
      accelTag <- shipTags.values.collect({
        case x: Tags.SpacecraftProperty => x
      }).find(x => x.kind.equals(SecTags.Acceleration))
    } yield {
      accelTag.value
    }).getOrElse(0.25) // AU / day^2

    val vel = (for {
      shipTags <- stringToTags.get(ship.id)
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
      stringToTags: Map[String, Map[Int, Tags.ParsedTag]]): Option[Tags.FlightEpoch] = {

    stringToTags.get(item.id).flatMap(_.values.collectFirst({case x: Tags.FlightEpoch => x}))

  }




  private def genShow(fst: String, snd: String): String = {
    s"""<b>$fst: </b> """ + snd + Html.brInline
  }


}

