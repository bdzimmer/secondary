// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.export.model

import scala.util.matching.Regex
import org.apache.commons.io.FileUtils


case class SecTag(kind: String, value: String, args: List[String])


object SecTags {

  val Link = "link"
  val Image = "image"
  val ImageResponsive = "image-responsive"
  val FamilyTree = "familytree"
  val Jumbotron = "jumbotron"
  val Marriage = "marriage"
  val Timeline = "timeline"
  val Flight = "flight"

  val Birth = "birth"
  val Death = "death"
  val Event = "event"

  val Mass = "mass"
  val Acceleration = "acceleration"

  val Demo = "demo"

  val ItemTagKinds = List(
      Link, Image, ImageResponsive,
      FamilyTree, Jumbotron, Marriage, Timeline, Flight)

  val EventTagKinds = List(Birth, Death, Event)
  val TaskTagKinds = List("thought", "todo", "started", "done", "blocked")
  val SpacecraftPropTagKinds = List(Mass, Acceleration)
  val OtherTagKinds = Demo +: (EventTagKinds ++ TaskTagKinds ++ SpacecraftPropTagKinds)

  val NonItemTagKinds = EventTagKinds ++ TaskTagKinds ++ OtherTagKinds

}


object ParseSecTags {

  val matcher = "\\{\\{(.*?)\\}\\}".r

  def getAllTags(text: String): List[SecTag] = {
    matcher.findAllMatchIn(text).map(m => {
      getTag(m.group(1))
    }).toList
  }


  // generate a tag from text
  def getTag(tagText: String): SecTag = {

    tagText.split(":\\s+").toList match {
      case kind :: text :: extra => text.split("\\s*\\|\\s*").toList match {
        case value :: args => SecTag(kind.toLowerCase(), value, args)
        case _ => SecTag(kind.toLowerCase(), text, List())
      }
      case _ => SecTag("link", tagText, List())
    }

  }


  def parseArgs(args: List[String]): Map[String, String] = {
    args.map(x => x.split("=").toList match {
      case fst :: snd :: xs => Some((fst, snd))
      case _ => None
    }).flatten.toMap
  }


}
