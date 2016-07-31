// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Functionality for parsing markdown and special Secondary tags.

// 2015-08-30: Created. Link and general tags.
// 2015-09-01: Image tags.
// 2015-09-02: Tags for jumbotron background image and text color.
// 2015-11-06: Family tree tags.
// 2015-11-09: Added list of args to SecTags.

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
  val Flight = "flight"  // not sure which category this should be in

  val Birth = "birth"
  val Death = "death"
  val Event = "event"

  val Thought = "thought"
  val Todo    = "todo"
  val Started = "started"
  val Done    = "done"
  val Blocked = "blocked"

  val Demo = "demo"

  val ItemTagKinds = List(
      Link, Image, ImageResponsive,
      FamilyTree, Jumbotron, Marriage, Timeline, Flight)

  val EventTagKinds = List(Birth, Death, Event)
  val TaskTagKinds = List(Thought, Todo, Started, Done, Blocked)
  val OtherTagKinds = Demo +: (EventTagKinds ++ TaskTagKinds)

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
