// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.export.model

import bdzimmer.orbits.{CalendarDateTime, AnimationSettings}


object SecTags {

  // Tags are organized below as they are in the documentation.

  // General Tags

  val Anchor = "anchor"
  val Footnotes = "footnotes"
  val Index = "index"
  val Link = "link"
  val Sidenote = "sidenote"
  val Snip = "snip"
  val Quote = "quote"

  // Image Tags

  val Gallery = "gallery"
  val Image = "image"
  val Jumbotron = "jumbotron"

  // Event and Genealogy Tags

  val Birth = "birth"
  val Death = "death"
  val Event = "event"
  val FamilyTree = "familytree"
  val Father = "father"
  val Mother = "mother"
  val Parent = "parent"
  val Son = "son"
  val Daughter = "daughter"
  val Child = "child"
  val Ancestor = "ancestor"
  val Descendant = "descendant"
  val Marriage = "marriage"
  val Timeline = "timeline"

  // Work Tracking Tags

  val BurnDown = "burndown"
  val Tasks = "tasks"
  val Thought = "thought"
  val Todo = "todo"
  val Started = "started"
  val Done = "done"
  val Blocked = "blocked"
  val Stats = "stats"
  val WordCount = "wordcount"

  // Flight Planning Tags

  val Acceleration = "acceleration"
  val Mass = "mass"
  val Velocity = "velocity"
  val Flight = "flight"
  val FlightAnimation = "flightanimation"
  val FlightEpoch = "flightepoch"

  // Miscellaneous Tags

  val Demo = "demo"
  val Conditional = "conditional"
  val Config = "config"
  val MarkovText = "markovtext"

}


object Tags {

  import WorldItems._

  // new tag parsing - 2016-08-07 - 2016-08-14
  // these can be parsed from raw tags (RawTag) as soon as mapping of
  // name to WorldItem is created.
  // the idea is that this codifies the tag types and behaviors
  // in the compiler and should be more self-documenting then the original
  // implementation

  case class RawTag(kind: String, value: String, args: List[String])

  trait ParsedTag

  case class Link(
    item: WorldItem,
    displayText: Option[String],
    anchor: Option[String]
  ) extends ParsedTag

  case class Image(
    item: ImageItem,
    responsive: Boolean,
    link: Boolean
  ) extends ParsedTag

  case class FamilyTree(
    root: CharacterItem
  ) extends ParsedTag

  case class Jumbotron(
    item: ImageItem,
    xPos: String,
    yPos: String,
    color: String
  ) extends ParsedTag

  case class Timeline(
    root: WorldItem,
    format: String
  ) extends ParsedTag

  // events

  trait EventTag extends ParsedTag {
    val date: String // TODO: real date type
    val desc: String
  }

  case class Birth(
    date: String, // TODO: real date type
    desc: String
  ) extends EventTag

  case class Death(
    date: String, // TODO: real date type
    desc: String
  ) extends EventTag

  case class Event(
    date: String, // TODO: real date type
    desc: String
  ) extends EventTag

  // space flight related tags

  case class Flight(
    ship: WorldItem,
    startLocation: String,
    endLocation: String,
    startDate: CalendarDateTime,
    endDate: CalendarDateTime,
    passengers: List[WorldItem],
    faction: String
  ) extends ParsedTag

  case class SpacecraftProperty(
    kind: String,
    value: Double,
    unit: String  // TODO: real unit type(s)
  ) extends ParsedTag

  case class FlightEpoch(
    name: String,
    startDate: CalendarDateTime,
    endDate: CalendarDateTime
  ) extends ParsedTag

  // obviously WIP
  case class FlightAnimation(
    item: WorldItem,
    epoch: String,
    settings: AnimationSettings,
    status: Int,
    style: String,
    visible: List[String]
    // TODO: other visualization options
    // examples include faction filters
  ) extends ParsedTag

  // tasks

  case class Task(
    kind: String,
    desc: String,
    log: Option[String],
    start: Option[String],
    done: Option[String],
    points: Int
  ) extends ParsedTag


  // genealogy tags

  trait GenealogyTag extends ParsedTag {
    val character: CharacterItem
  }

  case class Ancestor(
    kind: String,
    character: CharacterItem
  ) extends GenealogyTag

  case class Descendant(
    kind: String,
    character: CharacterItem
  ) extends GenealogyTag

  case class Marriage(
    character: CharacterItem,
    date: Option[String] // TODO: real date type
  ) extends GenealogyTag


  //  miscellaneous tags

  case class Demo(
    kind: String,
    body: String
  ) extends ParsedTag

  case class MarkovText(
    items: List[WorldItem],
    externalWorks: List[String],
    order: Int,
    count: Int,
    seed: Int
  ) extends ParsedTag

  case class WordCount(
    item: WorldItem,
    recursive: Boolean,
    sections: Boolean
  ) extends ParsedTag

  case class BurnDown(
    item: WorldItem,
    startDate: Option[CalendarDateTime],
    endDate: Option[CalendarDateTime],
    recursive: Boolean,
    weekends: Boolean
  ) extends ParsedTag

  case class Anchor(
    desc: String,
    id: String
  ) extends ParsedTag

  case class Index(
    item: WorldItem
  ) extends ParsedTag

  case class Tasks(
    item: WorldItem,
    shorthand: Boolean,
    recursive: Boolean,
    mode: String
  ) extends ParsedTag

  case class Stats(
    item: WorldItem
  ) extends ParsedTag

  case class Gallery(
    item: WorldItem,
    size: Integer,
    showCaptions: Boolean,
    recursive: Boolean
  ) extends ParsedTag

  case class Sidenote(
    desc: String,
    id: String
  ) extends ParsedTag

  case class Footnotes(
    item: WorldItem,
    sections: Boolean
  ) extends ParsedTag

  case class Snip(
    id: String,
    paragraphs: Int
  ) extends ParsedTag

  case class Quote(
    item: WorldItem,
    id: String
  ) extends ParsedTag

  case class Config(
      desc: String,
      args: Map[String, String]
  ) extends ParsedTag

  case class Conditional(
      text: String,
      modes: Set[String]
  ) extends ParsedTag

  // error tags

  trait ErrorTag extends ParsedTag

  case class GenError(msg: String) extends ErrorTag
  case class ParseError(tag: RawTag, msg: String) extends ErrorTag

  // establishes rules for how tags reference other items
  // used to determine related item outputs that need to be regenerated as items change

  def items(tag: ParsedTag): List[WorldItem] = tag match {
    case x: Link       => List(x.item)
    case x: Image      => List(x.item)
    case x: FamilyTree => List(x.root) // TODO: recursion through ancestor / descendant relationships
    case x: Jumbotron  => List(x.item)
    case x: Timeline   => List(x.root)
    case x: Flight     => x.ship :: x.passengers
    case x: Ancestor   => List(x.character)
    case x: Descendant => List(x.character)
    case x: Marriage   => List(x.character)
    case x: Quote      => List(x.item)
    // MarkovText would go here, but I don't want those to automatically update.

    // just a reminder to self that this by itself establishes the proper
    // recursive dependency, since hashes of items change when the hashes of their
    // children change
    case x: WordCount  => List(x.item)
    case x: Index      => List(x.item)
    case x: Tasks      => List(x.item)
    case x: Stats      => List(x.item)
    case _ => List()
  }

}

