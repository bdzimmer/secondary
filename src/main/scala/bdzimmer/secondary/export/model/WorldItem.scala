// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Beans for export.

// Ben Zimmer

// 2015-06-08: Refactored from Export.scala.
// 2015-07-11: Added more dynamic functionality for loading a master collection
//             from YAML files.
// 2015-07-20: Adding element to WorldItem that refers back to YML file.
// 2015-08-16: Added immutable versions for processing. Mutable versions are loaded then
//             converted to immutable.
// 2015-08-22: Changed null fields to default to empty string. Need to implement validation
//             on loaded world to make sure required fields are provided.
// 2015-08-24: Added ImageItemBean / ImageItem classes.
// 2015-09-08: Added tag list field to item classes.
// 2015-09-21: Manually implemented getters / setters to avoid null strings for missing
//             attributes.

package bdzimmer.secondary.export.model

import scala.beans.BeanProperty
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.reflect.ClassTag

import bdzimmer.secondary.export.controller.{ExtractRawTags => pst}
import bdzimmer.pixeleditor.model.AssetMetadata


// convert null strings to empty
object NonNullString {
  def apply(s: String): String = if (s == null) "" else s
}


// bean version of world items -- for loading from YAML

object WorldItemBeans {

  import WorldItems._

  trait WorldItemBean {

    var id: String = ""
    def getId(): String = id
    def setId(id: String): Unit = {this.id = NonNullString(id)}

    var name: String = ""
    def getName(): String = name
    def setName(name: String): Unit = {this.name = NonNullString(name)}

    var description: String = ""
    def getDescription(): String = description
    def setDescription(description: String): Unit = {
      this.description = NonNullString(description)
    }

    var notes: String = ""
    def getNotes(): String = notes
    def setNotes(notes: String): Unit = {this.notes = NonNullString(notes)}

    // used by new parser
    var path: String = ""
    def getPath(): String = path
    def setPath(path: String): Unit = {this.path = NonNullString(path)}


    var srcfilename: String = ""
    var remoteid: String = ""

    // function to get immutable version
    def getVal(): WorldItem
  }


  // the item generates or references an image
  trait ImageItemBean extends WorldItemBean

  // a reference to a piece of content that exists in its own file.
  trait RefItemBean extends WorldItemBean {
    @BeanProperty var filename: String = ""
  }

  // the referenced piece of content is a tileset with attributes
  trait TileRefItemBean extends ImageItemBean with RefItemBean {
    @BeanProperty var tiletype: String = ""
  }

  class CollectionItemBean extends WorldItemBean {
     @BeanProperty var children: java.util.List[WorldItemBean] = new java.util.LinkedList[WorldItemBean]()

     def getVal(): CollectionItem = CollectionItem (
         id, name, description, notes,
         srcfilename, remoteid,
         pst.getAllTags(notes),
         children.asScala.map(_.getVal).toList)
  }

  class ThingItemBean extends WorldItemBean {
    def getVal(): ThingItem = ThingItem(
        id, name, description, notes,
        srcfilename, remoteid, pst.getAllTags(notes))
  }

  class PlaceItemBean extends WorldItemBean {
    def getVal(): PlaceItem = PlaceItem(
        id, name, description, notes,
        srcfilename, remoteid, pst.getAllTags(notes))
  }

  class ImageFileItemBean extends ImageItemBean with RefItemBean {
    def getVal(): ImageFileItem = ImageFileItem(
        id, name, description, notes,
        srcfilename, remoteid,
        pst.getAllTags(notes),
        filename)
  }

  class TripItemBean extends ImageItemBean {
    def getVal(): TripItem = TripItem(
        id, name, description, notes,
        srcfilename, remoteid,
        pst.getAllTags(notes))
  }

  class CharacterItemBean extends WorldItemBean {

    def getVal(): CharacterItem = {

      val (cleanedName, nameParts) = WorldItems.cleanName(name)

      CharacterItem(
        id, cleanedName, nameParts,
        description, notes,
        srcfilename, remoteid,
        pst.getAllTags(notes))
    }

  }

  ///

  class TilesetItemBean extends TileRefItemBean {
    def getVal(): TilesetItem = TilesetItem(
        id, name, description, notes,
        srcfilename, remoteid,
        pst.getAllTags(notes),
        filename, tiletype)
  }

  class SpritesheetItemBean extends TileRefItemBean {
    def getVal(): SpritesheetItem = SpritesheetItem(
        id, name, description, notes,
        srcfilename, remoteid,
        pst.getAllTags(notes),
        filename, tiletype)
  }

  class MapItemBean extends ImageItemBean with RefItemBean {
    def getVal(): MapItem = MapItem(
        id, name, description, notes,
        srcfilename, remoteid,
        pst.getAllTags(notes),
        filename)
  }


  // reference to another source file
  // as far as I know, the getVal function here will never be called.
  class SrcIncludeBean extends RefItemBean {
    def getVal(): ThingItem = ThingItem(
        id, name, description, notes,
        srcfilename, remoteid, pst.getAllTags(notes))
  }


  class BookItemBean extends  WorldItemBean {
    @BeanProperty var uniqueIdentifier: String = ""
    @BeanProperty var authorname: String = ""

    def getVal(): BookItem = BookItem(
      id, name, description, notes,
      srcfilename, remoteid, pst.getAllTags(notes),
      uniqueIdentifier, authorname)
  }

}


// immutable versions ///////////////////////////////////////////////////////////////////


object WorldItems {

  import Tags.RawTag

  // all inheritance done with traits; nothing descends from case classes

  trait WorldItem {
      val id: String
      val name: String
      val description: String
      val notes: String
      val srcfilename: String
      val remoteid: String
      val tags: Map[Int, RawTag]
  }


  trait ImageItem extends WorldItem

  trait RefItem extends WorldItem {
    val filename: String
  }

  trait TileRefItem extends ImageItem with RefItem {
    val tiletype: String
  }

  case class CollectionItem(
      id: String, name: String, description: String, notes: String,
      srcfilename: String, remoteid: String, tags: Map[Int, RawTag],
      val children: List[WorldItem]) extends WorldItem

  case class ThingItem(
      id: String, name: String, description: String, notes: String,
      srcfilename: String, remoteid: String, tags: Map[Int, RawTag]) extends WorldItem

  // for now, PlaceItem and TripItem have no fields that distinguish them from ThingItem
  case class PlaceItem(
      id: String, name: String, description: String, notes: String,
      srcfilename: String, remoteid: String, tags: Map[Int, RawTag]) extends WorldItem

  case class ImageFileItem(
      id: String, name: String, description: String, notes: String,
      srcfilename: String, remoteid: String, tags: Map[Int, RawTag],
      filename: String) extends ImageItem with RefItem

  case class TripItem(
      id: String, name: String, description: String, notes: String,
      srcfilename: String, remoteid: String, tags: Map[Int, RawTag]) extends ImageItem

  case class CharacterItem(
      id: String, name: String, nameParts: Option[List[String]],
      description: String, notes: String,
      srcfilename: String, remoteid: String, tags: Map[Int, RawTag]) extends WorldItem

  /// items for pixel art content ///

  case class TilesetItem(
      id: String, name: String, description: String, notes: String,
      srcfilename: String, remoteid: String, tags: Map[Int, RawTag],
      filename: String, tiletype: String) extends ImageItem with TileRefItem

  case class SpritesheetItem(
      id: String, name: String, description: String, notes: String,
      srcfilename: String, remoteid: String, tags: Map[Int, RawTag],
      filename: String, tiletype: String) extends ImageItem with TileRefItem

  case class MapItem(
      id: String, name: String, description: String, notes: String,
      srcfilename: String, remoteid: String, tags: Map[Int, RawTag],
      filename: String) extends ImageItem with RefItem

  /// book items

  case class BookItem(
     id: String, name: String, description: String, notes: String,
     srcfilename: String, remoteid: String, tags: Map[Int, RawTag],
     uniqueIdentifier: String, authorname: String) extends WorldItem


  // create a list of all world items in a hierarchy
  def collectionToList(worldItem: WorldItem): List[WorldItem] = worldItem match {
    case x: CollectionItem => x :: x.children.flatMap(x => collectionToList(x))
    case _                 => List(worldItem)
  }


  // convert the world to a list of AssetMetadata items for use by the editor
  def assetMetadata(worldItem: WorldItem): List[AssetMetadata] = {
    collectionToList(worldItem).map(item => item match {
      case x: MapItem         => Some(AssetMetadata(x.id, "Map",         x.name, x.filename, "unused"))
      case x: TilesetItem     => Some(AssetMetadata(x.id, "Tileset",     x.name, x.filename, x.tiletype))
      case x: SpritesheetItem => Some(AssetMetadata(x.id, "Spritesheet", x.name, x.filename, x.tiletype))
      case _                  => None
    }).flatten
  }


  def cleanName(x: String): (String, Option[List[String]]) = {
    val split = x.split("\\|").map(_.trim)
    val cleaned = split.mkString(" ")
    val parts = if (split.length > 1) Some(split.toList) else None
    (cleaned, parts)
  }

}
