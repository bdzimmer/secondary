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

import org.yaml.snakeyaml.constructor.Constructor
import org.yaml.snakeyaml.TypeDescription
import org.yaml.snakeyaml.nodes.Tag

import bdzimmer.secondary.export.model.{ParseSecTags => pst}
import bdzimmer.pixeleditor.model.AssetMetadata

// convert null strings to empty
object NonNullString {
  def apply(s: String): String = s match {
    case null => ""
    case _ => s
  }
}


// bean version of world items -- for loading from YAML

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


  var srcyml: String = ""
  var remoteid: String = ""

  // function to get immutable version
  def getVal(): WorldItem
}

// represents metadata for a piece of content
// that exists in its own file.
trait MetaItemBean extends WorldItemBean {
  @BeanProperty var filename: String = ""
}

// a piece of content that is a tileset with attributes
trait TileMetaItemBean extends MetaItemBean {
  @BeanProperty var tiletype: String = ""
}

class BareWorldItemBean extends WorldItemBean {
  def getVal(): BareWorldItem = BareWorldItem(
      id, name, description, notes,
      srcyml, remoteid, pst.getAllTags(notes))
}


class CollectionItemBean extends WorldItemBean {
   @BeanProperty var children: java.util.List[WorldItemBean] = new java.util.LinkedList[WorldItemBean]()

   def getVal(): CollectionItem = CollectionItem (
       id, name, description, notes,
       srcyml, remoteid,
       pst.getAllTags(notes),
       children.asScala.map(_.getVal).toList)
}

// reference to another YML file
// as far as I know, the getVal function here will never be called.
class YamlIncludeBean extends MetaItemBean {
  def getVal(): BareWorldItem = BareWorldItem(
      id, name, description, notes,
      srcyml, remoteid, pst.getAllTags(notes))
}

class ImageItemBean extends MetaItemBean {
  def getVal(): ImageItem = ImageItem(
      id, name, description, notes,
      srcyml, remoteid,
      pst.getAllTags(notes),
      filename)
}

class TilesetItemBean extends TileMetaItemBean {
  def getVal(): TilesetItem = TilesetItem(
      id, name, description, notes,
      srcyml, remoteid,
      pst.getAllTags(notes),
      filename, tiletype)
}

class SpritesheetItemBean extends TileMetaItemBean {
  def getVal(): SpritesheetItem = SpritesheetItem(
      id, name, description, notes,
      srcyml, remoteid,
      pst.getAllTags(notes),
      filename, tiletype)
}

class MapItemBean extends MetaItemBean {
  def getVal(): MapItem = MapItem(
      id, name, description, notes,
      srcyml, remoteid,
      pst.getAllTags(notes),
      filename)
}

class CharacterItemBean extends WorldItemBean {

  // @BeanProperty var image: String = ""

  def getVal(): CharacterItem = CharacterItem(
      id, name, description, notes,
      srcyml, remoteid,
      pst.getAllTags(notes)) // image

}


// immutable versions ///////////////////////////////////////////////////////////////////

// all inheritance done with traits; nothing descends from case classes

trait WorldItem {
    val id: String
    val name: String
    val description: String
    val notes: String
    val srcyml: String
    val remoteid: String
    val tags: List[SecTag]
}


trait MetaItem extends WorldItem {
  val filename: String
}

trait TileMetaItem extends MetaItem {
  val tiletype: String
}


case class BareWorldItem(
    id: String, name: String, description: String, notes: String,
    srcyml: String, remoteid: String, tags: List[SecTag]) extends WorldItem

case class CollectionItem(
    id: String, name: String, description: String, notes: String,
    srcyml: String, remoteid: String, tags: List[SecTag],
    children: List[WorldItem]) extends WorldItem

case class ImageItem(
    id: String, name: String, description: String, notes: String,
    srcyml: String, remoteid: String, tags: List[SecTag],
    filename: String) extends MetaItem

case class TilesetItem(
    id: String, name: String, description: String, notes: String,
    srcyml: String, remoteid: String, tags: List[SecTag],
    filename: String, tiletype: String) extends TileMetaItem

case class SpritesheetItem(
    id: String, name: String, description: String, notes: String,
    srcyml: String, remoteid: String, tags: List[SecTag],
    filename: String, tiletype: String) extends TileMetaItem

case class MapItem(
    id: String, name: String, description: String, notes: String,
    srcyml: String, remoteid: String, tags: List[SecTag],
    filename: String) extends MetaItem

case class CharacterItem(
    id: String, name: String, description: String, notes: String,
    srcyml: String, remoteid: String, tags: List[SecTag]) extends WorldItem


////////////////////////////////////////////////////////////////////////////////////


object WorldItem {

  // YAML constructor with descriptions for the various types

  val constructor = new Constructor(classOf[CollectionItemBean])
  constructor.addTypeDescription(new TypeDescription(classOf[BareWorldItemBean], new Tag("!item")))
  constructor.addTypeDescription(new TypeDescription(classOf[CollectionItemBean], new Tag("!collection")))
  constructor.addTypeDescription(new TypeDescription(classOf[YamlIncludeBean], new Tag("!include")))
  constructor.addTypeDescription(new TypeDescription(classOf[ImageItemBean], new Tag("!image")))
  constructor.addTypeDescription(new TypeDescription(classOf[TilesetItemBean], new Tag("!tileset")))
  constructor.addTypeDescription(new TypeDescription(classOf[SpritesheetItemBean], new Tag("!spritesheet")))
  constructor.addTypeDescription(new TypeDescription(classOf[MapItemBean], new Tag("!map")))
  constructor.addTypeDescription(new TypeDescription(classOf[CharacterItemBean], new Tag("!character")))
  constructor.addTypeDescription(new TypeDescription(classOf[CharacterItemBean], new Tag("!person")))

  /**
   * Filter a list of WorldItems by type.
   *
   * @tparam A               type of WorldItem to keep
   * @param  items           list of  WorldItem to filter
   * @return a list of the WorldItems of type A in the list
   *
   */
  def filterList[A <: WorldItem : ClassTag](items: List[WorldItem]): List[A] = items.headOption match {
    case Some(x: A) => x :: filterList[A](items.tail)
    case Some(_) => filterList[A](items.tail)
    case None => Nil
  }


  // create a list of all world items in a hierarchy
  def collectionToList(worldItem: WorldItem): List[WorldItem] = worldItem match {
    case x: CollectionItem => x :: x.children.flatMap(x => collectionToList(x))
    case _ => List(worldItem)
  }


  // convert the world to a list of AssetMetadata items for use by the editor
  def assetMetadata(worldItem: WorldItem): List[AssetMetadata] = {
    collectionToList(worldItem).map(item => item match {
      case x: MapItem => Some(AssetMetadata(x.id, "Map", x.name, x.filename, "unused"))
      case x: TilesetItem => Some(AssetMetadata(x.id, "Tileset", x.name, x.filename, x.tiletype))
      case x: SpritesheetItem => Some(AssetMetadata(x.id, "Spritesheet", x.name, x.filename, x.tiletype))
      case _ => None
    }).flatten
  }


}
