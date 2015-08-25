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

package bdzimmer.secondary.export

import scala.beans.BeanProperty
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.reflect.ClassTag
import org.yaml.snakeyaml.constructor.Constructor
import org.yaml.snakeyaml.TypeDescription
import org.yaml.snakeyaml.nodes.Tag


// bean version of world items -- for loading from

trait WorldItemBean {
  @BeanProperty var id: String = ""             // TODO: enforce provided
  @BeanProperty var name: String = ""           // TODO: enforce provided
  @BeanProperty var description: String = ""
  @BeanProperty var notes: String = ""
  @BeanProperty var srcyml: String = ""
  @BeanProperty var remoteid: String = ""

  // function to get immutable version
  def getVal(): WorldItem
}

// represents metadata for a piece of content
// that exists in its own file.
trait MetaItemBean extends WorldItemBean {
  @BeanProperty var filename: String = ""    // TODO: enforce provided
}

// a piece of content that is a tileset with attributes
trait TileMetaItemBean extends MetaItemBean {
  @BeanProperty var tiletype: String = ""    // TODO: enforce provided
}


class BareWorldItemBean extends WorldItemBean {
  def getVal(): BareWorldItem = BareWorldItem(id, name, description, notes, srcyml, remoteid)
}

class CollectionItemBean extends WorldItemBean {
   @BeanProperty var children: java.util.List[WorldItemBean] = new java.util.LinkedList[WorldItemBean]()

   def getVal(): CollectionItem = CollectionItem (
       id, name, description, notes, srcyml, remoteid,
       children.asScala.map(_.getVal).toList)
}

class ImageItemBean extends MetaItemBean {
  def getVal(): ImageItem = ImageItem(
      id, name, description, notes, srcyml, remoteid,
      filename)
}

class TilesetItemBean extends TileMetaItemBean {
  def getVal(): TilesetItem = TilesetItem(
      id, name, description, notes, srcyml, remoteid,
      filename, tiletype)
}

class SpritesheetItemBean extends TileMetaItemBean {
  def getVal(): SpritesheetItem = SpritesheetItem(
      id, name, description, notes, srcyml, remoteid,
      filename, tiletype)
}

class MapItemBean extends MetaItemBean {
  def getVal(): MapItem = MapItem(
      id, name, description, notes, srcyml, remoteid,
      filename)
}

class CharacterItemBean extends WorldItemBean {

  @BeanProperty var spritesheet: String = ""    // TODO: enforce provided
  @BeanProperty var sheetrow: String = ""       // TODO: enforce provided

  def getVal(): CharacterItem = CharacterItem(
      id, name, description, notes, srcyml, remoteid,
      spritesheet, sheetrow)

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
}


trait MetaItem extends WorldItem {
  val filename: String
}

trait TileMetaItem extends MetaItem {
  val tiletype: String
}


case class BareWorldItem(
    id: String, name: String, description: String, notes: String,
    srcyml: String, remoteid: String) extends WorldItem

case class CollectionItem(
    id: String, name: String, description: String, notes: String,
    srcyml: String, remoteid: String,
    children: List[WorldItem]) extends WorldItem

case class ImageItem(
    id: String, name: String, description: String, notes: String,
    srcyml: String, remoteid: String,
    filename: String) extends MetaItem

case class TilesetItem(
    id: String, name: String, description: String, notes: String,
    srcyml: String, remoteid: String,
    filename: String, tiletype: String) extends TileMetaItem

case class SpritesheetItem(
    id: String, name: String, description: String, notes: String,
    srcyml: String, remoteid: String,
    filename: String, tiletype: String) extends TileMetaItem

case class MapItem(
    id: String, name: String, description: String, notes: String,
    srcyml: String, remoteid: String,
    filename: String) extends MetaItem

case class CharacterItem(
    id: String, name: String, description: String, notes: String,
    srcyml: String, remoteid: String,
    spritesheet: String, sheetrow: String) extends WorldItem


////////////////////////////////////////////////////////////////////////////////////



// 2015-07-26 - WorldItem companion object

object WorldItem {


  // YAML constructor with descriptions for the various types

  val constructor = new Constructor(classOf[CollectionItemBean])
  constructor.addTypeDescription(new TypeDescription(classOf[BareWorldItemBean], new Tag("!item")))
  constructor.addTypeDescription(new TypeDescription(classOf[CollectionItemBean], new Tag("!collection")))
  constructor.addTypeDescription(new TypeDescription(classOf[ImageItemBean], new Tag("!image")))
  constructor.addTypeDescription(new TypeDescription(classOf[TilesetItemBean], new Tag("!tileset")))
  constructor.addTypeDescription(new TypeDescription(classOf[SpritesheetItemBean], new Tag("!spritesheet")))
  constructor.addTypeDescription(new TypeDescription(classOf[MapItemBean], new Tag("!map")))
  constructor.addTypeDescription(new TypeDescription(classOf[CharacterItemBean], new Tag("!character")))

  // TODO: filterList may not be necessary.

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



  // TODO: not sure if filterTree needs to remain

  /**
   * Filter a tree of WorldItems by type.
   *
   * @tparam A               type of WorldItem to keep
   * @param  worldItem       root WorldItem to filter
   * @return a list of the WorldItems of type A in the tree
   */
  def filterTree[A <: WorldItem : ClassTag](worldItem: WorldItem): List[A] = worldItem match {
    case x: CollectionItem => x.children.flatMap(x => filterTree[A](x))
    case x: A => List(x)
    case _ => Nil
  }


   // TODO: not sure if filterTreeForCollections needs to remain

   /**
   * Filter a tree of WorldItems, keeping Collections.
   *
   * @param  worldItem       root WorldItem to filter
   * @return a list of the CollectionItems in the tree
   *
   */
  def filterTreeForCollections(worldItem: WorldItem): List[CollectionItem] = worldItem match {
    case x: CollectionItem => x :: x.children.flatMap(x => filterTreeForCollections(x))
    case _ => Nil
  }


}



