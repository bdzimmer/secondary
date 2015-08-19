// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary

import com.google.api.client.util.DateTime


package object export {


  // this stuff was used for the conversion from beans to
  // immutable case classes

  /*
  type WorldItem = WorldItemBean
  type MetaItem = MetaItemBean
  type TileMetaItem = TileMetaItemBean
  type BareWorldItem = BareWorldItemBean

  type CollectionItem = CollectionItemBean
  type TilesetItem = TilesetItemBean
  type SpritesheetItem = SpritesheetItemBean
  type MapItem = MapItemBean
  type CharacterItem = CharacterItemBean
  */

  /*
  type WorldItem = WorldItemVal
  type MetaItem = MetaItemVal
  type TileMetaItem = TileMetaItemVal
  type BareWorldItem = BareWorldItemVal

  type CollectionItem = CollectionItemVal
  type TilesetItem = TilesetItemVal
  type SpritesheetItem = SpritesheetItemVal
  type MapItem = MapItemVal
  type CharacterItem = CharacterItemVal
  */

  type FileModifiedMap = scala.collection.immutable.Map[String, (String, DateTime)]
  type FileOutputsMap = scala.collection.immutable.Map[String, List[String]]

}
