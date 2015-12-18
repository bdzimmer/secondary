// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// A TilesetLoader stores info that allows it to load and save a Tileset.

// For now, only includes implementation for the old tileset format.

package bdzimmer.secondary.editor.controller

import java.io.{FileInputStream, FileOutputStream}

import bdzimmer.secondary.editor.model.{TileAttributes, Tileset, Tile, TileProperties, Palette, Color}
import bdzimmer.secondary.editor.model.{QbInputStream, QbOutputStream}



// TODO: return Result to indicate success or failure

trait TilesetLoader {
  def load(): Tileset            // load a tileset from disk
  def save(t: Tileset): Unit     // save a tileset to disk
}



class OldTilesetLoader(val filename: String, attrs: TileAttributes) extends TilesetLoader {

  // copied code from the Java Tiles class in here and refactored
  def load(): Tileset = {

    // probably need to put all of this in a Try
    val is = new QbInputStream(new FileInputStream(filename))

    // load the tiles
    val tiles = (0 until attrs.count).map(x => OldTilesetLoader.loadTile(is, attrs.width, attrs.height)).toArray

    // load the palette
    val palette = Palette(
        attrs.palStart, attrs.palEnd,
        (attrs.palStart to attrs.palEnd).map(x => OldTilesetLoader.loadColor(is)).toArray)

    // load tile properties
    val properties = if (attrs.tileProperties) {
      (0 until attrs.count).map(x => OldTilesetLoader.loadProperties(is)).toArray
    } else {
      Array[TileProperties]()
    }

    is.close()

    new Tileset(tiles, properties, List(palette), attrs.tilesPerRow)
  }


  def save(t: Tileset): Unit = {
    val os = new QbOutputStream(new FileOutputStream(filename))

    t.tiles.foreach(x => OldTilesetLoader.saveTile(os, x))
    t.palettes(0).colors.foreach(x => OldTilesetLoader.saveColor(os, x))
    t.properties.foreach(x => OldTilesetLoader.saveProperties(os, x))

    os.close()
  }

}



object OldTilesetLoader {

  // load an m x n tile
  def loadTile(is: QbInputStream, width: Int, height: Int): Tile = {
    val tile = Tileset.emptyTile(width, height)
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        tile.pixels(y)(x) = is.readQbUnsignedByte()
      }
    }
    tile
  }


  // load an rgb triple
  def loadColor(is: QbInputStream): Color = {
    val red =   is.readQbUnsignedShortLow()
    val green = is.readQbUnsignedShortLow()
    val blue =  is.readQbUnsignedShortLow()
    Color(red, green, blue)
  }


  // load properties for a single tile
  def loadProperties(is: QbInputStream): TileProperties = {
    TileProperties(is.readQbUnsignedShortLow())
  }


  /////////////////////////////

  // save an m x n tile
  def saveTile(os: QbOutputStream, tile: Tile): Unit = {
    for (row <- tile.pixels) {
      for (pixel <- row) {
        os.writeQbUnsignedByte(pixel)
      }
    }
  }


   // save an rgb triple
  def saveColor(os: QbOutputStream, color: Color): Unit = {
    os.writeQbUnsignedShortLow(color.r)
    os.writeQbUnsignedShortLow(color.g)
    os.writeQbUnsignedShortLow(color.b)
  }


  // load properties for a single tile
  def saveProperties(os: QbOutputStream, properties: TileProperties): Unit = {
    os.writeQbUnsignedShortLow(properties.value)
  }

  /////////////////


  def fromAttributes(attrs: TileAttributes): Tileset = {

    // empty tiles
    val tiles = (0 until attrs.count).map(x => Tileset.emptyTile(attrs.width, attrs.height)).toArray

    // empty palette
    val palette = Palette(
        attrs.palStart, attrs.palEnd,
        (attrs.palStart to attrs.palEnd).map(x => Color(0, 0, 0)).toArray)

    // empty tile properties
    val properties = if (attrs.tileProperties) {
      (0 until attrs.count).map(x => TileProperties(0)).toArray
    } else {
      Array[TileProperties]()
    }

    new Tileset(tiles, properties, List(palette), attrs.tilesPerRow)

  }


}
