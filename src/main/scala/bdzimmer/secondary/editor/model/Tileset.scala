// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Sketching out a new tileset class.
// Trying to separate data from saving / loading operations.

// 2015-12-13: Created.


package bdzimmer.secondary.editor.model

import java.awt.image.BufferedImage;
import java.io.{File, FileInputStream, FileOutputStream}
import bdzimmer.secondary.editor.model.{QbInputStream, QbOutputStream}


case class TileProperties(value: Int) // for now
case class Tile(pixels: Array[Array[Int]])
case class Color(val r: Int, val g: Int, val b: Int)
case class Palette(start: Int, end: Int, colors: Array[Color])


class Tileset (
    val tiles: Array[Tile],
    val properties: Array[TileProperties],
    val palettes: List[Palette],
    val tilesPerRow: Int) {

  // Need to find the best way to protect this constructor.

  // We want to be able to assume that the tiles are all the same size
  // and probably that the palettes are also the same size.

  val width  = tiles(0).pixels.size
  val height = tiles(0).pixels(0).size

  def image(): BufferedImage = ???

}


// a loader stores info about where the tileset came from and
// how it should be saved

trait TilesetLoader {
  // TODO: return Result
  def load(): Tileset            // load a tileset from disk
  def save(t: Tileset): Unit     // save a tileset to disk
}



class OldTilesetLoader(val filename: String, val attrs: TileAttributes) extends TilesetLoader {

  // copied code from the Java Tiles class in here and refactored
  def load(): Tileset = {

    // probably need to put all of this in a Try
    val is = new QbInputStream(new FileInputStream(filename))

    // load the tiles
    val tiles = (0 until attrs.count).map(x => OldTilesetLoader.loadTile(is, attrs.width, attrs.height)).toArray

    // load the palette
    val palette = Palette(
        attrs.palStart,
        attrs.palEnd,
        (attrs.palStart to attrs.palEnd).map(x => OldTilesetLoader.loadColor(is)).toArray)

    // load tile properties
    val properties = if (this.attrs.tileProperties) {
      (0 until attrs.count).map(x => OldTilesetLoader.loadProperties(is)).toArray
    } else {
      Array[TileProperties]()
    }

    is.close()

    new Tileset(tiles, properties, List(palette), attrs.tilesPerRow)

  }

  def save(t: Tileset): Unit = ???

}


object OldTilesetLoader {

  // load an m x n tile
  def loadTile(is: QbInputStream, width: Int, height: Int): Tile = {

    val pixels = new Array[Array[Int]](height)

    for (y <- 0 until height) {
      val row = new Array[Int](width)
      for (x <- 0 until width) {
        row(x) = is.readQbUnsignedByte()
      }
      pixels(y) = row
    }

    Tile(pixels)
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

}
