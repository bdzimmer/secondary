// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Sketching out a new tileset class.
// Trying to separate data from saving / loading operations.

// 2015-12-13: Created.
// 2015-12-14: Bringing in image generation.


package bdzimmer.secondary.editor.model

import java.awt.image.{BufferedImage, IndexColorModel}     // scalastyle:ignore illegal.imports
import java.io.{File, FileInputStream, FileOutputStream}

case class TileProperties(value: Int)    // for now

case class Tile(pixels: Array[Array[Int]])

// color with red, green, and blue elements that range 0-63
case class Color(val r: Int, val g: Int, val b: Int) {
  def toInt(): Int = 255 << 24 | (r * 4) << 16 | (g * 4) << 8 | (b * 4)
}

case class Palette(start: Int, end: Int, colors: Array[Color]) {

  def colorModel(transparent: Color): IndexColorModel = {

    val r = new Array[Byte](256)
    val g = new Array[Byte](256)
    val b = new Array[Byte](256)

    for (i <- 0 until colors.length) {
      r(i + start) = ((colors(i).r * 4) & 0xFF).toByte
      g(i + start) = ((colors(i).g * 4) & 0xFF).toByte
      b(i + start) = ((colors(i).b * 4) & 0xFF).toByte
    }

    r(255) = ((transparent.r * 4) & 0xFF).toByte
    g(255) = ((transparent.g * 4) & 0xFF).toByte
    b(255) = ((transparent.b * 4) & 0xFF).toByte

    // weird things happen when you try to set a transparent index (extra argument)
    // it seems that it will always be index 0 in a png, but also strange palette
    // shifts happen if it is set to 256. Seems best to not set this for now.
    new IndexColorModel(8, 256, r, g, b);

  }

}


class Tileset (
    val tiles: Array[Tile],
    val properties: Array[TileProperties],
    val palettes: List[Palette],
    val tilesPerRow: Int) {

  // Need to find the best way to protect this constructor.

  // We want to be able to assume that the tiles are all the same size
  // and probably that the palettes are also the same size.

  val height  = tiles(0).pixels.size
  val width = tiles(0).pixels(0).size


  def imageRGB(paletteIndex: Int, transparent: Color = Tileset.Transparent): BufferedImage = {
    imageRGB(tilesPerRow, math.ceil(tiles.length.toFloat / tilesPerRow).toInt,
        paletteIndex, transparent)
  }


  def image(paletteIndex: Int, transparent: Color = Tileset.Transparent): BufferedImage = {
    image(tilesPerRow, math.ceil(tiles.length.toFloat / tilesPerRow).toInt,
        paletteIndex, transparent)
  }


  // get a 24-bit image of the tileset
  def imageRGB(
      tilesWide: Int,
      tilesHigh: Int,
      paletteIndex: Int,
      transparentColor: Color): BufferedImage = {

    val curPal = palettes(paletteIndex)
    val fullPal = (0 to 255).map(x => Color(0, 0, 0)).toArray
    for (x <- curPal.start to curPal.end) {
      fullPal(x) = curPal.colors(x - curPal.start)
    }
    fullPal(255) = transparentColor

    val tilesImage = new BufferedImage(
        tilesWide * width, tilesHigh * height, BufferedImage.TYPE_INT_RGB)

    for (whichTile <- 0 until tiles.length) {
      val xoff = (whichTile % tilesWide) * width
      val yoff = (whichTile / tilesWide) * height

      for (y <- 0 until height) {
        for (x <- 0 until width) {
          val color = fullPal(tiles(whichTile).pixels(y)(x))
          tilesImage.setRGB(xoff + x, yoff + y, color.toInt)
        }
      }
    }

    tilesImage
  }

  // get a 256-color indexed image of the tileset
  def image(
      tilesWide: Int,
      tilesHigh: Int,
      paletteIndex: Int,
      transparentColor: Color): BufferedImage = {

    val curPal = palettes(paletteIndex)


    val tilesImage = Tileset.indexedImage(
        tilesWide * width, tilesHigh * height,
        curPal, transparentColor)

    val wr = tilesImage.getRaster

    for (whichTile <- 0 until tiles.length) {
      val xoff = (whichTile % tilesWide) * width
      val yoff = (whichTile / tilesWide) * height
      for (y <- 0 until height) {
        wr.setPixels(xoff, yoff + y, width, 1, tiles(whichTile).pixels(y))
      }
    }

    tilesImage
  }

}


object Tileset {

  val Transparent = Color(50, 0, 50)

  // create an empty (zeroed) tile of the specified width and height
  def emptyTile(width: Int, height: Int): Tile = {
    val pixels = new Array[Array[Int]](height)
    for (y <- 0 until height) {
      pixels(y) = new Array[Int](width)
    }
    Tile(pixels)
  }

  // create an indexed BufferedImage with a palette
  def indexedImage(
      width: Int, height: Int,
      palette: Palette,
      transparent: Color): BufferedImage = {

    val cm = palette.colorModel(transparent)
    new BufferedImage(width, height, BufferedImage.TYPE_BYTE_INDEXED, cm)

  }

  // untested temporary functions for converting to and from the old Tiles class,
  // with palette modification side effects. This will allow the next step of eliminating
  // the tile saving and loading code from the old Tiles class.

  // TODO: ELIMINATE save and load functions in the old Tiles class.

  // convert to old Tiles class; modify palette array
  def toTiles(tileset: Tileset, fulPal: Array[Array[Int]]): Tiles = {

    val pal = tileset.palettes(0)

    // modify palette
    (pal.start to pal.end).foreach(i => {
      val color = pal.colors(i - pal.start)
      fulPal(i)(0) = color.r
      fulPal(i)(1) = color.g
      fulPal(i)(2) = color.b
    })

    val attrs = new TileAttributes(
        tileset.height, tileset.width, tileset.tiles.length,
        pal.start, pal.end,
        tileset.properties.length > 0, tileset.tilesPerRow)

    new Tiles(tileset.tiles.map(_.pixels), tileset.properties.map(_.value), attrs)
  }

  // convert from old Tiles class and palette array
  def fromTiles(tiles: Tiles, fulPal: Array[Array[Int]]): Tileset = {

    val pal = new Palette(
        tiles.attrs.palStart, tiles.attrs.palEnd,
        (tiles.attrs.palStart to tiles.attrs.palEnd).map(i => {
          new Color(fulPal(i)(0), fulPal(i)(1), fulPal(i)(2))
        }).toArray)

    new Tileset(
        tiles.tiles.map(Tile(_)),
        tiles.tileProps.map(TileProperties(_)),
        List(pal),
        tiles.attrs.tilesPerRow)
  }

}

