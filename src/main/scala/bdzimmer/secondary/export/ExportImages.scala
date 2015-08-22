// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Sketching out some ideas for out to implement project structure and metadata using YAML.
// Ben Zimmer

// 2015-01-12

// 2015-01-26: Tileset and map image export working.
// 2015-01-31: Page export!

// 2015-04-02: Various updates. Map page export. Changed export filenames to use ids rather than deriving
//             from filename.

// 2015-04-20: Todo list generation. Renamed to "Export". Collection page export. Also changing Export
//             to a class so some commonly used data can be held. Not sure about this, but we'll see how it works.
//             Added additional methods for HTML to Bootstrap class to try to eliminate HTML code
//             from Export class.

// 2015-06-08: Refactored slightly; added export all pages and images methods to Export class.

// 2015-07-07: Images are exported into an images directory. Images for characters are prepared from spritesheets.

// 2015-07-14: Started making functions that create images and pages return the relative paths of the things
//             that they create.
// 2015-07-15: More on above.
// 2015-07-25: Past weeks intelligent image downloading. Intelligent metadata upload / download.
// 2015-07-26: Refactoring. Improved item image links (shows up in collection pages).
// 2015-08-08: Generic WorldItem page creation. Simple image link implementation for WorldItems.
//             Todo list improvements, including "thoughts".

package bdzimmer.secondary.export

import java.awt.RenderingHints
import java.awt.image.BufferedImage
import java.io.File

import org.apache.commons.io.FileUtils

import bdzimmer.secondary.model.{DosGraphics, Map, TileAttributes, TileOptionsNew, Tiles}

import javax.imageio.ImageIO


class ExportImages(val location: String, license: String) {

  val imagesLocation = location + "/" + ExportImages.imagesDir + "/"
  new File(imagesLocation).mkdir



  def exportAllImages(items: List[WorldItem], contentDir: String): FileOutputsMap = {

    // export tileset / spritesheet and map images

    val mapImageOutputs = (WorldItem.filterList[MapItem](items)
        map(x => (x.filename, ExportImages.exportImage(x, contentDir, location))))


    val tileImageOutputs = (WorldItem.filterList[TileMetaItem](items)
        map(x => (x.filename, ExportImages.exportImage(x, contentDir, location))))


    // export individual tile images
    // (filterList[TileMetaItem](items)
    //    map(x => Export.exportIndividualTileImages(x, contentDir, location)))

    // only export individual tile images for sprite sheets
    val spritesheetImageOutputs = (WorldItem.filterList[SpritesheetItem](items)
        map(x =>(x.filename,  ExportImages.exportIndividualTileImages(x, contentDir, location))))


    // get list of spritesheets
    val spritesheets = WorldItem.filterList[SpritesheetItem](items)

    // skip adding spritesheet outputs to list, since we don't want to upload them
    val allImageOutputsList = List(mapImageOutputs.toMap, tileImageOutputs.toMap)

    val allImageOutputs = allImageOutputsList.reduce(ExportPages.mergeFileOutputsMaps(_, _))

    allImageOutputs

  }



  def prepareCharacterImages(characterItems: List[CharacterItem], spritesheets: List[SpritesheetItem], contentDir: String): FileOutputsMap = {

    // prepare character images

    val characterImageOutputs = characterItems.map(ci => {

      // there may not be a matching spritesheet in the collection
      // if it doesn't exist yet.

      val curSpritesheet = spritesheets.filter(_.id.equals(ci.spritesheet)).take(1)

      val outputImageFiles = curSpritesheet flatMap (cs => {
      // for (cs <- curSpritesheet) {

        val spritesheetType = TileOptionsNew.get(cs.tiletype)

        // TODO: clean this up
        ExportImages.outputScales map (scale => {
          val offset = 3
          val inputFile = imagesLocation + "/" + ci.spritesheet + "_tiles/" + (ci.sheetrow.toInt * spritesheetType.tilesPerRow + offset) + ExportImages.scalePostfix(scale) + ".png"

          val relativeName = ExportImages.imagesDir + "/" + ci.id + ExportImages.scalePostfix(scale) + ".png"
          val absoluteName = location + "/" + relativeName
          FileUtils.copyFile(new File(inputFile), new File(absoluteName))

          relativeName

        })

      })

      val spritesheetFile = curSpritesheet.headOption match {
        case Some(x) => x.filename
        case None => ""
      }

      println("prepared images: " + spritesheetFile + " " + outputImageFiles.mkString(", "))

      List((spritesheetFile, outputImageFiles)).toMap

    }).reduce(ExportPages.mergeFileOutputsMaps(_, _))

    characterImageOutputs

  }




}






object ExportImages {


  val imagesDir = "images"

  val transparentColor: (Int, Int, Int) = (255, 255, 255)
  // val transparentColor: (Int, Int, Int) = (51, 153, 102)

  val outputScales = List(1, 4, 12)    // scalastyle:ignore magic.number

  def scalePostfix(scale: Int, ignore: Int = 1): String =  scale match {
    case `ignore` => ""
    case _ => "_" + scale + "x"
  }


  /**
  * Export an image of a world item
  *
  * @param worldItem  worldItem to export
  * @param inputDir   input directory to find filenames from
  * @param outputDir  output directory to save images to
  * @returns          names of files exported
  */
  def exportImage(worldItem: WorldItem with MetaItem, inputDir: String, outputDir: String): List[String] = {

    val inputName = worldItem.filename

    val image: BufferedImage = worldItem match {
      case x: MapItem => getMapImage(inputDir + "/" + inputName, inputDir + "/tile/")
      case x: TileMetaItem => {

        TileOptionsNew.types.get(x.tiletype) match {
          case Some(tiletype) => getTilesetImage(inputDir + "/" + inputName, tiletype)
          case None => {
            val result = new BufferedImage(256, 256, BufferedImage.TYPE_INT_RGB)
            result.getGraphics.drawString("Invalid tile type.", 0, 20)
            result
          }
        }


      }
    }

    // various scales to output
    outputScales map (scaleFactor => {
       val relativeName = ExportImages.imagesDir + "/" + worldItem.id + scalePostfix(scaleFactor) + ".png"
       val absoluteName = outputDir + "/" + relativeName
       ImageIO.write(rescaleImage(image, scaleFactor), "png", new File(absoluteName))
       relativeName
    })

  }



  /**
   *
   * Export individual images of tiles in a tile set.
   *
   */
  def exportIndividualTileImages(tilesetItem: TileMetaItem, inputDir: String, outputDir: String): List[String] = {

    // println(tilesetItem.name)

    val inputName = tilesetItem.filename
    val tileType = TileOptionsNew.types.get(tilesetItem.tiletype).get

    val outputDirRelative = ExportImages.imagesDir + "/" + tilesetItem.id + "_tiles"
    new File(outputDir + "/" + outputDirRelative).mkdir

    val image = getTilesetImage(inputDir + "/" + inputName, tileType)

    // extract buffered images from the tile image.
    val images = (0 until tileType.count).flatMap(curTile => {

      val xoff = (curTile % tileType.tilesPerRow) * tileType.width
      val yoff = (curTile / tileType.tilesPerRow) * tileType.height

      val curTileImage = image.getSubimage(xoff, yoff, tileType.width, tileType.height)


      // val outputName = outputNewDir + "/" + curTile + ".png"
      // ImageIO.write(curTileImage, "png", new File(outputName))

      // various scales to output
      outputScales map (scaleFactor => {
        val relativeName = outputDirRelative + "/" + curTile + scalePostfix(scaleFactor) + ".png"
        val absoluteName = outputDir + "/" + relativeName
        ImageIO.write(rescaleImage(curTileImage, scaleFactor), "png", new File(absoluteName))
        relativeName
      })


    }).toList

    images
  }



  /**
   * Get a BufferedImage representation of a tiles file.
   *
   * @param inputFile       name of file to load
   * @param tileAttributes  attributes of file to load
   * @return 1x scaled BufferedImage of the data in the file
   *
   */
  def getTilesetImage(inputFile: String,
                      tileAttributes: TileAttributes,
                      transparentColor: (Int, Int, Int) = ExportImages.transparentColor): BufferedImage = {

    val dg = new DosGraphics()

    val tiles = new Tiles(tileAttributes)
    tiles.load(new File(inputFile), dg)

    val tc: Int =  255 << 24 | transparentColor._1 << 16 |  transparentColor._2 << 8 | transparentColor._3

    // check if the transparent color exists in the tileset.
    if ((0 to 255).map(i => dg.getPalette()(i) == tc).contains(true)) {
      throw new Exception("Transparent color collision!")
    }


    dg.getPalette()(255) = tc   // scalastyle:ignore magic.number

    // TODO: Better logic for determining spritesheet image attributes
    val image = tiles.getTilesImage(
        tileAttributes.tilesPerRow,
        math.ceil(tileAttributes.count.toFloat / tileAttributes.tilesPerRow).toInt, dg.getPalette())

    image

  }



  /**
   * Get a BufferedImage representation of a map file.
   *
   * @param inputFile       name of file to load
   * @param tilesDir        directory of tile files
   * @return 1x scaled BufferedImage of the data in the file
   *
   */
  def getMapImage(inputFile: String, tilesDir: String): BufferedImage = {

    val dg = new DosGraphics()

    val map = new Map
    map.load(new File(inputFile))

    val tiles = new Tiles(TileOptionsNew.get("Tiles"))
    tiles.load(new File(tilesDir + "/" + map.tileFileName + ".til"), dg)

    val image = map.getMapImage(tiles, dg)

    image

  }



  /**
   * Rescale a BufferedImage using nearest neighbor interpolation.
   *
   * @param image        image to rescale
   * @param scaleFactor  integer scale factor
   * @return scaled BufferedImage
   */
  def rescaleImage(image: BufferedImage, scaleFactor: Int): BufferedImage = {

    val width = image.getWidth() * scaleFactor
    val height = image.getHeight() * scaleFactor

    val result = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val g2d = result.createGraphics()
    g2d.addRenderingHints(new RenderingHints(RenderingHints.KEY_INTERPOLATION,
        RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR))
    g2d.drawImage(image, 0, 0, width, height, null)

    result

  }


}
