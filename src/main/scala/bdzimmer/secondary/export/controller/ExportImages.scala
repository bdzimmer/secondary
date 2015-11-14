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
// 2015-08-26: Wikimedia image exporting.
// 2015-08-31: Enhanced character image functionality.
// 2015-09-03: Only download wikimedia images if they don't already exist in scratch directory.

package bdzimmer.secondary.export.controller

import scala.util.Try

import java.awt.RenderingHints          // scalastyle:ignore illegal.imports
import java.awt.image.BufferedImage     // scalastyle:ignore illegal.imports
import java.io.File

import javax.imageio.ImageIO

import org.apache.commons.io.{FileUtils, FilenameUtils}

import bdzimmer.secondary.editor.model.{ContentStructure, DosGraphics, Map, TileAttributes, TileOptions, Tiles}

import bdzimmer.secondary.export.model._
import bdzimmer.secondary.export.view.Markdown
import bdzimmer.secondary.export.view.Tags._



class ExportImages(world: List[WorldItem], val location: String, license: String) {

  val imagesLocation = location + File.separator + ExportImages.ImagesDir + File.separator
  new File(imagesLocation).mkdir

  val metaItems = WorldItem.filterList[MetaItem](world)


  def exportAllImages(items: List[WorldItem], contentDir: String): FileOutputsMap = {

    // export tileset / spritesheet and map images

    // for maps, tilesets, and spritesheets, filename is a datafile that gets converted
    // to images of various scales which are saved as standard image files. These
    // scaled standard image files become the outputs.

    // TODO: merge these maps.
    val mapImageOutputs = (WorldItem.filterList[MapItem](items)
        map(x => (x.filename, ExportImages.exportImage(x, contentDir, location))))

    val tileImageOutputs = (WorldItem.filterList[TileMetaItem](items)
        map(x => (x.filename, ExportImages.exportImage(x, contentDir, location))))

    // // export individual tile images
    // (filterList[TileMetaItem](items)
    //    map(x => Export.exportIndividualTileImages(x, contentDir, location)))

    // only export individual tile images for sprite sheets
    val spritesheetImageOutputs = (WorldItem.filterList[SpritesheetItem](items)
        map(x => (x.filename,  ExportImages.exportIndividualTileImages(x, contentDir, location))))

    // In the case of ImageItems, the filename is already an image. It either exists
    // in the contentDir (no prefix) or is to be downloaded (currently "wikimedia:"
    // prefix.
    val imageOutputs = (WorldItem.filterList[ImageItem](items).map(x => prepareImageItemOutputs(x)))

    val characterOutputs = (WorldItem.filterList[CharacterItem](items)
        map(x => prepareCharacterItemOutputs(x, contentDir)))

    // we can't just call toMap on all of these lists and then merge them, because
    // the lists may contain duplicate keys themselves.

    // skip adding spritesheet outputs to list, since we don't want to upload them

    val allImageOutputsList = (mapImageOutputs ++ tileImageOutputs ++ imageOutputs ++ characterOutputs)

    val allImageOutputs = (allImageOutputsList
        .map(x => List(x).toMap)
        .foldLeft(ExportImages.getEmptyFileOutputsMap)(ExportImages.mergeFileOutputsMaps(_, _)))

    allImageOutputs

  }


  // download or copy image files to the output location
  def prepareImageItemOutputs(imageItem: ImageItem): (String, List[String]) = {

    val relativeName = (ExportImages.ImagesDir + File.separator +
        imageItem.id + "." + FilenameUtils.getExtension(imageItem.filename))
    val absoluteName = location + File.separator + relativeName

    imageItem.filename.startsWith("wikimedia:") match {

      case false => {
        // local file - copy to images folder
        // source is original local file

        // TODO: test non-wikimedia images!

        val srcFilename = imageItem.filename
        val srcFile = new java.io.File(srcFilename)

        if (srcFile.exists) {
          FileUtils.copyFile(srcFile, new java.io.File(absoluteName))
          (srcFilename, List(relativeName))
        } else {
          (srcFilename, List())
        }
      }

      case true => {
        // wikimedia file - download to images folder
        // source is scrcyml!
        val srcFilename = imageItem.srcyml

        // only download if it doesn't already exist in the scratch location
        if (new java.io.File(absoluteName).exists) {
          (srcFilename, List())
        } else {

          println("\t\t\tdownloading " + relativeName)

          val outputName = for {
            json <- ImageDownloader.getWikimediaJson(imageItem.filename.split(":")(1))
            wm <- ImageDownloader.parseWikimediaJson(json)
            junk = ImageDownloader.downloadImage(wm, imagesLocation + imageItem.id)
          } yield relativeName
          (srcFilename, outputName.toList)
        }

      }
    }

  }



  def prepareCharacterItemOutputs(ci: CharacterItem, contentDir: String): (String, List[String]) = {

    val (cm, sheetRow) = ExportImages.getCharacterImageInfo(ci, metaItems)

    val outputPairs = cm match {

      case Some(ss: SpritesheetItem) => {

        // TODO: use TileOptions.tiles.get here and deal with the option
        val spritesheetType = TileOptions.getOrQuit(ss.tiletype)

        val outputImageFiles = ExportImages.outputScales map (scale => {
          val offset = 3
          val inputFile = (imagesLocation + File.separator
              + ss.id + "_tiles" + File.separator
              + (sheetRow * spritesheetType.tilesPerRow + offset) + ExportImages.scalePostfix(scale) + ".png")
          val relativeName = ExportImages.ImagesDir + File.separator + ci.id + ExportImages.scalePostfix(scale) + ".png"
          val absoluteName = location + File.separator + relativeName
          FileUtils.copyFile(new File(inputFile), new File(absoluteName))

          relativeName

        })

        (ss.filename, outputImageFiles)

      }

      case _ => ("", List())

    }

    outputPairs

  }

}




object ExportImages {

  // constants

  val ImagesDir = "images"
  val Slash = "/"

  // val TransparentColor: (Int, Int, Int) = (255, 255, 255)
  val TransparentColor: (Int, Int, Int) = (51, 153, 102)

  val outputScales = List(1, 4, 12)    // scalastyle:ignore magic.number

  def scalePostfix(scale: Int, ignore: Int = 1): String = scale match {
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
  def exportImage(worldItem: MetaItem, inputDir: String, outputDir: String): List[String] = {

    val inputName = worldItem.filename

    val image: BufferedImage = worldItem match {
      case x: MapItem => getMapImage(
          inputDir + File.separator + inputName,
          inputDir + File.separator + ContentStructure.TileDir + File.separator)
      case x: TileMetaItem => {
        TileOptions.types.get(x.tiletype) match {
          case Some(tiletype) => getTilesetImage(inputDir + File.separator + inputName, tiletype)
          case None => ExportImages.imageMessage("Invalid tile type.")
        }
      }
      case _ => ExportImages.imageMessage("MetaItem type not supported.")
    }

    // various scales to output
    outputScales map (scaleFactor => {
       val relativeName = ImagesDir + File.separator + worldItem.id + scalePostfix(scaleFactor) + ".png"
       val absoluteName = outputDir + File.separator + relativeName
       ImageIO.write(rescaleImage(image, scaleFactor), "png", new File(absoluteName))
       relativeName
    })

  }



  // Export individual images of tiles in a tile set.
  def exportIndividualTileImages(tilesetItem: TileMetaItem, inputDir: String, outputDir: String): List[String] = {

    val inputName = tilesetItem.filename

    TileOptions.types.get(tilesetItem.tiletype) match {

      case Some(tileType) => {

        val outputDirRelative = ImagesDir + File.separator + tilesetItem.id + "_tiles"
        new File(outputDir + File.separator + outputDirRelative).mkdir

        val image = getTilesetImage(inputDir + File.separator + inputName, tileType)

        // extract buffered images from the tile image.
        val images = (0 until tileType.count).flatMap(curTile => {

          val xoff = (curTile % tileType.tilesPerRow) * tileType.width
          val yoff = (curTile / tileType.tilesPerRow) * tileType.height

          val curTileImage = image.getSubimage(xoff, yoff, tileType.width, tileType.height)

          // various scales to output
          outputScales map (scaleFactor => {
            val relativeName = outputDirRelative + File.separator + curTile + scalePostfix(scaleFactor) + ".png"
            val absoluteName = outputDir + File.separator + relativeName
            ImageIO.write(rescaleImage(curTileImage, scaleFactor), "png", new File(absoluteName))
            relativeName
          })

        }).toList

        images

      }
      case None => List()
    }

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
                      transparentColor: (Int, Int, Int) = TransparentColor): BufferedImage = {

    val dg = new DosGraphics()
    val tiles = new Tiles(tileAttributes, new File(inputFile), dg.getRgbPalette)
    dg.updateClut()

    val tc: Int =  255 << 24 | transparentColor._1 << 16 |  transparentColor._2 << 8 | transparentColor._3

    // check if the transparent color exists in the tileset.
    if ((0 to 255).map(i => dg.getPalette()(i) == tc).contains(true)) {
      throw new Exception("Transparent color collision!")
    }

    dg.getPalette()(255) = tc   // scalastyle:ignore magic.number

    val image = tiles.getTilesImage(dg.getPalette())

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

    val map = new Map(new File(inputFile))

    // TODO: add a hard-coded TileAttributes to Map for Tiles
    val tiles = new Tiles(
        TileOptions.getOrQuit("Tiles"),
        new File(tilesDir + File.separator + map.tileFileName + ".til"),
        dg.getRgbPalette)

    dg.updateClut()

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


  // create an image with an error message
  def imageMessage(message: String): BufferedImage = {
    val result = new BufferedImage(256, 256, BufferedImage.TYPE_INT_RGB)
    result.getGraphics.drawString(message, 0, 20)
    result
  }

   ///


  def getCharacterImageInfo(ci: CharacterItem, metaItems: List[MetaItem]): (Option[MetaItem], Int) = {

    // split spritesheet attribute by comma
    // first part is item id, second part spritesheet row (if exists)
    val spriteSplit = ci.image.split(",\\s+")
    val (metaId, sheetRow) = spriteSplit.toList match {
      case x :: xs => {
        (x, xs.headOption.flatMap(s => Try(s.toInt).toOption).getOrElse(0))
      }
      case Nil => ("", 0)
    }

    // there may not be a matching MetaItem in the collection
    // if it doesn't exist yet
    val metaOption = metaItems.filter(_.id.equals(metaId)).headOption
    (metaOption, sheetRow)

  }

  // produce HTML for the image of a character
  def characterImage(
      ci: CharacterItem, metaItems: List[MetaItem],
      scale: Int = 4,
      responsive: Boolean = true,
      maxWidth: Int = 480): String = {

    val metaOption = getCharacterImageInfo(ci, metaItems)._1
    val path = characterItemImagePath(ci, metaItems, scale)

    metaOption.map(meta => meta match {
      case ss: SpritesheetItem => image(path)
      case im: ImageItem => image(path, responsive, maxWidth)
      case _ => ""
    }).getOrElse("")

  }


  def characterItemImagePath(
      ci: CharacterItem,
      metaItems: List[MetaItem],
      scale: Int = 4): String = {

    val (metaOption, sheetRow) = getCharacterImageInfo(ci, metaItems)

    metaOption.map(meta => meta match {
      case ss: SpritesheetItem => pixelImagePathScale(ci, scale)
      case im: ImageItem => imageItemImagePath(im)
      case _ => ""
    }).getOrElse("")

  }


  def pixelImagePathScale(item: WorldItem, scale: Int = 4): String = {
    val imageFile = ImagesDir + Slash + item.id + "%s.png"
    imageFile.format(scalePostfix(scale))
  }


  // generate HTML for an item's upscaled image with responsive style
  // that links to the bare image
  def pixelImageLinkResponsive(item: WorldItem): String = {
    link(image(pixelImagePathScale(item), true), pixelImagePathScale(item))
  }


  def imageItemImagePath(imageItem: ImageItem): String = {
    ImagesDir + Slash + imageItem.id + "." + FilenameUtils.getExtension(imageItem.filename)
  }


  // generate HTML for a smaller image, with a link to the page
  // the attributes are kind of piling up on this function because of the many
  // different kinds of images and contexts where this is used.
  def imageLinkPage(
      item: WorldItem,
      metaItems: List[MetaItem],
      responsive: Boolean = true,
      maxWidth: Int = 480,
      showName: Boolean = true,
      scale: Int = 4): String = {

    val imageTag = item match {
      case x: MapItem => {
        val imageFile = pixelImagePathScale(x, 1)
        imageSprite(imageFile, 0, 0, 192, 192)
      }
      case x: CharacterItem => characterImage(x, metaItems, scale, responsive, maxWidth)
      case x: ImageItem => image(imageItemImagePath(x), responsive, maxWidth)
      case _ => ""
    }

    val imageName = showName match {
      case true => (if (!responsive) br else "" ) + Markdown.processLine(item.name)
      case false => ""
    }

    link(imageTag + imageName, item.id + ".html")
  }



  // can this be combined with the above somehow?
  // I believe this is only used for jumbotron images.
  def itemImagePath(
      item: WorldItem,
      metaItems: List[MetaItem]): String = item match {

    case x: MapItem => pixelImagePathScale(x, 4)
    case x: CharacterItem => characterItemImagePath(x, metaItems)
    case x: ImageItem => imageItemImagePath(x)
    case _ => ""
  }



  /// /// ///


  // functions for working with FileOutputsMaps

  def getEmptyFileOutputsMap(): FileOutputsMap = {
    List.empty[(String, List[String])].toMap
  }

  // combine two dictionaries of (String -> List[String], merging the values of
  // corresponding keys
  def mergeFileOutputsMaps(map1: FileOutputsMap, map2: FileOutputsMap): FileOutputsMap = {
    // TODO: add a distinct here?
    map1 ++ map2.map{case (k, v) => k -> (v ++ map1.getOrElse(k, Nil))}
  }


}

