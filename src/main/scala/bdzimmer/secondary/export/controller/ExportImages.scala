// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.export.controller

import scala.util.Try

import java.awt.RenderingHints          // scalastyle:ignore illegal.imports
import java.awt.image.BufferedImage     // scalastyle:ignore illegal.imports
import java.io.File

import javax.imageio.ImageIO

import org.apache.commons.io.{FileUtils, FilenameUtils}

import bdzimmer.pixeleditor.model.{ContentStructure, DosGraphics, Map, TileAttributes, TileOptions}
import bdzimmer.pixeleditor.model.Color
import bdzimmer.pixeleditor.controller.OldTilesetLoader

import bdzimmer.secondary.export.model._
import bdzimmer.secondary.export.view.Markdown
import bdzimmer.secondary.export.view.Tags._

import bdzimmer.util.StringUtils._



class ExportImages(world: List[WorldItem], val location: String, license: String) {

  val imagesLocation = location / ExportImages.ImagesDir
  new File(imagesLocation).mkdir

  val metaItems = WorldItem.filterList[RefItem](world)


  def exportAllImages(items: List[WorldItem], contentDir: String): FileOutputsMap = {

    // export tileset / spritesheet and map images

    // for maps, tilesets, and spritesheets, filename is a datafile that gets converted
    // to images of various scales which are saved as standard image files. These
    // scaled standard image files become the outputs.

    val mapImageOutputs = (WorldItem.filterList[MapItem](items)
        map(x => (x.filename, ExportImages.exportImage(x, contentDir, location))))

    val tileImageOutputs = (WorldItem.filterList[TileRefItem](items)
        map(x => (x.filename, ExportImages.exportImage(x, contentDir, location))))

    // // export individual tile images
    // (filterList[TileMetaItem](items)
    //    map(x => Export.exportIndividualTileImages(x, contentDir, location)))

    // export individual tile images for sprite sheets
    // val spritesheetImageOutputs = (WorldItem.filterList[SpritesheetItem](items)
    //    map(x => (x.filename,  ExportImages.exportIndividualTileImages(x, contentDir, location))))

    // In the case of ImageItems, the filename is already an image. It either exists
    // in the contentDir (no prefix) or is to be downloaded (currently "wikimedia:"
    // prefix.
    val imageOutputs = (WorldItem.filterList[ImageItem](items).map(x => prepareImageItemOutputs(x, contentDir)))

    // we can't just call toMap on all of these lists and then merge them, because
    // the lists may contain duplicate keys themselves.

    // skip adding spritesheet outputs to list, since we don't want to upload them
    val allImageOutputsList = mapImageOutputs ++ tileImageOutputs ++ imageOutputs

    val allImageOutputs = (allImageOutputsList
        .map(x => List(x).toMap)
        .foldLeft(ExportImages.getEmptyFileOutputsMap)(ExportImages.mergeFileOutputsMaps(_, _)))

    allImageOutputs

  }


  // download or copy image files to the output location
  def prepareImageItemOutputs(imageItem: ImageItem, contentDir: String): (String, List[String]) = {

    val ext = FilenameUtils.getExtension(imageItem.filename)
    val relativeName = ExportImages.ImagesDir / imageItem.id + "." + ext
    val absoluteName = location / relativeName
    val dstFile = new File(absoluteName)

    val (resultSrc, resultDst) = imageItem.filename.startsWith("wikimedia:") match {

      case false => {
        // local file - copy to images folder
        // source is original local file

        val srcFilename = imageItem.filename
        val srcAbsFilename = contentDir / imageItem.filename
        val srcFile = new File(srcAbsFilename)

        if (!dstFile.exists && srcFile.exists) {
          println("\t\t\tcopying to " + relativeName)
          FileUtils.copyFile(srcFile, dstFile)
          (srcFilename, Some(relativeName))
        } else {
          (srcFilename, None)
        }
      }

      case true => {
        // wikimedia file - download to images folder
        // source is scrcyml!
        val srcFilename = imageItem.srcyml

        // only download if it doesn't already exist in the scratch location
        if (!dstFile.exists) {
          println("\t\t\tdownloading to " + relativeName)
          val outputName = for {
            json <- ImageDownloader.getWikimediaJson(imageItem.filename.split(":")(1))
            wm <- ImageDownloader.parseWikimediaJson(json)
            imagePrefix = imagesLocation / imageItem.id
            _ = ImageDownloader.downloadImage(wm, imagePrefix)
          } yield relativeName
          (srcFilename, outputName)
        } else {
          (srcFilename, None)
        }
      }
    }

    resultDst.map(filename => {
      // downsize image (in place) if necessary
      ImageDownloader.downsizeImage(location / filename, location / filename, ext)
    })

    (resultSrc, resultDst.toList)
  }

}



object ExportImages {

  // constants

  val ImagesDir = "images"
  val TransparentColor = Color(51 / 4, 153 / 4, 102 / 4)

  val outputScales = List(1, 4, 12)    // scalastyle:ignore magic.number

  def scalePostfix(scale: Int, ignore: Int = 1): String = scale match {
    case `ignore` => ""
    case _ => "_" + scale + "x"
  }


  /**
  * Export an image of a world item
  *
  * @param refItem  worldItem to export
  * @param inputDir   input directory to find filenames from
  * @param outputDir  output directory to save images to
  * @returns          names of files exported
  */
  def exportImage(refItem: RefItem, inputDir: String, outputDir: String): List[String] = {

    val inputName = refItem.filename

    val image: BufferedImage = refItem match {
      case x: MapItem => getMapImage(
          inputDir / inputName,
          inputDir / ContentStructure.TileDir + slash)
      case x: TileRefItem => {
        TileOptions.types.get(x.tiletype) match {
          case Some(tiletype) => getTilesetImage(inputDir / inputName, tiletype)
          case None => ExportImages.imageMessage("Invalid tile type.")
        }
      }
      case _ => ExportImages.imageMessage("MetaItem type not supported.")
    }

    // various scales to output
    outputScales map (scaleFactor => {
       val relativeName = ImagesDir / refItem.id + scalePostfix(scaleFactor) + ".png"
       val absoluteName = outputDir / relativeName
       ImageIO.write(rescaleImage(image, scaleFactor), "png", new File(absoluteName))
       relativeName
    })

  }


  // Export individual images of tiles in a tile set.
  def exportIndividualTileImages(tilesetItem: TileRefItem, inputDir: String, outputDir: String): List[String] = {

    val inputName = tilesetItem.filename

    TileOptions.types.get(tilesetItem.tiletype) match {

      case Some(tileType) => {

        val outputDirRelative = ImagesDir / tilesetItem.id + "_tiles"
        new File(outputDir / outputDirRelative).mkdir

        val image = getTilesetImage(inputDir / inputName, tileType)

        // extract buffered images from the tile image.
        val images = (0 until tileType.count).flatMap(curTile => {

          val xoff = (curTile % tileType.tilesPerRow) * tileType.width
          val yoff = (curTile / tileType.tilesPerRow) * tileType.height

          val curTileImage = image.getSubimage(xoff, yoff, tileType.width, tileType.height)

          // various scales to output
          outputScales map (scaleFactor => {
            val relativeName = outputDirRelative / curTile.toString + scalePostfix(scaleFactor) + ".png"
            val absoluteName = outputDir / relativeName
            ImageIO.write(rescaleImage(curTileImage, scaleFactor), "png", new File(absoluteName))
            relativeName
          })

        }).toList

        images

      }
      case None => List()
    }

  }


  // get an image of a tileset
  def getTilesetImage(
      inputFile: String,
      tileAttributes: TileAttributes,
      transparent: Color = TransparentColor,
      indexed: Boolean = true): BufferedImage = {

    val tileset = new OldTilesetLoader(inputFile, tileAttributes).load()
    val curPal = tileset.palettes(0)

    indexed match {
      case true  => tileset.image(0, transparent)
      case false => tileset.imageRGB(0, transparent)
    }

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

    val map = new Map(new File(inputFile))

    val tiles = new OldTilesetLoader(
        tilesDir / map.tileFileName + ".til",
        TileOptions.getOrQuit("Tiles")).load()

    val image = map.image(tiles, tiles.palettes(0))

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


  def pixelImagePathScale(item: WorldItem, scale: Int = 4): String = {
    val imageFile = ImagesDir / item.id + "%s.png"
    imageFile.format(scalePostfix(scale))
  }


  // generate HTML for an item's upscaled image with responsive style
  // that links to the bare image
  def pixelImageLinkResponsive(item: WorldItem): String = {
    link(image(pixelImagePathScale(item, 1), true), pixelImagePathScale(item))
  }


  def imageItemImagePath(imageItem: ImageItem): String = {
    ImagesDir / imageItem.id + "." + FilenameUtils.getExtension(imageItem.filename)
  }


  // generate HTML for a smaller image, with a link to the page
  // the attributes are kind of piling up on this function because of the many
  // different kinds of images and contexts where this is used.
  def imageLinkPage(
      item: WorldItem,
      responsive: Boolean = true,
      maxWidth: Int = 480,
      showName: Boolean = true,
      scale: Int = 4): String = {

    val imageTag = item match {
      case x: MapItem => {
        val imageFile = pixelImagePathScale(x, 1)
        imageSprite(imageFile, 0, 0, 192, 192)
      }
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
  def itemImagePath(item: WorldItem): String = item match {
    case x: MapItem => pixelImagePathScale(x, 4)
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

