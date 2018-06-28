// Copyright (c) 2018 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.export.controller

import java.awt.RenderingHints          // scalastyle:ignore illegal.imports
import java.awt.image.BufferedImage     // scalastyle:ignore illegal.imports
import java.io.File

import javax.imageio.ImageIO

import org.apache.commons.io.{FileUtils, FilenameUtils}

import bdzimmer.pixeleditor.model.{ContentStructure, TileAttributes, TileOptions}
import bdzimmer.pixeleditor.model.Color
import bdzimmer.pixeleditor.controller.OldTilesetLoader

import bdzimmer.secondary.export.model.Tags
import bdzimmer.secondary.export.model.WorldItems._
import bdzimmer.secondary.export.view.Markdown
import bdzimmer.secondary.export.view.Html._

import bdzimmer.orbits.{RenderFlight, MeeusPlanets, ConstAccelCraft, ConstVelCraft, ConstVelFlightFn}

import bdzimmer.util.StringUtils._



class RenderImages(
    world: List[WorldItem],
    stringToTags: Map[String, Map[Int, Tags.ParsedTag]],
    wikiCache: FilesystemCache,
    val location: String,
    license: String) {

  val imagesLocation = location / RenderImages.ImagesDir
  new File(imagesLocation).mkdir

  val np = new RenderTags(stringToTags, world.collect({case x: CharacterItem => x}))


  def exportAllImages(items: List[WorldItem], contentDir: String): FileOutputsMap = {

    // export tileset / spritesheet and map images

    // for maps, tilesets, and spritesheets, filename is a datafile that gets converted
    // to images of various scales which are saved as standard image files, probably png.
    // These scaled standard image files become the outputs.

    val mapImageOutputs = (items.collect({case x: MapItem => x})
        map(x => (x.filename, RenderImages.exportImage(x, contentDir, location))))

    val tileImageOutputs = (items.collect({case x: TileRefItem => x})
        map(x => (x.filename, RenderImages.exportImage(x, contentDir, location))))

    // In the case of ImageItems, the filename is already an image. It either exists
    // in the contentDir (no prefix) or is to be downloaded (currently "wikimedia:"
    // prefix.
    val imageFileOutputs = (items.collect({case x: ImageFileItem => x})
        map(x => prepareImageFileItemOutputs(x, contentDir)))

    // TripItems generate images of interplanetary flights from tags in their notes.
    val tripOutputs = (items.collect({case x: TripItem => x})
        map(x => prepareTripItemOutputs(x, contentDir)))

    // we can't just call toMap on all of these lists and then merge them, because
    // the lists may contain duplicate keys themselves.

    val allImageOutputsList = mapImageOutputs ++ tileImageOutputs ++ imageFileOutputs ++ tripOutputs

    val allImageOutputs = allImageOutputsList
        .map(x => List(x).toMap)
        .foldLeft(RenderImages.emptyFileOutputsMap)(RenderImages.mergeFileOutputsMaps(_, _))

    allImageOutputs

  }


  // download or copy image files to the output location
  def prepareImageFileItemOutputs(imageFileItem: ImageFileItem, contentDir: String): (String, List[String]) = {

    // where does the image ultimately have to end up?
    val ext = FilenameUtils.getExtension(imageFileItem.filename)
    val dstRelativeName = RenderImages.ImagesDir / imageFileItem.id + "." + ext
    val dstAbsoluteName = location / dstRelativeName
    val dstFile = new File(dstAbsoluteName)

    // source is original file in content directory
    val srcFilename = imageFileItem.filename

    // resultSrc is relative
    // resultToCopyAbs is absolute and will be None if the file already exists in the output
    // location or something goes wrong in the downloading

    val (resultSrc, resultToCopyAbs) = if (!imageFileItem.filename.startsWith("wikimedia:")) {

      // local file - copy to output web image directory

      val srcAbsFilename = contentDir / imageFileItem.filename
      val srcFile = new File(srcAbsFilename)

      if (!dstFile.exists && srcFile.exists) {
        (srcFilename, Some(srcAbsFilename))
      } else {
        (srcFilename, None)
      }

    } else {

      // wikimedia file - download to cache folder if not already there,
      // then copy to images folder.

      val wikiName = imageFileItem.filename.split(":")(1)
      val cachedFilename = wikiCache.dir / wikiName
      val cachedFile = new File(cachedFilename)

      // only download if it doesn't already exist in the cache location
      if (!cachedFile.exists) {
        val wikiMeta = for {
          json     <- wikiCache(wikiName)
          wm       <- ImageDownloader.parseWikimediaJson(json)
        } yield wm
        wikiMeta.foreach(x => ImageDownloader.downloadImage(x, cachedFilename))
      }

      if (!dstFile.exists && cachedFile.exists) {
        (srcFilename, Some(cachedFilename))
      } else {
        (srcFilename, None)
      }

    }

    val resultDst = resultToCopyAbs.map(x => {
      // copy and downsize image if necessary
      ImageDownloader.downsizeImage(x, dstAbsoluteName, ext)
      dstRelativeName
    })

    (resultSrc, resultDst.toList)
  }


  def prepareTripItemOutputs(tripItem: TripItem, contentDir: String): (String, List[String]) = {

    val src = tripItem.srcfilename

    // val flightParams = stringToTags.getOrElse(tripItem.id, Map()).values.collect({
    //   case x: Tags.Flight => Flight.flightParams(x, stringToTags)
    // }).toList

    // sort the flight tags by the order they appear in the item
    val flightTags = stringToTags.getOrElse(tripItem.id, Map()).toList.sortBy(x => x._1).map(_._2).collect({
      case x: Tags.Flight => x
    })

    val dst = for {
      // fp <- flightParams.headOption
      tag <- flightTags
      fp = Flight.flightParams(tag, stringToTags)

      startLoc <- MeeusPlanets.Planets.get(fp.startLocation)
      endLoc <- MeeusPlanets.Planets.get(fp.endLocation)

    } yield {

      val ship = if (fp.vel > ConstVelFlightFn.VelMin) {
        ConstVelCraft(fp.ship.name, fp.vel)
      } else {
        ConstAccelCraft(fp.ship.name, fp.mass, fp.accel)
      }

      val im = RenderFlight.drawFlight(
          ship,
          fp.faction,
          fp.startLocation,
          fp.endLocation,
          startLoc, endLoc,
          fp.startDate,
          fp.endDate)

      // val relativeName = RenderImages.ImagesDir / tripItem.id + ".png"
      val relativeName = RenderImages.tagImagePath(tag)
      val outputImage = new java.io.File(location / relativeName)
      ImageIO.write(im, "png", outputImage)
      relativeName
    }

    // copy the first image to the default name
    dst.headOption.foreach(x => {
      val relativeName = RenderImages.ImagesDir / tripItem.id + ".png"
      val outputImage = new java.io.File(location / relativeName)
      FileUtils.copyFile(new File(location / x), outputImage)
    })

    (src, dst)
  }
}



object RenderImages {

  // constants

  val ImagesDir = "images"
  val TransparentColor = Color(51 / 4, 153 / 4, 102 / 4)

  val outputScales = List(1, 4, 12)    // scalastyle:ignore magic.number

  def scalePostfix(scale: Int, ignore: Int = 1): String = scale match {
    case `ignore` => ""
    case _ => "_" + scale + "x"
  }


  // Export an image of a world item
  def exportImage(refItem: RefItem, inputDir: String, outputDir: String): List[String] = {

    val inputName = refItem.filename

    val image: BufferedImage = refItem match {
      case x: MapItem => getMapImage(
        inputDir / inputName,
        inputDir / ContentStructure.TileDir + slash)
      case x: TileRefItem => TileOptions.types.get(x.tiletype) match {
        case Some(tiletype) => getTilesetImage(inputDir / inputName, tiletype)
        case None => imageMessage("Invalid tile type.")
      }
      case _ => imageMessage("MetaItem type not supported.")
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
    // val curPal = tileset.palettes(0)

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

    val map = new bdzimmer.pixeleditor.model.Map(new File(inputFile))

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


  def imagePath(item: ImageItem): String = {
    val ext = item match {
      case x: ImageFileItem => FilenameUtils.getExtension(x.filename)
      case _ => "png"
    }
    ImagesDir / item.id + "." + ext
  }


  def tagImagePath(tag: Tags.ParsedTag): String = {
    tag match {
      case _: Tags.Flight => ImagesDir / "flight_" + tag.hashCode.toHexString + ".png"
      case _ => ""
    }
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
      case x: ImageFileItem => image(imagePath(x), responsive, maxWidth)
      case x: TripItem => image(imagePath(x), responsive, maxWidth)
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
    case x: ImageFileItem => imagePath(x)
    case x: TripItem => imagePath(x)
    case _ => ""
  }


  /// /// ///

  // functions for working with FileOutputsMaps

  def emptyFileOutputsMap: FileOutputsMap = {
    List.empty[(String, List[String])].toMap
  }

  // combine two dictionaries of (String -> List[String], merging the values of
  // corresponding keys
  def mergeFileOutputsMaps(map1: FileOutputsMap, map2: FileOutputsMap): FileOutputsMap = {
    // TODO: add a distinct here?
    map1 ++ map2.map{case (k, v) => k -> (v ++ map1.getOrElse(k, Nil))}
  }


}

