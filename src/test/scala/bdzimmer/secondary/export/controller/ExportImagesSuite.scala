// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Tests for ExportImages. Trying to refactor and improve functionality of pixel art classes.

package bdzimmer.secondary.export.controller

import org.scalatest.FunSuite
import java.io.File
import javax.imageio.ImageIO

import bdzimmer.util.TempDirectory
import bdzimmer.util.StringUtils._

import bdzimmer.pixeleditor.model.TileOptions


class ExportImagesSuite extends FunSuite with TempDirectory {

  // directory with some sample images and map
  val resourceDir = getClass.getResource("/pixel").getPath

  test("save tileset to png") {

    val inputFilename = resourceDir / "amex.til"
    val outputFile = new File(tempDirname / "amex.png")

    val tileAttrs = TileOptions.types.get("Tiles").get  // I don't even care
    val image = RenderImages.getTilesetImage(inputFilename, tileAttrs)
    ImageIO.write(image, "png", outputFile)

    val loadedImage = ImageIO.read(outputFile)
    assert(loadedImage.getColorModel.getPixelSize == 8)
    assert(loadedImage.getWidth == 256 && loadedImage.getHeight == 256)

  }


  test("save map to png") {

    val inputFilename = resourceDir / "albion.map"
    val outputFile = new File(tempDirname / "albion.png")

    val image = RenderImages.getMapImage(inputFilename, resourceDir)
    ImageIO.write(image, "png", outputFile)

    val loadedImage = ImageIO.read(outputFile)
    assert(loadedImage.getColorModel.getPixelSize == 8)
    // TODO: add dimensions check

  }

}
