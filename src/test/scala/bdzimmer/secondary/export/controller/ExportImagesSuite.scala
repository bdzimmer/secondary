// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Tests for ExportImages. Trying to refactor and improve functionality of pixel art classes.

package bdzimmer.secondary.export.controller

import org.scalatest.FunSuite
import java.io.File
import javax.imageio.ImageIO

import bdzimmer.util.StringUtils._

import bdzimmer.secondary.editor.model.TileOptions

class ExportImagesSuite extends FunSuite {

    val resourceDir = getClass.getResource("/pixel").getPath

    // test new indexed png export
    test("save tileset to png") {

      val inputFilename = resourceDir / "amex.til"
      val outputFile = new File("amex.png")

      val tileAttrs = TileOptions.types.get("Tiles").get  // I don't even care
      val image = ExportImages.getTilesetImageIndexed(inputFilename, tileAttrs)

      ImageIO.write(image, "png", outputFile)
      val loadedImage = ImageIO.read(outputFile)
      assert(loadedImage.getColorModel.getPixelSize == 8)

    }

}
