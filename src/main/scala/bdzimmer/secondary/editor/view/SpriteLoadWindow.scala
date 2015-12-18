// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.editor.view

import java.awt.image.BufferedImage                   // scalastyle:ignore illegal.imports
import java.awt.event.{ActionEvent, ActionListener}   // scalastyle:ignore illegal.imports
import java.io.File
import java.util.ArrayList
import javax.swing.JButton

import bdzimmer.util.StringUtils._

import bdzimmer.secondary.editor.model.{DosGraphics, TileAttributes, TileOptions}
import bdzimmer.secondary.editor.controller.OldTilesetLoader



class SpriteLoadWindow(main: Main) extends LoadWidgetWindow(main, main.contentDir, "Load Sprites") {

  def populateObjects(inputDir: String): ArrayList[ImageWidget] = {

    val spriteItems = main.metadata.filter(_.assetType.equals("Spritesheet"))
    val widgets = spriteItems.map(x => {
      println(inputDir / x.filename)
      spritesWidget(inputDir / x.filename, x.name, x.info)
    })

    val widgetsArrayList = new java.util.ArrayList[ImageWidget]
    widgets.map(widgetsArrayList.add(_))
    widgetsArrayList
  }


  private def spritesWidget(spritesFilename: String, title: String, tiletype: String): ImageWidget = {

    val tilesFile = new File(spritesFilename)
    val tileAttrs = TileOptions.types.get(tiletype).getOrElse(TileOptions.Default)
    val tilesImage = new OldTilesetLoader(spritesFilename, tileAttrs).load().image(0)

    val subsetImage = new BufferedImage(
        ImageWidget.DefaultWidth, ImageWidget.DefaultHeight, BufferedImage.TYPE_INT_RGB)
    subsetImage.getGraphics.drawImage(tilesImage, 0, 0, null)

    val loader = new JButton("Edit")
    loader.addActionListener(new ActionListener() {
      def actionPerformed(event: ActionEvent): Unit = {
        main.createSpriteWindow(spritesFilename, tiletype)
      }
    })

    new ImageWidget(title, subsetImage, List(loader))

  }

}
