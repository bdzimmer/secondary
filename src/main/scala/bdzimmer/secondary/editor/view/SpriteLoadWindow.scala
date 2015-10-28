// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.editor.view

import java.awt.image.BufferedImage                   // scalastyle:ignore illegal.imports
import java.awt.event.{ActionEvent, ActionListener}   // scalastyle:ignore illegal.imports
import java.io.File
import java.util.ArrayList

import bdzimmer.secondary.export.model.{CollectionItem, SpritesheetItem, WorldItem}
import bdzimmer.secondary.export.controller.WorldLoader
import bdzimmer.secondary.editor.model.{DosGraphics, TileAttributes, TileOptions, Tiles}

import javax.swing.JButton


class SpriteLoadWindow(main: Main) extends WorldObjectWindow(main, main.contentDir, "Load Sprites") {

  def populateObjects(inputDir: String): ArrayList[WorldObject] = {

    val spriteItems = WorldItem.filterList[SpritesheetItem](WorldLoader.collectionToList(main.master))
    val widgets = spriteItems.map(x => {
      println(inputDir + File.separator + x.filename)
      spritesWidget(inputDir + File.separator + x.filename, x.name, x.tiletype)
    })

    val widgetsArrayList = new java.util.ArrayList[WorldObject]
    widgets.map(widgetsArrayList.add(_))
    widgetsArrayList
  }


  private def spritesWidget(spritesFilename: String, title: String, tiletype: String): ImageWidget = {

    val tilesFile = new File(spritesFilename)

    val tileAttributes = TileOptions.types.get(tiletype).getOrElse(TileOptions.Default)

    val dosGraphics = new DosGraphics()
    val tiles = tilesFile.exists match {
      case true => new Tiles(tileAttributes, tilesFile, dosGraphics.getRgbPalette)
      case false => new Tiles(tileAttributes)
    }
    dosGraphics.updateClut()

    val tilesImage = tiles.getTilesImage(dosGraphics.getPalette())
    val subsetImage = new BufferedImage(320, 200, BufferedImage.TYPE_INT_RGB)
    subsetImage.getGraphics.drawImage(tilesImage, 0, 0, null)

    val loader = new JButton("Edit")
    loader.addActionListener(new ActionListener() {
      def actionPerformed(event: ActionEvent): Unit = {
        main.createSpriteWindow(spritesFilename, tiletype)
      }
    })

    val buttons = List(loader)

    new ImageWidget(title, subsetImage, buttons)

  }

}
