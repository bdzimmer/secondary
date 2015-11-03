// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.editor.view;

import java.awt.image.BufferedImage                   // scalastyle:ignore illegal.imports
import java.awt.event.{ActionEvent, ActionListener}   // scalastyle:ignore illegal.imports
import java.io.File
import java.util.ArrayList
import javax.swing.JButton

import bdzimmer.secondary.editor.model.{DosGraphics, TileAttributes, TileOptions, Tiles}



class TilesLoadWindow(main: Main) extends LoadWidgetWindow(main, main.contentDir, "Load Map Tiles") {

  def populateObjects(inputDir: String): ArrayList[ImageWidget] = {

   val tilesetItems = main.metadata.filter(_.assetType.equals("Tileset"))
    val widgets = tilesetItems.map(x => {
      println(inputDir + File.separator + x.filename)
      tilesWidget(inputDir + File.separator + x.filename, x.name)
    })

    val widgetsArrayList = new java.util.ArrayList[ImageWidget]
    widgets.foreach(widgetsArrayList.add(_))
    widgetsArrayList
  }


  private def tilesWidget(tilesFilename: String, title: String): ImageWidget = {

    val tilesFile = new File(tilesFilename)

    val dosGraphics = new DosGraphics()
    val tiles = tilesFile.exists match {
      case true => new Tiles(TileOptions.getOrQuit("Tiles"), tilesFile, dosGraphics.getRgbPalette)
      case false => new Tiles(TileOptions.getOrQuit("Tiles"))
    }
    dosGraphics.updateClut()

    val tilesImage = tiles.getTilesImage(dosGraphics.getPalette)

    val loader = new JButton("Edit")
    loader.addActionListener(new ActionListener() {
      def actionPerformed(event: ActionEvent): Unit = {
        main.createLinkedTileAndMapWindows(tilesFilename, "")
      }
    })

    val buttons = List(loader)

    new ImageWidget(title, tilesImage, buttons)

  }

}
