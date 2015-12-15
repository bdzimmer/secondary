// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.editor.view;

import java.awt.image.BufferedImage                   // scalastyle:ignore illegal.imports
import java.awt.event.{ActionEvent, ActionListener}   // scalastyle:ignore illegal.imports
import java.io.File
import java.util.ArrayList
import javax.swing.JButton

import bdzimmer.util.StringUtils._

import bdzimmer.secondary.editor.model.{DosGraphics, TileAttributes, TileOptions, Tiles}
import bdzimmer.secondary.editor.controller.OldTilesetLoader



class TilesLoadWindow(main: Main) extends LoadWidgetWindow(main, main.contentDir, "Load Map Tiles") {

  def populateObjects(inputDir: String): ArrayList[ImageWidget] = {

   val tilesetItems = main.metadata.filter(_.assetType.equals("Tileset"))
    val widgets = tilesetItems.map(x => {
      println(inputDir / x.filename)
      tilesWidget(inputDir / x.filename, x.name)
    })

    val widgetsArrayList = new java.util.ArrayList[ImageWidget]
    widgets.foreach(widgetsArrayList.add(_))
    widgetsArrayList
  }


  private def tilesWidget(tilesFilename: String, title: String): ImageWidget = {

    val tilesFile = new File(tilesFilename)
    val tileAttrs = TileOptions.getOrQuit("Tiles")
    val tilesImage = new OldTilesetLoader(tilesFilename, tileAttrs).load.image(0)

    val loader = new JButton("Edit")
    loader.addActionListener(new ActionListener() {
      def actionPerformed(event: ActionEvent): Unit = {
        main.createLinkedTileAndMapWindows(tilesFilename, "")
      }
    })

    new ImageWidget(title, tilesImage, List(loader))

  }

}
