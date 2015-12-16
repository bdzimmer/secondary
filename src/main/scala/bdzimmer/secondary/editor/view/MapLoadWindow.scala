// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.editor.view;

import java.awt.image.BufferedImage                   // scalastyle:ignore illegal.imports
import java.awt.event.{ActionEvent, ActionListener}   // scalastyle:ignore illegal.imports
import java.io.File
import java.util.ArrayList
import javax.swing.JButton

import bdzimmer.util.StringUtils._

import bdzimmer.secondary.editor.model.{ContentStructure, DosGraphics, Map, TileAttributes, TileOptions}
import bdzimmer.secondary.editor.controller.OldTilesetLoader


class MapLoadWindow(main: Main) extends LoadWidgetWindow(main, main.contentDir, "Load Maps") {

  def populateObjects(inputDir: String): ArrayList[ImageWidget] = {

    val mapItems = main.metadata.filter(_.assetType.equals("Map"))
    val widgets = mapItems.map(x => {
      println(inputDir / x.filename)
      mapWidget(inputDir / x.filename, x.name)
    })

    val widgetsArrayList = new java.util.ArrayList[ImageWidget]
    widgets.foreach(widgetsArrayList.add(_))
    widgetsArrayList
  }


  private def mapWidget(mapFilename: String, title: String): ImageWidget = {

    // TODO: deal with tiles file doesn't exist

    val mapFile = new File(mapFilename)
    val map = mapFile.exists match {
      case true => new Map(mapFile)
      case false => new Map()
    }

    val tilesFilename =  main.contentDir / ContentStructure.TileDir / map.tileFileName + ".til"
    val mapTiles = new OldTilesetLoader(tilesFilename, TileOptions.getOrQuit("Tiles")).load()
    val mapImage = map.image(mapTiles, mapTiles.palettes(0))

    val subsetImage = new BufferedImage(
        ImageWidget.DefaultWidth, ImageWidget.DefaultHeight, BufferedImage.TYPE_INT_RGB)

    subsetImage.getGraphics.drawImage(
        mapImage,
        (ImageWidget.DefaultWidth - mapImage.getWidth) / 2,
        (ImageWidget.DefaultHeight - mapImage.getHeight) / 2,
        null)

    val loader = new JButton("Edit")
    loader.addActionListener(new ActionListener() {
      def actionPerformed(event: ActionEvent): Unit = {
        main.createLinkedTileAndMapWindows(tilesFilename, mapFilename)
      }
    })

    new ImageWidget(title, subsetImage, List(loader))

  }

}
