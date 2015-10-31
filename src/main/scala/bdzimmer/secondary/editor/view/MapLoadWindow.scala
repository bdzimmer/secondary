// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.editor.view;

import java.awt.image.BufferedImage                   // scalastyle:ignore illegal.imports
import java.awt.event.{ActionEvent, ActionListener}   // scalastyle:ignore illegal.imports
import java.io.File
import java.util.ArrayList
import javax.swing.JButton

import bdzimmer.secondary.export.model.{CollectionItem, MapItem, WorldItem}
import bdzimmer.secondary.editor.model.{ContentStructure, DosGraphics, Map, TileAttributes, TileOptions, Tiles}


class MapLoadWindow(main: Main) extends LoadWidgetWindow(main, main.contentDir, "Load Maps") {

  def populateObjects(inputDir: String): ArrayList[ImageWidget] = {

    val mapItems = WorldItem.filterList[MapItem](WorldItem.collectionToList(main.master))
    val widgets = mapItems.map(x => {
      println(inputDir + File.separator + x.filename)
      mapWidget(inputDir + File.separator + x.filename, x.name)
    })

    val widgetsArrayList = new java.util.ArrayList[ImageWidget]
    widgets.foreach(widgetsArrayList.add(_))
    widgetsArrayList
  }


  private def mapWidget(mapFilename: String, title: String): ImageWidget = {

    val mapFile = new File(mapFilename)

    val dosGraphics = new DosGraphics()
    val map = mapFile.exists match {
      case true => new Map(mapFile)
      case false => new Map()
    }

    val tilesFilename =  main.contentDir + File.separator + ContentStructure.TileDir + File.separator + map.tileFileName + ".til"

    val mapTiles = new Tiles(
        TileOptions.getOrQuit("Tiles"),
        new File(tilesFilename), dosGraphics.getRgbPalette())
    dosGraphics.updateClut()
    val mapImage = map.getMapImage(mapTiles, dosGraphics)

    val subsetImage = new BufferedImage(320, 200, BufferedImage.TYPE_INT_RGB)
    subsetImage.getGraphics.drawImage(
        mapImage, (320 - mapImage.getWidth) / 2, (200 - mapImage.getHeight) / 2, null)

    val loader = new JButton("Edit")
    loader.addActionListener(new ActionListener() {
      def actionPerformed(event: ActionEvent): Unit = {
        main.createLinkedTileAndMapWindows(tilesFilename, mapFilename)
      }
    })

    val buttons = List(loader)

    new ImageWidget(title, subsetImage, buttons)

  }

}
