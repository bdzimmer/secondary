// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.view;

import java.awt.{Color, Component}                     // scalastyle:ignore illegal.imports
import java.awt.event.{ActionEvent, ActionListener}    // scalastyle:ignore illegal.imports
import java.io.File
import java.util.ArrayList

import bdzimmer.secondary.export.{CollectionItem, TilesetItem, WorldItem, WorldLoader}
import bdzimmer.secondary.model.{DosGraphics, TileAttributes, TileOptions, Tiles}

import javax.swing.{ImageIcon, JButton, SwingConstants}

class TilesLoadWindow(main: Main) extends WorldObjectWindow(main, main.contentDir, "Load Tiles") {

  def populateObjects(inputDir: String): ArrayList[WorldObject] = {

    val tilesetItems = WorldItem.filterList[TilesetItem](WorldLoader.collectionToList(main.master))
    val icons = tilesetItems.map(x => {
      println(inputDir + File.separator + x.filename)
      new TilesIcon(
          inputDir + File.separator + x.filename,
          x.name)
    })

    // this isn't so great
    val iconsArrayList = new java.util.ArrayList[WorldObject]
    icons.map(iconsArrayList.add(_))
    iconsArrayList
  }


  class TilesIcon(tilesFilename: String, name: String) extends WorldObject {

    val tilesFile = new File(tilesFilename)

    val dosGraphics = new DosGraphics()
    val tiles = tilesFile.exists match {
      case true => new Tiles(TileOptions.getOrQuit("Tiles"), tilesFile, dosGraphics.getRgbPalette)
      case false => new Tiles(TileOptions.getOrQuit("Tiles"))
    }
    dosGraphics.updateClut()

    val tilesImage = tiles.getTilesImage(16, 16, dosGraphics.getPalette)

    width = tilesImage.getWidth
    height = tilesImage.getHeight + 32

    val loader = new JButton(
        this.name,
        new ImageIcon(this.tilesImage))
    loader.setSize(width, height)
    loader.setForeground(Color.WHITE)
    loader.setBackground(Color.BLACK)
    loader.setHorizontalTextPosition(SwingConstants.CENTER)
    loader.setVerticalTextPosition(SwingConstants.TOP)
    loader.setToolTipText(tilesFilename)
    loader.addActionListener(new ActionListener() {
      def actionPerformed(event: ActionEvent): Unit = {
        main.createLinkedTileAndMapWindows(tilesFilename, "")
      }
    })
    add(loader)

  }

}
