// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.view;

// scalastyle:ignore illegal.imports
import java.awt.{Color, Component, BorderLayout, Dimension, GridLayout, Graphics, Font}
// scalastyle:ignore illegal.imports
import java.awt.event.{ActionEvent, ActionListener}
import java.io.File
import java.util.ArrayList

import bdzimmer.secondary.export.{CollectionItem, TilesetItem, WorldItem, WorldLoader}
import bdzimmer.secondary.model.{DosGraphics, TileAttributes, TileOptions, Tiles}

import javax.swing.{ImageIcon, JButton, JPanel, SwingConstants}
import javax.swing.border.EmptyBorder


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


  class TilesIcon(tilesFilename: String, val name: String) extends WorldObject(320, 256) {

    val tilesFile = new File(tilesFilename)

    val dosGraphics = new DosGraphics()
    val tiles = tilesFile.exists match {
      case true => new Tiles(TileOptions.getOrQuit("Tiles"), tilesFile, dosGraphics.getRgbPalette)
      case false => new Tiles(TileOptions.getOrQuit("Tiles"))
    }
    dosGraphics.updateClut()

    val tilesImage = tiles.getTilesImage(16, 16, dosGraphics.getPalette)

    setLayout(new BorderLayout())

    val buttonPanel = new JPanel()
    buttonPanel.setPreferredSize(new Dimension(64, 256))
    buttonPanel.setLayout(new GridLayout(5, 1, 0, 0))
    buttonPanel.setBackground(Color.black)
    val loader = new JButton("Edit")
    loader.addActionListener(new ActionListener() {
      def actionPerformed(event: ActionEvent): Unit = {
        main.createLinkedTileAndMapWindows(tilesFilename, "")
      }
    })
    buttonPanel.add(loader)
    add(buttonPanel, BorderLayout.EAST)


    override def paintComponent(graphics: Graphics): Unit = {
      super.paintComponent(graphics)

      graphics.setColor(Color.black)
      graphics.fillRect(0, 0, this.getWidth, this.getHeight)
      graphics.drawImage(tilesImage, 0, 0, null)

      graphics.setFont(new Font("Monospace", Font.BOLD, 16))
      graphics.setColor(Color.white)
      graphics.drawString(name, 10, 20)
    }

  }

}
