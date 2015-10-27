// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Window and functions for the world's collection of sprite sheets.

// Ben Zimmer

// 2015-01-10

// TODO: redo with Scala Swing

package bdzimmer.secondary.editor.view

import java.awt.{Color, Component}                     // scalastyle:ignore illegal.imports
import java.awt.event.{ActionEvent, ActionListener}    // scalastyle:ignore illegal.imports
import java.io.File
import java.util.ArrayList

import bdzimmer.secondary.editor.model.{DosGraphics, TileAttributes, TileOptions, Tiles}

import javax.swing.{ImageIcon, JButton}


class SpriteLoadWindow(main: Main, inputDir: String) extends WorldObjectWindow(main, inputDir, "Load Sprites") {

  val spritesheetList = "list.csv"

  def populateObjects(inputDir: String): ArrayList[WorldObject] = {

    // load a list from the sprite directory describing the collection
    // of spritesheets and their various types and descriptions

    // TODO: spritesheet loading with csv is incorrect and obsolete
    val lines = scala.io.Source.fromFile(inputDir + spritesheetList).getLines

    val filesAndDescriptions = lines.map(x => {
      val items = x.split(",\\s*").map(_.trim)

      // TODO: do something else here to deal with the Option
      (items(0), items(1), TileOptions.getOrQuit(items(2)))

    }).toList

    val iconsList = filesAndDescriptions.map(x => new SpriteIcon(inputDir + x._1, x._3, 320, 200, x._2))

    // this isn't so great
    val iconsArrayList = new java.util.ArrayList[WorldObject]
    iconsList.map(iconsArrayList.add(_))
    iconsArrayList
  }




  class SpriteIcon(spriteFileName: String, attrs: TileAttributes, width: Int, height: Int, description: String) extends WorldObject {

    val dg = new DosGraphics

    val spriteSheet = new Tiles(
        attrs, new File(spriteFileName), dg.getRgbPalette)
    dg.updateClut()

    val spriteSheetImage = spriteSheet.getTilesImage(spriteSheet.attrs.count / 16, 16, dg.getPalette)


    setAlignmentX(Component.RIGHT_ALIGNMENT);

    val loader = new JButton();
    loader.setSize(width, height);
    loader.setIcon(new ImageIcon(spriteSheetImage));
    loader.setBackground(Color.BLACK);
    loader.setForeground(Color.WHITE);
    loader.setText("\n\n\n" + description + " -  " + new File(spriteFileName).getName());

    // don't know why this doesn't work
    // loader.setHorizontalTextPosition(JButton.CENTER)
    // loader.setVerticalTextPosition(JButton.CENTER)

    loader.addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent): Unit = {
        // do nothing
      }
    })

    this.add(loader);

  }

}