// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Window and functions for the world's collection of sprite sheets.

// Ben Zimmer

// 2015-01-10

// TODO: redo with Scala Swing


package bdzimmer.secondary.view

import java.awt.{Color, Component}
import java.awt.event.{ActionEvent, ActionListener}
import java.io.File
import java.util.ArrayList

import bdzimmer.secondary.model.{DosGraphics, TileAttributes, TileOptionsNew, Tiles}

import javax.swing.{ImageIcon, JButton}


class SpriteLoadWindow(main: Main, inputDir: String) extends WorldObjectWindow(main, inputDir, "Load Sprites") {

  val serialVersionUID: Long = 1L

  val spritesheetList = "list.csv"

  def populateObjects(inputDir: String): ArrayList[WorldObject] = {

    // load a list from the sprite directory describing the collection
    // of spritesheets and their various types and descriptions

    // TODO: this is incorrect and obsolete
    val lines = scala.io.Source.fromFile(inputDir + spritesheetList).getLines

    val filesAndDescriptions = lines.map(x => {
      val items = x.split(",\\s*").map(_.trim)

      (items(0), items(1), TileOptionsNew.get(items(2)))

    }).toList


    val iconsList = filesAndDescriptions.map(x => new SpriteIcon(inputDir + x._1, x._3, 320, 200, x._2))


    // this isn't so great
    val iconsArrayList = new java.util.ArrayList[WorldObject]
    iconsList.map(iconsArrayList.add(_))

    iconsArrayList
  }




  class SpriteIcon(spriteFileName: String, attrs: TileAttributes, width: Int, height: Int, description: String) extends WorldObject  {

    val serialVersionUID: Long = 1L

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
