// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Scala version of TileOptions. Also designed to allow external configuration of available
// TileOptions types.

// Ben Zimmer

// 2015-01-10

// TODO: redo with Scala Swing

package bdzimmer.secondary.model

import bdzimmer.secondary.view.Main


import java.awt.{Dialog, GridLayout}
import java.awt.event.{ActionListener, ActionEvent}


import javax.swing._

import scala.io.Source


import java.util.Properties

import scala.collection.JavaConverters._

object TileOptions {

  val types = getTileTypes

  def getOptions(): TileAttributes = {

    val dialog = new JDialog();
    dialog.setTitle("Tile Type");
    dialog.setLayout(new GridLayout(6, 1, 0, 0));

    // create the radio buttons, add them to a button group
    val buttons = types.keys.toList.map(x => new JRadioButton(x))
    val buttonGroup = new ButtonGroup()
    buttons.foreach(buttonGroup.add(_))
    buttons.foreach(dialog.add(_))


    // add an ok button
    val ok = new JButton("OK")
    ok.addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent): Unit = {
        dialog.dispose
      }
    })
    dialog.add(ok)

    dialog.pack
    dialog.setModalityType(Dialog.DEFAULT_MODALITY_TYPE)
    dialog.setVisible(true)

    // wait for it to close

    // now find which was selected
    val selectedName = buttons.filter(_.isSelected)(0).getText

    println("Selected Name: " + selectedName)

    types(selectedName)

  }


  def getTileTypes(): scala.collection.Map[String, TileAttributes] = {

    val prop = new Properties()
    prop.load(getClass.getClassLoader.getResourceAsStream("tiletypes.properties"))

    prop.stringPropertyNames().asScala.map(x => {

      val items = prop.getProperty(x).split(",\\s*").map(_.trim)

      (x, new TileAttributes(
          items(0).toInt, items(1).toInt, items(2).toInt,
          items(3).toInt, items(4).toInt, items(5).toBoolean,
          items(6).toInt))

    }).toList.toMap


  }



  // for Java compatibility
  def get(key: String): TileAttributes = {
    types.get(key).getOrElse({
      println("Tile type does not exist!")
      sys.exit(1)
    })
  }

}
