// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.editor.view;

import java.awt.image.BufferedImage                   // scalastyle:ignore illegal.imports
import java.awt.event.{ActionEvent, ActionListener}   // scalastyle:ignore illegal.imports
import java.io.File
import java.util.ArrayList
import javax.swing.JButton

import scala.collection.JavaConverters._
import scala.sys.process._

import bdzimmer.secondary.export.model.{CollectionItem, TilesetItem, WorldItem}
import bdzimmer.secondary.export.controller.WorldLoader
import bdzimmer.secondary.editor.model._



class ScriptLoadWindow(main: Main) extends LoadWidgetWindow(main, main.contentDir, "Load Script Files") {

  def populateObjects(inputDir: String): ArrayList[ImageWidget] = {
    val world = new World(main.contentDir)
    val widgets = world.getScriptFiles.asScala.map(scriptWidget(_))

    val widgetsArrayList = new java.util.ArrayList[ImageWidget]
    widgets.foreach(widgetsArrayList.add(_))
    widgetsArrayList
  }


  private def scriptWidget(scriptFile: ScriptFile): ImageWidget = {

    val mapImages = scriptFile.getMaps.asScala.map(x => {

      val dosGraphics = new DosGraphics()

      val curMap = new Map(new File(
            main.contentDir + File.separator
            + ContentStructure.MapDir + File.separator
            + x + ".map"))

      val curTiles = new Tiles(
          TileOptions.getOrQuit("Tiles"),
          new File(
            main.contentDir + File.separator
            + ContentStructure.TileDir + File.separator
            + curMap.tileFileName + ".til"), dosGraphics.getRgbPalette())

      dosGraphics.updateClut();

      curMap.getMapImage(curTiles, dosGraphics);

    })


    val subsetImage = new BufferedImage(
        ImageWidget.DefaultWidth, ImageWidget.DefaultHeight, BufferedImage.TYPE_INT_RGB)
    mapImages.headOption.foreach(x => {
      subsetImage.getGraphics.drawImage(
          x,
          (ImageWidget.DefaultWidth - x.getWidth) / 2,
          (ImageWidget.DefaultHeight - x.getHeight) / 2,
          null)
    })

    val viewer = new JButton("View Maps")
    viewer.addActionListener(new ActionListener() {
      def actionPerformed(event: ActionEvent): Unit = {
        mapImages.foreach(new ImageWindow(_))
      }
    })


    val mapLoader = new JButton("Edit Maps")
    mapLoader.addActionListener(new ActionListener() {
      def actionPerformed(event: ActionEvent): Unit = {

        scriptFile.getMaps.asScala.map(x => {
          val mapFilename = (main.contentDir + File.separator
              + ContentStructure.MapDir + File.separator
              + x + ".map")

          val map = new Map(new File(mapFilename))

          val tileFilename = (main.contentDir + File.separator
              + ContentStructure.TileDir + File.separator
              + map.tileFileName + ".til")

          main.createLinkedTileAndMapWindows(tileFilename, mapFilename)

        })
      }
    })

    val scriptLoader = new JButton("Edit Script")
    scriptLoader.addActionListener(new ActionListener() {
      def actionPerformed(event: ActionEvent): Unit = {
        val command =  ("notepad.exe " + main.contentDir + File.separator
            + ContentStructure.ScriptDir + File.separator
            + scriptFile.getFileName())
        command.!
      }
    })

    val buttons = List(viewer, mapLoader, scriptLoader)

    new ImageWidget(scriptFile.getTitle, subsetImage, buttons)

  }

}
