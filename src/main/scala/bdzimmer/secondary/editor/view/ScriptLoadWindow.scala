// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.editor.view;

import java.awt.image.BufferedImage                   // scalastyle:ignore illegal.imports
import java.awt.event.{ActionEvent, ActionListener}   // scalastyle:ignore illegal.imports
import java.io.File
import java.util.ArrayList
import javax.swing.JButton

import scala.collection.JavaConverters._
import scala.sys.process._

import bdzimmer.util.{Result, Pass, Fail}
import bdzimmer.util.StringUtils._

import bdzimmer.secondary.editor.model._
import bdzimmer.secondary.editor.controller.OldTilesetLoader



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

      val image = for {
        mapFile <- Result.fromFilename(main.contentDir / ContentStructure.MapDir / x + ".map")
        map = new Map(mapFile)
        tilesFile <- Result.fromFilename(
            main.contentDir / ContentStructure.TileDir / map.tileFileName + ".til")
        tiles = new OldTilesetLoader(tilesFile.getPath, TileOptions.getOrQuit("Tiles")).load()
      } yield {
        map.image(tiles, tiles.palettes(0))
      }

      image match {
        case Pass(x) => Some(x)
        case Fail(x) => None
      }

    }).flatten


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
          val mapFilename = (main.contentDir / ContentStructure.MapDir / x + ".map")
          val map = new Map(new File(mapFilename))
          val tileFilename = (main.contentDir / ContentStructure.TileDir / map.tileFileName + ".til")

          main.createLinkedTileAndMapWindows(tileFilename, mapFilename)

        })
      }
    })

    val scriptLoader = new JButton("Edit Script")
    scriptLoader.addActionListener(new ActionListener() {
      def actionPerformed(event: ActionEvent): Unit = {
        val command =  ("notepad.exe " + main.contentDir /
            ContentStructure.ScriptDir /
            scriptFile.getFileName())
        command.!
      }
    })

    val buttons = List(viewer, mapLoader, scriptLoader)

    new ImageWidget(scriptFile.getTitle, subsetImage, buttons)

  }

}
