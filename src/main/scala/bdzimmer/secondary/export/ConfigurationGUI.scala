// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// GUI for configuring properties for the world builder application.

// 2015-08-24: Created.

package bdzimmer.secondary.export

import scala.swing._
import scala.swing.event._
import BorderPanel.Position._

import scala.collection.JavaConverters

import java.io.FileInputStream
import java.util.Properties

import java.awt.Font                    // scalastyle:ignore illegal.imports
import javax.swing.border.EmptyBorder
import javax.swing.SwingConstants



object ConfigurationGUI extends SimpleSwingApplication {

  val prop = new PropertiesWrapper(DriverConfig.propFilename)

  def top = new MainFrame {

    val BORDER_WIDTH = 10
    val myBorder = new EmptyBorder(BORDER_WIDTH, BORDER_WIDTH, BORDER_WIDTH, BORDER_WIDTH)


    val saveStatus = new Label("")

    val save = new Button("Save") {
      reactions += {
        case ButtonClicked(save) => {

          // it appears that other events be handled while this is running,
          // which is exactly what I want

          // TODO: verify the configuration

          prop.prop.store(
              new java.io.FileOutputStream(DriverConfig.propFilename),
              "created with configuration editor")
          saveStatus.text = "Saved."
        }
      }
    }



    val savePanel = new GridPanel(1, 3) {
      contents ++= List(saveStatus, new Label(), save)
    }

    def configField(key: String) = new TextField {
      text = prop(key).getOrElse("")
      columns = 75                         // scalastyle:ignore magic.number
      font = new Font("monospaced", Font.PLAIN, 12)
      reactions += {
        case ValueChanged(field) => {
          prop.set(key, text)
          saveStatus.text = "Modified."
        }
      }
    }


    val columnPairs = DriverConfig.requiredProperties.map({case (x, y) => (configField(x), y + ":")}) :+
      (savePanel, "")


    title = "World Builder Configuration"

    contents = new BorderPanel {

       border = myBorder

       layout(new FlowPanel {
         contents += new Label("Configuration file: " + DriverConfig.propFilename)
       }) = South

       val numRows = columnPairs.length

       layout(new GridPanel(numRows, 1) {
         contents ++= columnPairs.map(x => new Label(x._2) {
           horizontalAlignment = Alignment.Left
         })
         border = myBorder
       }) = West

       layout(new GridPanel(numRows, 1) {
         contents ++= columnPairs.map(_._1)
         border = myBorder
       }) = Center
    }
  }


}



class PropertiesWrapper(filename: String) {

    val file = new java.io.File(filename)
    val prop = new Properties()

    if (file.exists) {
      prop.load(new FileInputStream(file))
    }

    def apply(key: String): Option[String] = Option(prop.getProperty(key))
    def set(k: String, v: String): Unit = prop.setProperty(k, v)

}
