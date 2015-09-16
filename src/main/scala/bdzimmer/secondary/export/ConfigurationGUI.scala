// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// GUI for configuring properties for the world builder application.

// 2015-08-24: Created.

package bdzimmer.secondary.export

import scala.swing._
import scala.swing.event._
import BorderPanel.Position._

import scala.collection.JavaConverters

import java.awt.Font                    // scalastyle:ignore illegal.imports
import javax.swing.border.EmptyBorder
import javax.swing.SwingConstants



class ConfigurationGUI(projectConfig: ProjectConfig) extends SimpleSwingApplication {

  val propFileName = projectConfig.propFilename
  val prop = projectConfig.prop

  def top = new Frame {

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
              new java.io.FileOutputStream(propFileName),
              "created with configuration editor")
          saveStatus.text = "Saved."
        }
      }
    }

    val done = new Button("Done") {
      reactions += {
        case ButtonClicked(done) => dispose()
      }
    }

    val savePanel = new GridPanel(1, 4) {
      contents ++= List(saveStatus, new Label(), save, done)
    }

    def configField(key: String, default: String) = new TextField {
      text = prop(key).getOrElse(default)
      columns = 75                         // scalastyle:ignore magic.number
      font = new Font("monospaced", Font.PLAIN, 12)
      reactions += {
        case ValueChanged(field) => {
          prop.set(key, text)
          saveStatus.text = "Modified."
        }
      }
    }


    val columnPairs = DriverConfig.requiredProperties.map(
        x => (configField(x.key, x.default), x.description + ":")) :+ (savePanel, "")

    title = "Secondary - Project Configuration"

    contents = new BorderPanel {

       border = myBorder

       layout(new FlowPanel {
         contents += new Label("Configuration file: " + propFileName)
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

