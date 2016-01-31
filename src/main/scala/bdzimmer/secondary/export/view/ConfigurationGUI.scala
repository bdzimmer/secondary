// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Simple Scala Swing GUI for configuring properties.

package bdzimmer.secondary.export.view

import scala.swing._
import scala.swing.event._
import BorderPanel.Position._

import scala.collection.JavaConverters

import java.awt.Font                    // scalastyle:ignore illegal.imports
import javax.swing.border.EmptyBorder
import javax.swing.SwingConstants

import bdzimmer.util.PropertiesWrapper
import bdzimmer.util.StringUtils._

import bdzimmer.secondary.export.model.ConfigurationModel._


class ConfigurationGUI(
    prop: PropertiesWrapper,
    reqProps: List[ConfigField],
    guiTitle: String) extends SimpleSwingApplication {

  val propFile = prop.file
  val fieldFont = new Font("monospaced", Font.PLAIN, 12)

  def top = new Frame {

    val myBorder = new EmptyBorder(10, 10, 10, 10)  // scalastyle:ignore magic.number
    val saveStatus = new Label("")

    val save = new Button("Save") {
      reactions += {
        case ButtonClicked(save) => {
          prop.prop.store(
              new java.io.FileOutputStream(propFile),
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

    val savePanel = new GridPanel(1, 4) {   // scalastyle:ignore magic.number
      contents ++= List(saveStatus, new Label(), save, done)
    }

    def setProperty(key: String, text: String): Unit = {
      prop.set(key, text)
      saveStatus.text = "Modified."
    }

    def textConfigField(key: String, default: String) = new TextField {
      text = prop(key).getOrElse(default)
      columns = 75                         // scalastyle:ignore magic.number
      font = fieldFont
      reactions += { case ValueChanged(field) => setProperty(key, text) }
    }

    def chooseConfigField(
        key: String, default: String, choices: List[String]) = new ComboBox(choices) {
      font = fieldFont
      selection.item = prop(key).getOrElse(default)
      selection.reactions += { case SelectionChanged(field) => setProperty(key, selection.item) }
    }

    def boolConfigField(key: String, default: String) = new CheckBox {
      selected = prop(key).getOrElse(default).toBooleanSafe
      reactions += { case ButtonClicked(field) => setProperty(key, selected.toString) }
    }

    val columnPairs = reqProps.map(x => x match {
      case t: TextConfigField => (textConfigField(t.key, t.default), t.description + ":")
      case c: ChooseConfigField => (chooseConfigField(c.key, c.default, c.choices), c.description + ":")
      case b: BoolConfigField => (boolConfigField(b.key, b.default), b.description + ":")
    }) :+ (savePanel, "")

    title = guiTitle

    contents = new BorderPanel {

       border = myBorder

       layout(new FlowPanel {
         contents += new Label("Configuration file: " + propFile.getAbsolutePath)
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
