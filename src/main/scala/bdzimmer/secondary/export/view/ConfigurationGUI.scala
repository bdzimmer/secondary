// Copyright (c) 2019 Ben Zimmer. All rights reserved.

// Simple Swing GUI for configuring properties.

package bdzimmer.secondary.export.view

import java.awt.Font                    // scalastyle:ignore illegal.imports
import java.awt.{GridLayout, BorderLayout}
import java.awt.event.{ActionListener, ActionEvent}
import javax.swing.{JFrame, JLabel, JButton, JTextField, JPanel, JComboBox, JCheckBox, SwingConstants}
import javax.swing.border.EmptyBorder
import javax.swing.event.{DocumentListener, DocumentEvent}

import bdzimmer.util.PropertiesWrapper
import bdzimmer.util.StringUtils._

import bdzimmer.secondary.export.model.ConfigurationModel._


class ConfigurationGUI(
    prop: PropertiesWrapper,
    reqProps: List[ConfigField],
    guiTitle: String) extends JFrame {

  val propFile = prop.file
  val fieldFont = new Font("monospaced", Font.PLAIN, 12)


  val myBorder = new EmptyBorder(10, 10, 10, 10)
  val saveStatus = new JLabel("")

  val save = new JButton("Save")
  save.addActionListener(new ActionListener() {
    override def actionPerformed(e: ActionEvent): Unit = {
      prop.prop.store(
        new java.io.FileOutputStream(propFile),
        "created with configuration editor")
      saveStatus.setText("Saved.")
    }
  })

  val done = new JButton("Done")
  done.addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      dispose()
    }
  })

  val savePanel = new JPanel(new GridLayout(1, 4))
  savePanel.add(saveStatus)
  savePanel.add(new JLabel())
  savePanel.add(save)
  savePanel.add(done)

  def setProperty(key: String, text: String): Unit = {
    prop.set(key, text)
    saveStatus.setText("Modified.")
  }

  def textConfigField(key: String, default: String) = {
    val res = new JTextField(
      prop(key).getOrElse(default),
      40
    )
    res.getDocument.addDocumentListener(new DocumentListener {
      def changedUpdate(e: DocumentEvent): Unit = setProperty(key, res.getText)
      def removeUpdate(e: DocumentEvent): Unit = setProperty(key, res.getText)
      def insertUpdate(e: DocumentEvent): Unit = setProperty(key, res.getText)
    })
    res
  }

  def chooseConfigField(key: String, default: String, choices: List[String])= {
    val res = new JComboBox[String](choices.toArray)
    res.setFont(fieldFont)
    res.setSelectedItem(prop(key).getOrElse(default))
    res.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        setProperty(key, res.getSelectedItem.toString)
      }
    })
    res
  }

  def boolConfigField(key: String, default: String) = {
    val res = new JCheckBox()
    res.setSelected(prop(key).getOrElse(default).toBooleanSafe)
    res.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        setProperty(key, res.isSelected.toString)
      }
    })
    res
  }

  val columnPairs = reqProps.map(x => x match {
    case t: TextConfigField => (textConfigField(t.key, t.default), t.description + ":")
    case c: ChooseConfigField => (chooseConfigField(c.key, c.default, c.choices), c.description + ":")
    case b: BoolConfigField => (boolConfigField(b.key, b.default), b.description + ":")
  }) :+ (savePanel, "")

  setTitle(guiTitle)

  val contents = new JPanel(new BorderLayout())

  contents.add({
    val res = new JPanel()
    res.add(new JLabel("Configuration file: " + propFile.getAbsolutePath))
    res
  }, BorderLayout.SOUTH)

  contents.add({
    val res = new JPanel(new GridLayout(columnPairs.length, 1))
    res.setBorder(myBorder)
    columnPairs.foreach(x => {
      val label = new JLabel(x._2)
      label.setHorizontalAlignment(SwingConstants.LEFT)
      res.add(label)
    })
    res
  }, BorderLayout.WEST)

  contents.add({
    val res = new JPanel(new GridLayout(columnPairs.length, 1))
    res.setBorder(myBorder)
    columnPairs.foreach(x => {
      res.add(x._1)
    })
    res
  }, BorderLayout.CENTER)

  add(contents, BorderLayout.CENTER)
  pack()
  setVisible(true)

}
