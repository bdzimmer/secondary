// Copyright (c) 2017 Ben Zimmer. All rights reserved.

// Utility for easily saving images from the clipboard into Secondary projects.

package bdzimmer.secondary.export.view

import java.io.File

import java.awt.{BorderLayout, Desktop, Font, GridLayout, Image, Toolkit}
import java.awt.datatransfer.{DataFlavor, Transferable, StringSelection}
import java.awt.event.{ActionEvent, ActionListener, WindowAdapter, WindowEvent}
import java.awt.image.BufferedImage

import javax.swing.{JFrame, WindowConstants, JLabel, JButton, JTextField, JTextArea, JPanel, JOptionPane}
import javax.swing.event.{DocumentEvent, DocumentListener}
import javax.imageio.ImageIO

import scala.sys.process._
import scala.util.Try

import bdzimmer.util.StringUtils._

import bdzimmer.pixeleditor.view.ImageWindow


class ScreenshotUtility(
    contentDir: String) extends JFrame {

  var image: Option[Image] = None

  setTitle("Secondary Screenshot Utility")
  setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  addWindowFocusListener(new WindowAdapter() {
    override def windowGainedFocus(event: WindowEvent): Unit = {
	    onFocus()
	  }
  })
  setFocusable(true)

  val panel = new JPanel(new GridLayout(8, 2))

  panel.add(new JLabel("clipboard:"))
  val clipboardInfo = new JLabel(" " * 60)
  panel.add(clipboardInfo)

  panel.add(new JLabel("id:"))
  val id = new JTextField("image")
  ScreenshotUtility.addChangeListener(id, updateSecondaryCode)
  panel.add(id)

  panel.add(new JLabel("name:"))
  val name = new JTextField("Image")
  ScreenshotUtility.addChangeListener(name, updateSecondaryCode)
  panel.add(name)

  panel.add(new JLabel("path:"))
  val path = new JTextField("Images")
  ScreenshotUtility.addChangeListener(path, updateSecondaryCode)
  panel.add(path)

  panel.add(new JLabel("output directory:"))
  val outputDirText = new JTextField("images")
  ScreenshotUtility.addChangeListener(outputDirText, updateSecondaryCode)
  panel.add(outputDirText)


  val save = new JButton("Save")
  save.addActionListener(new ActionListener() {
    def actionPerformed(e: ActionEvent): Unit = {
      if (new File(outputDir).exists()) {
        if (!(new File(outputFilename).exists())) {
          image.foreach(x => {
            val bufferedImage = new BufferedImage(
              x.getWidth(null), x.getHeight(null), BufferedImage.TYPE_INT_ARGB)
            val graphics = bufferedImage.createGraphics()
            graphics.drawImage(x, 0, 0, null)
            graphics.dispose()
            ImageIO.write(
              bufferedImage, "png", new File(outputFilename))
          })
        } else {
          JOptionPane.showMessageDialog(
            ScreenshotUtility.this, "Output file already exists; delete it or change the id.")
        }
      } else {
        JOptionPane.showMessageDialog(
          ScreenshotUtility.this, "Output directory doesn't exist.")
      }
    }
  })
  panel.add(save)


  val edit = new JButton("Edit")
  edit.addActionListener(new ActionListener() {
    def actionPerformed(e: ActionEvent): Unit = {
      if (new File(outputFilename).exists()) {
        val command = "C:/Windows/System32/mspaint \"" + new File(outputFilename).getAbsolutePath() + "\""
        println(command)
        Try(command.!!)
      } else {
        JOptionPane.showMessageDialog(
          ScreenshotUtility.this, "Specified output filename doesn't exist.")
      }
    }
  })
  panel.add(edit)

  val copy = new JButton("Copy Code")
  copy.addActionListener(new ActionListener() {
    def actionPerformed(e: ActionEvent): Unit = {
      val clipboard = Toolkit.getDefaultToolkit().getSystemClipboard()
      clipboard.setContents(new StringSelection(secondary.getText()), null)
    }
  })
  panel.add(copy)

  val preview = new JButton("Preview")
  preview.addActionListener(new ActionListener() {
    def actionPerformed(e: ActionEvent): Unit = {
      image.foreach(x => new ImageWindow(x))
    }
  })
  panel.add(preview)

  val open = new JButton("Output Directory")
  open.addActionListener(new ActionListener() {
    def actionPerformed(e: ActionEvent): Unit = {
      Try(Desktop.getDesktop.open(new File(outputDir)))
    }
  })
  panel.add(open)

  add(panel, BorderLayout.NORTH)

  val secondary = new JTextArea(8, 30)
  secondary.setFont(new Font("monospaced", Font.BOLD, 12))
  secondary.setEditable(false)
  add(secondary, BorderLayout.SOUTH)

  updateSecondaryCode()
  pack()
  setResizable(false)
  setLocationRelativeTo(null)
  setVisible(true)

  //// //// //// ////

  def outputFilename(): String = outputDir / id.getText() + ".png"
  def outputDir(): String = contentDir / outputDirText.getText()
  def relOutputFilename(): String = new File(contentDir).toURI().relativize(
    new File(outputFilename).toURI()).getPath()

  def onFocus(): Unit = {
    image = getImageFromClipboard()
    val text = image match {
      case Some(x) => "image size " + x.getWidth(null) + "x" + x.getHeight(null)
      case _       => "no image data"
    }
    clipboardInfo.setText(text)
    repaint()
  }


  def getImageFromClipboard(): Option[Image] = {
     val transferable = Toolkit.getDefaultToolkit().getSystemClipboard().getContents(null);
     if (transferable != null && transferable.isDataFlavorSupported(DataFlavor.imageFlavor)) {
       Try(transferable.getTransferData(DataFlavor.imageFlavor).asInstanceOf[Image]).toOption
     } else {
       None
     }
  }


  def updateSecondaryCode(): Unit = {
    val code = (
        "!image\n" +
        "id: "       + id.getText()        + "\n" +
        "name: "     + name.getText()      + "\n" +
        "filename: " + relOutputFilename() + "\n" +
        "path: "     + path.getText()      + "\n\n" +

        "{{image: " + id.getText() + " | responsive=true}}"
    )
    secondary.setText(code)
    repaint()
  }

}



object ScreenshotUtility {

  def addChangeListener(textField: JTextField, callback: () => Unit): Unit = {
    textField.getDocument().addDocumentListener(new DocumentListener() {
      def insertUpdate(e: DocumentEvent): Unit = {
        callback()
      }
      def removeUpdate(e: DocumentEvent): Unit = {
        callback()
      }
      def changedUpdate(e: DocumentEvent): Unit = {
        callback()
      }
    });
  }
}
