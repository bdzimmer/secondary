// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.editor.view

// scalastyle:ignore illegal.imports
import java.awt.{Color, BorderLayout, Dimension, GridLayout, Graphics, Font}
import java.awt.event.{ActionEvent, ActionListener}   // scalastyle:ignore illegal.imports
import java.awt.image.BufferedImage                   // scalastyle:ignore illegal.imports

import java.io.File

import javax.swing.{JButton, JPanel, SwingConstants}
import javax.swing.border.EmptyBorder


class ImageWidget(
    title: String,
    image: BufferedImage,
    buttons: List[JButton],
    buttonWidth: Int = 64) extends WorldObject(image.getWidth + buttonWidth, image.getHeight) {

  setLayout(new BorderLayout())

  val buttonPanel = new JPanel()
  buttonPanel.setPreferredSize(new Dimension(buttonWidth, this.getWidth))
  buttonPanel.setLayout(new GridLayout(buttons.length, 1, 0, 0))
  buttonPanel.setBackground(Color.black)

  buttons.foreach(buttonPanel.add(_))
  add(buttonPanel, BorderLayout.EAST)

  override def paintComponent(graphics: Graphics): Unit = {
    super.paintComponent(graphics)

    graphics.setColor(Color.black)
    graphics.fillRect(0, 0, this.getWidth, this.getHeight)
    graphics.drawImage(image, 0, 0, null)

    graphics.setFont(new Font("Monospace", Font.BOLD, 16))
    graphics.setColor(Color.white)
    graphics.drawString(title, 10, 20)
  }

}
