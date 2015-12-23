// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.editor.view;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;

import javax.swing.JFrame;
import javax.swing.JPanel;


public class ImageWindow extends JFrame {
  
  private static final long serialVersionUID = 1L;


  /**
   * Create a new ImageWindow from an Image object. 
   * 
   * @param image Image object
   */
  public ImageWindow(Image image) {
    add(new ImagePanel(image));
    pack();
    setLocationRelativeTo(null);
    setResizable(false);
    setVisible(true);
    toFront();
  }

  class ImagePanel extends JPanel {
    private static final long serialVersionUID = 1L;

    private final Image image;

    ImagePanel(Image image) {
      this.image = image;
      setPreferredSize(
          new Dimension(image.getWidth(null), image.getHeight(null)));
      setVisible(true);
    }

    protected void paintComponent(Graphics gr) {
      gr.drawImage(image, 0, 0, null);
    }

  }

}
