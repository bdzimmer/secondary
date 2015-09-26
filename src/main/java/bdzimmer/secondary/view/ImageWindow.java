// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.view;

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
    this.add(new ImagePanel(image));
    this.pack();
    this.setLocationRelativeTo(null);
    this.setVisible(true);
  }

  class ImagePanel extends JPanel {
    private static final long serialVersionUID = 1L;

    private final Image image;

    ImagePanel(Image image) {
      this.image = image;
      this.setPreferredSize(new Dimension(image.getWidth(null), image
          .getHeight(null)));
      this.setVisible(true);
    }

    protected void paintComponent(Graphics gr) {
      gr.drawImage(this.image, 0, 0, null);
    }

  }

}
