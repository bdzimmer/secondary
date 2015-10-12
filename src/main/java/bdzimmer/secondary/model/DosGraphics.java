// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.model;

// 11-20-12: Modifying to allow different scale factors (at least on creation).

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferInt;

import javax.swing.JPanel;

public class DosGraphics extends JPanel {
  private static final long serialVersionUID = 1; // Meaningless junk.

  private int scale;
  private int sud;
  private int slr; // size

  // public static int[] palette = new int[256];
  // public static int[][] RGBPalette = new int[256][3];

  private int[] palette = new int[256];
  private int[][] rgbPalette = new int[256][3];

  private final BufferedImage screenBuffer;
  private final int[] buffer;

  /**
   * Create a new DosGraphics instance.
   * 
   * @param ud      vertical dimension
   * @param lr      horizontal dimension
   * @param scale   pixel scaling
   */
  public DosGraphics(int ud, int lr, int scale) {
    this.sud = ud;
    this.slr = lr;
    this.scale = scale;

    setPreferredSize(new Dimension(this.slr * this.scale, this.sud * this.scale));
    setVisible(true);
    setIgnoreRepaint(false);
    // Buffers...finally...
    screenBuffer = new BufferedImage(slr * this.scale, sud * this.scale,
        BufferedImage.TYPE_INT_RGB); // Created image buffer
    // I don't fully understand this, but I think it is somehow linking the
    // pixels in the BufferedImage with an array.
    buffer = ((DataBufferInt) screenBuffer.getRaster().getDataBuffer())
        .getData();
  }

  public DosGraphics() {
    this(240, 320, 2);
  }

  /**
   * Draw a tile without transparency.
   * 
   * @param tile  2d int array of tile
   * @param ud    vertical position to draw at
   * @param lr    horizontal position to draw at
   */
  public void drawTile(int[][] tile, int ud, int lr) {
    if (tile != null) {
      // Draw tile to screen.
      for (int i = 0; i < tile.length; i++) {
        for (int j = 0; j < tile[0].length; j++) {
          setPixel(ud + i, lr + j, tile[i][j]);

        }
      }

    }
  }

  /**
   * Draw a tile with transparency.
   * 
   * @param tile  2d int array of tile
   * @param ud    vertical position to draw at
   * @param lr    horizontal position to draw at
   */
  public void drawTileTrans(int[][] tile, int ud, int lr) {
    if (tile != null) {
      // Draw tile to screen.
      for (int i = 0; i < tile.length; i++) {
        for (int j = 0; j < tile[0].length; j++) {
          int curColor = tile[i][j];
          if (curColor != 255) {
            setPixel(ud + i, lr + j, curColor);
          }
        }
      }

    }
  }

  /**
   * Set a pixel at a given location.
   * 
   * @param ud        vertical position
   * @param lr        horizontal position
   * @param myColor   color to set
   */
  public void setPixel(int ud, int lr, int myColor) {
    int rowLength = this.slr * this.scale;
    int upperleft = ud * this.scale * rowLength + lr * this.scale;
    int curColor = palette[myColor];

    if (this.scale == 2) {

      this.buffer[upperleft] = curColor;
      this.buffer[upperleft + 1] = curColor;
      this.buffer[upperleft + rowLength] = curColor;
      this.buffer[upperleft + rowLength + 1] = curColor;

    } else if (this.scale == 3) {

      this.buffer[upperleft] = curColor;
      this.buffer[upperleft + 1] = curColor;
      this.buffer[upperleft + 2] = curColor;

      this.buffer[upperleft + rowLength] = curColor;
      this.buffer[upperleft + rowLength + 1] = curColor;
      this.buffer[upperleft + rowLength + 2] = curColor;

      rowLength *= 2;

      this.buffer[upperleft + rowLength] = curColor;
      this.buffer[upperleft + rowLength + 1] = curColor;
      this.buffer[upperleft + rowLength + 2] = curColor;

    } else {

      for (int i = 0; i < scale; i++) {
        int curRowOffset = rowLength * i;
        for (int j = 0; j < scale; j++) {
          this.buffer[upperleft + curRowOffset + j] = curColor;
        }
      }
    }
  }

  public BufferedImage getBuffer() {
    return this.screenBuffer;
  }

  public int getScale() {
    return this.scale;
  }

  /**
   * Recalculate the CLUT from the RGB palette. Requires redrawing of all
   * DosGraphics instances sharing this palette to take effect.
   * 
   */
  public void updateClut() {
    // this also requires redrawing of all DosGraphics' to take effect
    for (int i = 0; i < 256; i++) {
      this.palette[i] = 255 << 24 | (this.rgbPalette[i][0] * 4) << 16
          | (this.rgbPalette[i][1] * 4) << 8 | (this.rgbPalette[i][2] * 4);
    }

  }

  public int[] getPalette() {
    return this.palette;
  }

  public int[][] getRgbPalette() {
    return this.rgbPalette;
  }

  public void setRgbPalette(int[][] rgbPalette) {
    this.rgbPalette = rgbPalette;
  }

  /**
   * Draw the component.
   */
  public void paint(Graphics graphics) {
    super.paintComponent(graphics); // Draw things in superclass

    graphics.setColor(Color.BLACK);
    graphics.fillRect(0, 0, slr * 2 - 1, sud * 2 - 1);
    graphics.drawImage(screenBuffer, 0, 0, null);

  }

}