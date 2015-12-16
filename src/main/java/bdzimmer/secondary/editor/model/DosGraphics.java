// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.editor.model;

// 11-20-12: Modifying to allow different scale factors (at least on creation).

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferInt;
import java.awt.image.IndexColorModel;

import javax.swing.JPanel;

public class DosGraphics extends JPanel {
  private static final long serialVersionUID = 1; // Meaningless junk.

  private final BufferedImage screenBuffer;
  private final int[] buffer;
  
  private final int scale;
  private final int sud;
  private final int slr; // size

  private final int[] palette = new int[256];
  private int[][] rgbPalette = new int[256][3];



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
   
    screenBuffer = new BufferedImage(slr * this.scale, sud * this.scale,
        BufferedImage.TYPE_INT_RGB); // Created image buffer
    buffer = ((DataBufferInt) screenBuffer.getRaster().getDataBuffer())
        .getData();
  }

  public DosGraphics() {
    this(240, 320, 2);
  }

  
  // functions for drawing tiles and tilesets
  
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
   * Draw the tileset.
   * @param tiles    Tiles to draw
   */
  public void drawTileset(Tiles tiles) {
    if (tiles.tiles != null) {
      int numRows = (int)Math.ceil((float)tiles.attrs.count / tiles.attrs.tilesPerRow);
      for (int i = 0; i < numRows; i++) { 
        for (int j = 0; j < tiles.attrs.tilesPerRow; j++) {
          int curTile = i * tiles.attrs.tilesPerRow + j;
          if (curTile < tiles.attrs.count) {
            drawTile(
                tiles.tiles[curTile],
                i * tiles.attrs.height,
                j * tiles.attrs.width);
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
  public void setPixel(int ud, int lr, int colorIndex) {
    int rowLength = slr * scale;
    int upperleft = ud * scale * rowLength + lr * scale;
    int curColor = palette[colorIndex];

    if (this.scale == 2) {

      buffer[upperleft] = curColor;
      buffer[upperleft + 1] = curColor;
      buffer[upperleft + rowLength] = curColor;
      buffer[upperleft + rowLength + 1] = curColor;

    } else if (this.scale == 3) {

      buffer[upperleft] = curColor;
      buffer[upperleft + 1] = curColor;
      buffer[upperleft + 2] = curColor;

      buffer[upperleft + rowLength] = curColor;
      buffer[upperleft + rowLength + 1] = curColor;
      buffer[upperleft + rowLength + 2] = curColor;

      rowLength *= 2;

      buffer[upperleft + rowLength] = curColor;
      buffer[upperleft + rowLength + 1] = curColor;
      buffer[upperleft + rowLength + 2] = curColor;

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
  
  // get an IndexColorModel for part of the range
  public IndexColorModel getIndexColorModel(int start, int end) {
    byte[] r = new byte[256];
    byte[] g = new byte[256];
    byte[] b = new byte[256];
    
    for (int i = start; i <= end; i++) {
      r[i] = (byte)((rgbPalette[i][0] * 4) & 0xFF);
      g[i] = (byte)((rgbPalette[i][1] * 4) & 0xFF);
      b[i] = (byte)((rgbPalette[i][2] * 4) & 0xFF);
    }
    
    // weird things happen when you try to set a transparent index (extra argument)
    // it seems that it will always be index 0 in a png, but also strange palette
    // shifts happen if it is set to 256. Seems best to not set this for now.
    return new IndexColorModel(8, 256, r, g, b);
        
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