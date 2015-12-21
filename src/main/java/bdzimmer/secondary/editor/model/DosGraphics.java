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
  private final int height;
  private final int width; // size

  private final int[] palette = new int[256];
  private int[][] rgbPalette = new int[256][3];
  
  private boolean showGrid = false;
  private int gridWidth = 0;
  private int gridHeight = 0;



  /**
   * Create a new DosGraphics instance.
   * 
   * @param height     vertical dimension
   * @param width      horizontal dimension
   * @param scale      pixel scaling
   */
  public DosGraphics(int height, int width, int scale) {
    
    this.height = height;
    this.width = width;
    this.scale = scale;

    
    screenBuffer = new BufferedImage(
        this.width * this.scale, this.height * this.scale, BufferedImage.TYPE_INT_RGB);
    buffer = ((DataBufferInt) screenBuffer.getRaster().getDataBuffer()).getData();
    
    setPreferredSize(new Dimension(this.width * this.scale, this.height * this.scale));
    setIgnoreRepaint(false);
    setVisible(true);
  
  }

  
  public DosGraphics() {
    this(240, 320, 2);
  }

  
  // functions for drawing tiles and tilesets
  
  /**
   * Draw a tile without transparency.
   * 
   * @param tile  2d int array of tile
   * @param y    vertical position to draw at
   * @param x    horizontal position to draw at
   */
  public void drawTile(int[][] tile, int y, int x) {
    if (tile != null) {
      // Draw tile to screen.
      for (int i = 0; i < tile.length; i++) {
        for (int j = 0; j < tile[0].length; j++) {
          setPixel(y + i, x + j, tile[i][j]);

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
  public void drawTileTrans(int[][] tile, int y, int x) {
    if (tile != null) {
      // Draw tile to screen.
      for (int i = 0; i < tile.length; i++) {
        for (int j = 0; j < tile[0].length; j++) {
          int curColor = tile[i][j];
          if (curColor != 255) {
            setPixel(y + i, x + j, curColor);
          }
        }
      }

    }
  }
  
  
  /**
   * Draw the tileset.
   * @param tiles    Tiles to draw
   */
  public void drawTileset(Tileset tileset) {
    if (tileset.tiles() != null) {
      int numRows = (int)Math.ceil((float)tileset.tiles().length / tileset.tilesPerRow());
      for (int i = 0; i < numRows; i++) { 
        for (int j = 0; j < tileset.tilesPerRow(); j++) {
          int curTile = i * tileset.tilesPerRow() + j;
          if (curTile < tileset.tiles().length) {
            drawTile(
                tileset.tiles()[curTile].pixels(),
                i * tileset.height(),
                j * tileset.width());
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
  public void setPixel(int y, int x, int colorIndex) {
    int rowLength = width * scale;
    int upperleft = y * scale * rowLength + x * scale;
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
      palette[i] = 255 << 24 | (rgbPalette[i][0] * 4) << 16
          | (rgbPalette[i][1] * 4) << 8 | (rgbPalette[i][2] * 4);
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
  
  public boolean getShowGrid() {
    return showGrid;
  }
  
  public void setShowGrid(boolean showGrid) {
    this.showGrid = showGrid;
  }
  
  public void setGridDimensions(int gridWidth, int gridHeight) {
    this.gridWidth = gridWidth;
    this.gridHeight = gridHeight;
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
    graphics.fillRect(0, 0, width * 2 - 1, height * 2 - 1);
    graphics.drawImage(screenBuffer, 0, 0, null);
    if (showGrid) drawGrid(graphics);
  }
  
  // helper function for drawing a grid
  private void drawGrid(Graphics g) {
    System.out.println("drawing grid");
    g.setColor(new Color(
        rgbPalette[255][0] * 4, rgbPalette[255][1] * 4, rgbPalette[255][2] * 4));
    for (int i = 0; i < height * scale; i += gridHeight * scale) { 
      g.drawLine(0, i, width * scale - 1, i);
    }    
    for (int j = 0; j < width * scale; j += gridWidth * scale) {
      g.drawLine(j, 0, j, height * scale - 1);
    }
  }

}