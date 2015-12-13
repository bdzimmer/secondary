// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// 1-8-13: Separated out from TilesetWindow.

package bdzimmer.secondary.editor.model;

import java.awt.image.BufferedImage;
import java.awt.image.IndexColorModel;
import java.awt.image.WritableRaster;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;


public class Tiles {

  public final int[][][] tiles;
  public final int[] tileProps;
  public final TileAttributes attrs;

  /**
   * Create a new tile set.
   * @param ta      tile attributes to use.
   */
  public Tiles(TileAttributes ta) {

    this.attrs = ta;
    tiles = new int[ta.count][ta.height][ta.width];
    tileProps = new int[ta.count];

  }

  /**
   * Load the tile set from a file.
   * 
   * @param ta                    tile attributes to use
   * @param tilesFile             file to load           
   * @param rgbPalette            DosGraphics instance to load the palette into.
   */
  public Tiles(TileAttributes ta, File tilesFile, int[][] rgbPalette) {
    
    this(ta);
    
    // open file for input
    try {
      QbInputStream graphicsIn = new QbInputStream(
          new FileInputStream(tilesFile));

      // System.out.println("Loading pixel data");

      int numThings = this.attrs.count;

      // load small member files into a large member file.
      // if (nTiles == 128) {
      // nThings = 64;
      // }

      for (int i = 0; i < numThings; i++) {
        for (int j = 0; j < this.attrs.height; j++) {
          for (int k = 0; k < this.attrs.width; k++) {
            this.tiles[i][j][k] = graphicsIn.readQbUnsignedByte();
          }
        }
      }

      for (int i = this.attrs.palStart; i <= this.attrs.palEnd; i++) {
        rgbPalette[i][0] = graphicsIn.readQbUnsignedShortLow(); // red
        rgbPalette[i][1] = graphicsIn.readQbUnsignedShortLow(); // green
        rgbPalette[i][2] = graphicsIn.readQbUnsignedShortLow(); // blue
      }
      
      // read in tile properties.
      if (this.attrs.tileProperties) {
        for (int i = 0; i < this.attrs.count; i++) {
          this.tileProps[i] = graphicsIn.readQbUnsignedShortLow();
        }
      }

      graphicsIn.close();

    } catch (FileNotFoundException e) {
      System.err.println(e); // print exception if the file doesn't exist.
      return;
    } catch (IOException e) {
      System.err.println(e);
      return;
    }

  }

  /**
   * Save the tile set to a file.
   * 
   * @param tilesFile             File to save
   * @param graphicsForPalette    DosGraphics instance to save the palette from
   */
  public void save(File tilesFile, DosGraphics graphicsForPalette) {

    // open file for output
    try {

      // System.out.println("Saving pixel data");

      QbOutputStream graphicsOut = new QbOutputStream(new FileOutputStream(
          tilesFile));

      // Write out data.
      for (int i = 0; i < this.attrs.count; i++) {
        for (int j = 0; j < this.attrs.height; j++) {
          for (int k = 0; k < this.attrs.width; k++) {
            graphicsOut.writeQbUnsignedByte(this.tiles[i][j][k]);
          }
        }
      }

      for (int i = this.attrs.palStart; i <= this.attrs.palEnd; i++) {
        graphicsOut.writeQbUnsignedShortLow(graphicsForPalette.getRgbPalette()[i][0]);
        graphicsOut.writeQbUnsignedShortLow(graphicsForPalette.getRgbPalette()[i][1]);
        graphicsOut.writeQbUnsignedShortLow(graphicsForPalette.getRgbPalette()[i][2]);
      }

      // tile properties
      if (this.attrs.tileProperties) {
        // System.out.println("Saving tile properties");
        for (int i = 0; i < this.attrs.count; i++) {
          graphicsOut.writeQbUnsignedShortLow(this.tileProps[i]);
        }
      }

      graphicsOut.close();
    } catch (FileNotFoundException e) {
      System.err.println(e); // print exception if the file doesn't exist.
      return;
    } catch (IOException e) {
      System.err.println(e);
      return;
    }

  }

  /**
   * Draw the tile set to a DosGraphics instance.
   * @param dg    DosGraphics to draw to.
   */
  public void drawTileset(DosGraphics dg) {
    if (this.tiles != null) {
      int numRows = (int)Math.ceil((float)attrs.count / attrs.tilesPerRow);
      for (int i = 0; i < numRows; i++) { 
        for (int j = 0; j < attrs.tilesPerRow; j++) {
          int curTile = i * attrs.tilesPerRow + j;
          if (curTile < attrs.count) {
            dg.drawTile(
                tiles[curTile],
                i * this.attrs.height,
                j * this.attrs.width);
          }
        }
      }
    }
  }
  
  

  /**
   * Get an image of the tile set.
   * 
   * @param tilesWide     width in tiles of the image
   * @param tilesHigh     height in tiles of the image
   * @param palette       integer rgb palette to draw with
   * @return              image of tile set
   */
  public BufferedImage getTilesImage(int tilesWide, int tilesHigh, int[] palette) {

    BufferedImage tilesImage = new BufferedImage(tilesWide * this.attrs.width,
        tilesHigh * this.attrs.height, BufferedImage.TYPE_INT_RGB);

    for (int whichTile = 0; whichTile < this.tiles.length; whichTile++) {

      int xoff = (whichTile % tilesWide) * this.attrs.width;
      int yoff = (whichTile / tilesWide) * this.attrs.height;

      for (int y = 0; y < this.attrs.height; y++) {
        for (int x = 0; x < this.attrs.width; x++) {

          tilesImage.setRGB(xoff + x, yoff + y,
              palette[this.tiles[whichTile][y][x]]);

        }
      }
    }

    return tilesImage; 
  }
  
  
  // get an indexed image of the tileset
  
  public BufferedImage getTilesImageIndexed(int tilesWide, int tilesHigh, IndexColorModel cm) {
    
    BufferedImage tilesImage = new BufferedImage(
        tilesWide * this.attrs.width,
        tilesHigh * this.attrs.height,
        BufferedImage.TYPE_BYTE_INDEXED,
        cm);
    
    WritableRaster wr = tilesImage.getRaster();
    
    for (int whichTile = 0; whichTile < this.tiles.length; whichTile++) {
      int xoff = (whichTile % tilesWide) * this.attrs.width;
      int yoff = (whichTile / tilesWide) * this.attrs.height;
      for (int y = 0; y < this.attrs.height; y++) {
        wr.setPixels(xoff, yoff + y, this.attrs.width, 1, tiles[whichTile][y]);
      }
    }
    
    return tilesImage;
     
  }
  
  
  /**
   * Get an image of the tile set, using TileAttributes to draw.
   * 
   * @param palette       integer rgb palette to draw with
   * @return              image of tile set
   */
  public BufferedImage getTilesImage(int[] palette) {
    return getTilesImage(
        attrs.tilesPerRow,
        (int)Math.ceil((float)attrs.count / attrs.tilesPerRow),
        palette); 
  }

  
  // indexed version
  public BufferedImage getTilesImageIndexed(IndexColorModel cm) {
    return getTilesImageIndexed(
        attrs.tilesPerRow,
        (int)Math.ceil((float)attrs.count / attrs.tilesPerRow),
        cm); 
  }

  
  public int[][][] getTiles() {
    return tiles;
  }

  public int[] getTileProps() {
    return tileProps;
  }

}
