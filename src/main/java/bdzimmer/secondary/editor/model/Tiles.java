// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.editor.model;

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
   * Create a new tileset from the constituent pieces.
   * @param tiles       tiles
   * @param tileProps   tile properties
   * @param attrs       tile attributes
   */
  public Tiles(int[][][] tiles, int[] tileProps, TileAttributes attrs) {
    this.tiles = tiles;
    this.tileProps = tileProps;
    this.attrs = attrs;
  }
  
  
  /**
   * Create an empty tile set.
   * @param ta      tile attributes to use.
   */
  public Tiles(TileAttributes ta) {
    this(new int[ta.count][ta.height][ta.width], new int[ta.count], ta);
  }
  
  
  //TODO: ELIMINATE these save and load functions.
  
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
      QbInputStream graphicsIn = new QbInputStream(new FileInputStream(tilesFile));

      int numThings = this.attrs.count;

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

  
  public int[][][] getTiles() {
    return tiles;
  }

  public int[] getTileProps() {
    return tileProps;
  }

}
