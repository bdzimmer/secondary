// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// 1-8-13: Separated out from TilesetWindow.

package bdzimmer.secondary.model;

import java.awt.image.BufferedImage;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

public class Tiles {

  public int[][][] tiles;
  public int[] tileProps;

  public TileAttributes attrs;

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
   * @param tilesFile             file to load           
   * @param graphicsForPalette    DosGraphics instance to load the palette into.
   */
  public void load(File tilesFile, DosGraphics graphicsForPalette) {

    // open file for input
    try {
      DataInputStream graphicsIn = new DataInputStream(new FileInputStream(
          tilesFile));

      // Read in data.

      // System.out.println("Loading pixel data");

      int numThings = this.attrs.count;

      // Un-remark this to load small member files into a large member file.
      // if (nTiles == 128) {
      // nThings = 64;
      // }

      for (int i = 0; i < numThings; i++) {
        for (int j = 0; j < this.attrs.height; j++) {
          for (int k = 0; k < this.attrs.width; k++) {
            this.tiles[i][j][k] = (0x000000FF & (int) graphicsIn.readByte());
          }
        }

      }

      for (int i = this.attrs.palStart; i <= this.attrs.palEnd; i++) {

        final int r = (0x000000FF & (int) graphicsIn.readByte());
        graphicsIn.readByte();
        final int g = (0x000000FF & (int) graphicsIn.readByte());
        graphicsIn.readByte();
        final int b = (0x000000FF & (int) graphicsIn.readByte());
        graphicsIn.readByte();

        // System.out.println("r: "+r+" g: "+g+" b: "+ b);
        graphicsForPalette.getPalette()[i] = 255 << 24 | (r * 4) << 16
            | (g * 4) << 8 | (b * 4);
        graphicsForPalette.getRgbPalette()[i][0] = r;
        graphicsForPalette.getRgbPalette()[i][1] = g;
        graphicsForPalette.getRgbPalette()[i][2] = b;

      }
      // read in tile properties.

      if (this.attrs.tileProperties) {
        // System.out.println("Loading tile properties");
        for (int i = 0; i < this.attrs.count; i++) {
          // this.tileProps[i] = (int)graphicsIn.readShort(); // I think
          this.tileProps[i] = (0x000000FF & (int) graphicsIn.readByte()); // I
                                                                          // think
          graphicsIn.readByte();

          // System.out.println(tileProps[i]);
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

      System.out.println("Saving pixel data");

      DataOutputStream graphicsOut = new DataOutputStream(new FileOutputStream(
          tilesFile));

      // Write out data.
      for (int i = 0; i < this.attrs.count; i++) {
        for (int j = 0; j < this.attrs.height; j++) {
          for (int k = 0; k < this.attrs.width; k++) {
            graphicsOut.writeByte(this.tiles[i][j][k] & 0xFF);
          }
        }

      }

      for (int i = this.attrs.palStart; i <= this.attrs.palEnd; i++) {
        graphicsOut.writeByte(graphicsForPalette.getRgbPalette()[i][0] & 0xFF);
        graphicsOut.writeByte(0);
        graphicsOut.writeByte(graphicsForPalette.getRgbPalette()[i][1] & 0xFF);
        graphicsOut.writeByte(0);
        graphicsOut.writeByte(graphicsForPalette.getRgbPalette()[i][2] & 0xFF);
        graphicsOut.writeByte(0);
      }

      // tile properties
      if (this.attrs.tileProperties) {
        System.out.println("Saving tile properties");
        for (int i = 0; i < this.attrs.count; i++) {
          graphicsOut.writeByte(this.tileProps[i] & 0xFF); // I think
          graphicsOut.writeByte(0);
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

      if (this.attrs.count > 16) {
        for (int i = 0; i < (this.attrs.count / 16); i++) { // this will get a
                                                            // little more
                                                            // complicated
          for (int j = 0; j < 16; j++) {
            dg.drawTile(tiles[i * 16 + j], i * this.attrs.height, j
                * this.attrs.width);
          }
        }
      } else {
        for (int i = 0; i < this.attrs.count; i++) { // this will get a little
                                                     // more complicated
          dg.drawTile(tiles[i], 0, i * this.attrs.width);
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

    // System.out.println(tilesImage.getWidth() + " " + tilesImage.getHeight());

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

    /*
     * for (int i = 0; i < tilesHigh; i++) { for (int j = 0; j < tilesWide; j++)
     * {
     * 
     * for (int y = 0; y < this.attrs.height; y++) { for (int x = 0; x <
     * this.attrs.width; x++) {
     * 
     * // System.out.println(j * this.attrs.width + x + " " + i *
     * this.attrs.height + y);
     * 
     * //System.out.println("--" + i * tilesHigh + j); //System.out.println(x +
     * " " + y); //System.out.println(j * this.attrs.width + x + " " + i *
     * this.attrs.height + y);
     * 
     * tilesImage.setRGB(j * this.attrs.width + x, i * this.attrs.height + y,
     * palette[this.tiles[i * tilesHigh + j][y][x]]);
     * 
     * } }
     * 
     * 
     * 
     * } }
     */

    return tilesImage;

  }

  public int[][][] getTiles() {
    return tiles;
  }

  public int[] getTileProps() {
    return tileProps;
  }

}
