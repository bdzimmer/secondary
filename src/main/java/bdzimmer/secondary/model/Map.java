// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// TODO: refactor.

package bdzimmer.secondary.model;

import java.awt.image.BufferedImage;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

public class Map {

  public String mapDesc = "";

  public String tileFileName = "";
  public int[][] paraMap = new int[24][40];
  public int[][] map = new int[128][128];
  public int[][] overMap = new int[128][128];

  public boolean hasParallax;

  public int mud;
  public int mlr;
  public int pmud;
  public int pmlr;

  
  // no-arg constructor - blank map
  public Map() {
    
  }
  

  /**
   * Load the map from a file.
   * 
   * @param mapFile File to load.
   */
  public Map(File mapFile) {
    // erase current map
    for (int i = 0; i < 128; i++) {
      for (int j = 0; j < 128; j++) {
        this.map[i][j] = 0;
        this.overMap[i][j] = 0;
      }
    }
    for (int i = 0; i < 24; i++) {
      for (int j = 0; j < 40; j++) {
        this.paraMap[i][j] = 0;
      }
    }

    // open file for input
    try {
      DataInputStream mapIn = new DataInputStream(new FileInputStream(mapFile));
      // read description
      int[] mapDescB = new int[30];
      char[] mapDesc = new char[30];
      for (int i = 0; i < 30; i++) {
        mapDescB[i] = (0x000000FF & (int) mapIn.readByte());
        mapDesc[i] = (char) mapDescB[i];
        // System.out.println(mapDesc[i]);
      }
      this.mapDesc = new String(mapDesc);
      // read tileset
      int[] tileFileNameB = new int[8];
      char[] tileFileName = new char[8];
      for (int i = 0; i < 8; i++) {
        tileFileNameB[i] = (0x000000FF & (int) mapIn.readByte());
        tileFileName[i] = (char) tileFileNameB[i];
      }
      this.tileFileName = new String(tileFileName).trim();

      this.mud = (0x000000FF & (int) mapIn.readByte());
      mapIn.readByte();
      this.mlr = (0x000000FF & (int) mapIn.readByte());
      mapIn.readByte();

      // map data
      for (int i = 0; i <= mud; i++) {
        for (int j = 0; j <= mlr; j++) {
          this.map[i][j] = (0x000000FF & (int) mapIn.readByte());
          mapIn.readByte();
        }
      }
      for (int i = 0; i <= mud; i++) {
        for (int j = 0; j <= mlr; j++) {
          this.overMap[i][j] = (0x000000FF & (int) mapIn.readByte());
          mapIn.readByte();
        }
      }

      // System.out.println("Loaded normal and overlay layers.");

      try {
        this.pmud = mapIn.readByte();

        if (this.pmud > -1) { // load parallax layer
          this.pmud = (0x000000FF & (int) this.pmud);
          mapIn.readByte();
          this.pmlr = (0x000000FF & (int) mapIn.readByte());
          mapIn.readByte();
          for (int i = 0; i <= pmud; i++) {
            for (int j = 0; j <= pmlr; j++) {
              this.paraMap[i][j] = (0x000000FF & (int) mapIn.readByte());
              mapIn.readByte();
            }
          }
          this.hasParallax = true;

          // System.out.println("Loaded parallax layer.");
        }

      } catch (EOFException e) {
        this.hasParallax = false;

        this.pmud = 0;
        // System.out.println("No parallax layer.");
      }

      mapIn.close();

    } catch (FileNotFoundException e) {
      System.err.println(e); // print exception if the file doesn't exist.
      return;
    } catch (IOException e) {
      System.err.println(e);
      return;
    }

  }

  
  /**
   * Save the map to a file.
   * 
   * @param mapFile File to save
   */
  public void save(File mapFile) {
    // open file for input
    try {
      DataOutputStream mapOut = new DataOutputStream(new FileOutputStream(
          mapFile));
      // System.out.println("Opened output stream.");
      // read description
      int[] mapDescB = new int[30];
      char[] mapDesc = this.mapDesc.toCharArray();
      for (int i = 0; i < 30; i++) {
        if (i < mapDesc.length) {
          mapDescB[i] = (int) mapDesc[i];
        } else {
          mapDescB[i] = (int) ' ';
        }
        mapOut.writeByte(mapDescB[i] & 0xFF);
      }
      // write tileset name
      int[] tileFileNameB = new int[8];
      char[] tileFileName = this.tileFileName.toCharArray();
      for (int i = 0; i < 8; i++) {
        if (i < tileFileName.length) {
          tileFileNameB[i] = (int) tileFileName[i];
        } else {
          tileFileNameB[i] = (int) ' ';
        }
        mapOut.writeByte(tileFileNameB[i] & 0xFF);

      }

      // Determine size of map to save.
      this.mud = 0;
      this.mlr = 0;
      for (int i = 0; i < 128; i++) {
        for (int j = 0; j < 128; j++) {
          if (this.map[i][j] > 0) {
            if (i > this.mud) {
              mud = i;
            }
            if (j > this.mlr) {
              mlr = j;
            }
          }
        }
      }

      System.out.println("Map size: " + mud + " " + mlr);

      mapOut.writeByte(this.mud & 0xFF);
      mapOut.writeByte(0);
      mapOut.writeByte(this.mlr & 0xFF);
      mapOut.writeByte(0);

      // map data
      for (int i = 0; i <= mud; i++) {
        for (int j = 0; j <= mlr; j++) {
          mapOut.writeByte(this.map[i][j] & 0xFF);
          mapOut.writeByte(0);
        }
      }
      for (int i = 0; i <= mud; i++) {
        for (int j = 0; j <= mlr; j++) {
          mapOut.writeByte(this.overMap[i][j] & 0xFF);
          mapOut.writeByte(0);
        }
      }

      if (this.hasParallax) {
        // save the parallax map...

        // Determine size of map to save.
        this.pmud = 0;
        this.pmlr = 0;
        for (int i = 0; i < 24; i++) {
          for (int j = 0; j < 40; j++) {
            if (this.paraMap[i][j] > 0) {
              if (i > this.pmud) {
                pmud = i;
              }
              if (j > this.pmlr) {
                pmlr = j;
              }
            }
          }
        }

        System.out.println("Parallax map size: " + pmud + " " + pmlr);

        mapOut.writeByte(this.pmud & 0xFF);
        mapOut.writeByte(0);
        mapOut.writeByte(this.pmlr & 0xFF);
        mapOut.writeByte(0);

        // map data
        for (int i = 0; i <= pmud; i++) {
          for (int j = 0; j <= pmlr; j++) {
            mapOut.writeByte(this.paraMap[i][j] & 0xFF);
            mapOut.writeByte(0);
          }
        }

      }

      mapOut.close();
    } catch (FileNotFoundException e) {
      System.err.println(e); // print exception if the file doesn't exist.
      return;
    } catch (IOException e) {
      System.err.println(e);
      return;
    }
  }

  
  /**
   * Get an image of the map. 
   * 
   * @param tiles         Tiles object to use
   * @param dosGraphics   DosGraphics instance to use for palette
   * @return  image representation of the map
   */
  public BufferedImage getMapImage(Tiles tiles, DosGraphics dosGraphics) {

    // render image

    BufferedImage mapImage = new BufferedImage((this.mlr + 1) * 16,
        (this.mud + 1) * 16, BufferedImage.TYPE_INT_RGB);

    for (int i = 0; i <= this.mud; i++) {
      for (int j = 0; j <= this.mlr; j++) {

        for (int k = 0; k < 16; k++) {
          for (int l = 0; l < 16; l++) {
            int curColor = tiles.getTiles()[this.map[i][j]][k][l];
            mapImage.setRGB(j * 16 + l, i * 16 + k, dosGraphics.getPalette()[curColor]);
          }
        }

        if (this.overMap[i][j] > 0) {
          for (int k = 0; k < 16; k++) {
            for (int l = 0; l < 16; l++) {
              int curColor = tiles.getTiles()[this.overMap[i][j]][k][l];
              if (curColor != 255) {
                mapImage
                    .setRGB(j * 16 + l, i * 16 + k, dosGraphics.getPalette()[curColor]);
              }
            }
          }
        }

      }
    }

    return mapImage;

  }

  
  /**
   * Wipe out the contents of the Map.
   * 
   */
  public void erase() {
    mapDesc = "";
    tileFileName = "";
    for (int i = 0; i < 128; i++) {
      for (int j = 0; j < 128; j++) {
        map[i][j] = 0;
        overMap[i][j] = 0;
      }

    }

  }
  
}
