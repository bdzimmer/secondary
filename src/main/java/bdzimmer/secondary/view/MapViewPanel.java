// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Separating out map view so that I can use the map view for other things
// 2014-08-16

package bdzimmer.secondary.view;

import bdzimmer.secondary.model.DosGraphics;
import bdzimmer.secondary.model.Map;
import bdzimmer.secondary.model.Tiles;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JPanel;


public class MapViewPanel extends JPanel {

  private static final long serialVersionUID = 1L;

  private DosGraphics dosGraphics;

  private Tiles tileSet;
  private int[][] rgbPalette;

  private boolean parallaxEdit = false;

  private boolean dispOver = true;
  private boolean dispBack = true;
  private boolean dispBounds = false;

  private Map map;

  private int numVerticalTiles = 12;
  private int numHorizontalTiles = 20;

  public int vud = 0;
  public int vlr = 0;
  public int scale = 3;

  /**
   * Create a new map view.
   * 
   * @param map           map for the view
   * @param tileSet       tiles to use when displaying the map
   * @param rgbPalette    palette for displaying the map
   */
  public MapViewPanel(Map map, Tiles tileSet, int[][] rgbPalette) {

    this.map = map;
    this.tileSet = tileSet;
    this.rgbPalette = rgbPalette;

    this.dosGraphics = new DosGraphics(192, 320, this.scale); 
    this.dosGraphics.setRgbPalette(this.rgbPalette);
    // this.dosGraphics.updateClut();
    this.add(dosGraphics, BorderLayout.SOUTH);

  }

  // ## --------------------------------------------

  /**
   * Update the DosGraphics instance (used when changing scale).
   */
  public void updateGraphics() {

    this.remove(this.dosGraphics);
    this.dosGraphics = new DosGraphics(this.numVerticalTiles * 16,
        this.numHorizontalTiles * 16, this.scale);

    this.dosGraphics.setRgbPalette(this.rgbPalette);
    this.dosGraphics.updateClut();
    this.add(this.dosGraphics);

  }

  private void drawMap() {
    if (this.tileSet != null) {
      // Draw map on screen.
  
      if (this.parallaxEdit) {

        for (int i = 0; i < 12; i++) {
          for (int j = 0; j < 20; j++) {
            int curTile;
            if (((vud + i) <= 23) && ((vud + i) >= 0) && ((vlr + j) <= 39)
                && ((vlr + j) >= 0)) {
              curTile = map.paraMap[i + vud][j + vlr];
            } else {
              curTile = 0;
            }
            this.dosGraphics.drawTile(this.tileSet.getTiles()[curTile], i
                * this.tileSet.attrs.height, j * this.tileSet.attrs.width);

          }
        }

      } else {

        for (int i = 0; i < 12; i++) {
          for (int j = 0; j < 20; j++) {
            if (this.dispBack) {
              int curTile;
              if (((vud + i) <= 127) && ((vud + i) >= 0) && ((vlr + j) <= 127)
                  && ((vlr + j) >= 0)) {
                curTile = map.map[i + vud][j + vlr];
              } else {
                curTile = 0;
              }
              this.dosGraphics.drawTile(this.tileSet.getTiles()[curTile], i
                  * this.tileSet.attrs.height, j * this.tileSet.attrs.width);
            } else {
              this.dosGraphics.drawTile(
                  new int[this.tileSet.attrs.height][this.tileSet.attrs.width],
                  i * this.tileSet.attrs.height, j * this.tileSet.attrs.width);
            }
          }
        }
        
        if (this.dispOver) {
          for (int i = 0; i < 12; i++) {
            for (int j = 0; j < 20; j++) {
              int curTile;
              if (((vud + i) <= 127) && ((vud + i) >= 0) && ((vlr + j) <= 127)
                  && ((vlr + j) >= 0)) {
                curTile = map.overMap[i + vud][j + vlr];
              } else {
                curTile = 0;
              } 
              if (curTile > 0) {
                this.dosGraphics
                    .drawTileTrans(this.tileSet.getTiles()[curTile], i
                        * this.tileSet.attrs.height, j
                        * this.tileSet.attrs.width);
              }
            }
          }
        }

      }

    }
  }

  /**
   * Paint the component. Draws the map, updates the DosGraphics CLUT, and repaints
   * the DosGraphics. Also draws bounds and tile properties on top.
   */
  public void paint(Graphics graphics) {

    // if you forget this....horrible flickering
    super.paint(graphics);

    this.drawMap();

    System.out.println("Drew map");

    this.dosGraphics.updateClut();

    System.out.println("updated clut");

    this.dosGraphics.repaint();

    Graphics dgGraphics = this.dosGraphics.getBuffer().getGraphics();

    System.out.println("got graphics");

    // Bounds drawing...
    if (this.dispBounds) {
      if (this.tileSet != null) {
        
        dgGraphics.setColor(new Color(dosGraphics.getPalette()[255]));
        for (int i = 0; i < 12; i++) {
          for (int j = 0; j < 20; j++) {
            if (this.dispBack) {
              int curTile;
              if (((vud + i) <= 127) && ((vud + i) >= 0) && ((vlr + j) <= 127)
                  && ((vlr + j) >= 0)) {
                curTile = map.map[i + vud][j + vlr];
              } else {
                curTile = 0;
              }
              if ((this.tileSet.getTileProps()[curTile] & 1) == 0) {
                dgGraphics.drawLine(j * this.tileSet.attrs.width * this.scale, i
                    * this.tileSet.attrs.height * this.scale
                    + this.tileSet.attrs.height * this.scale - 1, j
                    * this.tileSet.attrs.width * this.scale
                    + this.tileSet.attrs.width * this.scale - 1, i
                    * this.tileSet.attrs.height * this.scale
                    + this.tileSet.attrs.height * this.scale - 1);
              }
              if ((this.tileSet.getTileProps()[curTile] & 2) == 0) {
                dgGraphics.drawLine(j * this.tileSet.attrs.width * this.scale, i
                    * this.tileSet.attrs.height * this.scale, j
                    * this.tileSet.attrs.width * this.scale, i
                    * this.tileSet.attrs.height * this.scale
                    + this.tileSet.attrs.height * this.scale - 1);
              }
              if ((this.tileSet.getTileProps()[curTile] & 4) == 0) {
                dgGraphics.drawLine(j * this.tileSet.attrs.width * this.scale
                    + this.tileSet.attrs.width * this.scale - 1, i
                    * this.tileSet.attrs.height * this.scale, j
                    * this.tileSet.attrs.width * this.scale
                    + this.tileSet.attrs.width * this.scale - 1, i
                    * this.tileSet.attrs.height * this.scale
                    + this.tileSet.attrs.height * this.scale - 1);
              }
              if ((this.tileSet.getTileProps()[curTile] & 8) == 0) {
                dgGraphics.drawLine(j * this.tileSet.attrs.width * this.scale, i
                    * this.tileSet.attrs.height * this.scale, j
                    * this.tileSet.attrs.width * this.scale
                    + this.tileSet.attrs.width * this.scale - 1, i
                    * this.tileSet.attrs.height * this.scale);
              }
              if ((this.tileSet.getTileProps()[curTile] & 16) != 0) {
                dgGraphics.drawLine(j * this.tileSet.attrs.width * this.scale, i
                    * this.tileSet.attrs.height * this.scale, j
                    * this.tileSet.attrs.width * this.scale
                    + this.tileSet.attrs.width * this.scale - 1, i
                    * this.tileSet.attrs.height * this.scale
                    + this.tileSet.attrs.height * this.scale - 1);
              }

            }
          }
        }
        dgGraphics.setColor(new Color(dosGraphics.getPalette()[10]));
        if (this.dispOver) {
          for (int i = 0; i < 12; i++) {
            for (int j = 0; j < 20; j++) {
              int curTile;
              if (((vud + i) <= 127) && ((vud + i) >= 0) && ((vlr + j) <= 127)
                  && ((vlr + j) >= 0)) {
                curTile = map.overMap[i + vud][j + vlr];
              } else {
                curTile = 0;
              }
              if ((this.tileSet.getTileProps()[curTile] & 1) == 0) {
                dgGraphics.drawLine(j * this.tileSet.attrs.width * this.scale, i
                    * this.tileSet.attrs.height * this.scale
                    + this.tileSet.attrs.height * this.scale - 1, j
                    * this.tileSet.attrs.width * this.scale
                    + this.tileSet.attrs.width * this.scale - 1, i
                    * this.tileSet.attrs.height * this.scale
                    + this.tileSet.attrs.height * this.scale - 1);
              }
              if ((this.tileSet.getTileProps()[curTile] & 2) == 0) {
                dgGraphics.drawLine(j * this.tileSet.attrs.width * this.scale, i
                    * this.tileSet.attrs.height * this.scale, j
                    * this.tileSet.attrs.width * this.scale, i
                    * this.tileSet.attrs.height * this.scale
                    + this.tileSet.attrs.height * this.scale - 1);
              }
              if ((this.tileSet.getTileProps()[curTile] & 4) == 0) {
                dgGraphics.drawLine(j * this.tileSet.attrs.width * this.scale
                    + this.tileSet.attrs.width * this.scale - 1, i
                    * this.tileSet.attrs.height * this.scale, j
                    * this.tileSet.attrs.width * this.scale
                    + this.tileSet.attrs.width * this.scale - 1, i
                    * this.tileSet.attrs.height * this.scale
                    + this.tileSet.attrs.height * this.scale - 1);
              }
              if ((this.tileSet.getTileProps()[curTile] & 8) == 0) {
                dgGraphics.drawLine(j * this.tileSet.attrs.width * this.scale, i
                    * this.tileSet.attrs.height * this.scale, j
                    * this.tileSet.attrs.width * this.scale
                    + this.tileSet.attrs.width * this.scale - 1, i
                    * this.tileSet.attrs.height * this.scale);
              }
              if ((this.tileSet.getTileProps()[curTile] & 16) != 0) {
                dgGraphics.drawLine(j * this.tileSet.attrs.width * this.scale, i
                    * this.tileSet.attrs.height * this.scale, j
                    * this.tileSet.attrs.width * this.scale
                    + this.tileSet.attrs.width * this.scale - 1, i
                    * this.tileSet.attrs.height * this.scale
                    + this.tileSet.attrs.height * this.scale - 1);
              }

            }
          }
        }

      }

    }

  }

  // ###--------Getters and setters---------------

  public void setMap(Map map) {
    this.map = map;
  }
  
  
  
  public int getScale() {
    return this.scale;
  }

  public int getNumVerticalTiles() {
    return numVerticalTiles;
  }

  public void setNumVerticalTiles(int numVerticalTiles) {
    this.numVerticalTiles = numVerticalTiles;
  }

  public int getNumHorizontalTiles() {
    return numHorizontalTiles;
  }

  public void setNumHorizontalTiles(int numHorizontalTiles) {
    this.numHorizontalTiles = numHorizontalTiles;
  }

  public boolean isDispBack() {
    return dispBack;
  }

  public void setDispBack(boolean dispBack) {
    this.dispBack = dispBack;
  }

  public void setDispOver(boolean dispOver) {
    this.dispOver = dispOver;
  }

  public boolean isDispBounds() {
    return dispBounds;
  }

  public void setDispBounds(boolean dispBounds) {
    this.dispBounds = dispBounds;
  }

  public boolean isDispOver() {
    return dispOver;
  }

  public boolean isParallaxEdit() {
    return parallaxEdit;
  }

  public void setParallaxEdit(boolean parallaxEdit) {
    this.parallaxEdit = parallaxEdit;
  }

}
