// Copyright (c) 2015 Ben Zimmer. All rights reserved.

//Class for drawing a tile or sprite zoomed.

package bdzimmer.secondary.editor.view;

import bdzimmer.secondary.editor.controller.FloodFill;
import bdzimmer.secondary.editor.model.DosGraphics;
import bdzimmer.secondary.editor.model.TileProperties;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JToolBar;


public class ZoomedTileWindow extends JFrame {
  private static final long serialVersionUID = 1L;
  private int tileHeight;
  private int tileWidth;
  private int zoomFactor;
  private int[][] tile;
  private int currentTile;
  private int[][] overlayTile;
  private int penMode;

  private int[][] rgbPalette;

  private DosGraphics dosGraphics;
  private DosGraphics tileTile;
  private JPanel graphicsPanel = new JPanel();

  private TilesEditorWindow tileWindow;
  private JButton tpTop;
  private JButton tpBottom;
  private JButton tpLeft;
  private JButton tpRight;
  private JButton tpOverlay;
  private JButton tpLeftStair;
  private JButton tpRightStair;
  private JButton togglePen;

  /**
   * Create a new ZoomedTileWindow.
   * 
   * @param title         title for window
   * @param tile          tile to display and edit
   * @param rgbPalette    rgb palette
   */
  public ZoomedTileWindow(String title, int[][] tile, int[][] rgbPalette) { // Constructor
    this.zoomFactor = 8;
    if (tile != null) {
      this.tileHeight = tile.length;
      this.tileWidth = tile[0].length;
      this.dosGraphics = new DosGraphics(tileHeight * zoomFactor, tileWidth
          * zoomFactor, 2);
      tileTile = new DosGraphics(tileHeight * 3, tileWidth * 3, 2);
    } else {
      dosGraphics = new DosGraphics(64, 64, 2); // call normal DosGraphics
                                                  // constructor
      tileTile = new DosGraphics(64, 64, 2);
    }

    this.rgbPalette = rgbPalette;
    this.tileTile.setRgbPalette(rgbPalette);
    this.tile = tile;

    this.setLayout(new BorderLayout(0, 0));

    graphicsPanel.add(dosGraphics, BorderLayout.SOUTH);
    this.add(graphicsPanel);

    this.getContentPane().addMouseMotionListener(new MouseMotionListener() {

      public void mouseDragged(MouseEvent event) {
        handleClicks(event, 3);
      }

      @Override
      public void mouseMoved(MouseEvent arg0) {
        // Do nothing.
      }

    });

    this.getContentPane().addMouseListener(new MouseListener() {
      @Override
      public void mouseClicked(MouseEvent event) {
        // Do nothing.

      }

      @Override
      public void mouseEntered(MouseEvent event) {
        // Do nothing.

      }

      @Override
      public void mouseExited(MouseEvent event) {
        // Do nothing.

      }

      @Override
      public void mousePressed(MouseEvent event) {
        handleClicks(event, 3);
      }

      @Override
      public void mouseReleased(MouseEvent event) {
        tileWindow.repaint(); // Only repaint the tileWindow when the mouse is
                              // released
      }

    });

    JPanel layoutPanel = new JPanel();
    layoutPanel.setLayout(new GridLayout(3, 1, 0, 0)); // for now

    // JPanel bP = new JPanel();
    JToolBar buttonsToolBar = new JToolBar();
    buttonsToolBar.setLayout(new GridLayout(4, 4, 0, 0)); // for now

    // Zoom in and out buttons
    JButton zoomIn = new JButton("+");
    buttonsToolBar.add(zoomIn);
    JButton zoomOut = new JButton("-");
    buttonsToolBar.add(zoomOut);

    // Lighten
    JButton lighten = new JButton("Lighten");
    lighten.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {

        handleTools(event);
      }

    });
    buttonsToolBar.add(lighten);

    // Darken
    JButton darken = new JButton("Darken");
    darken.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        handleTools(event);
      }

    });
    buttonsToolBar.add(darken);

    // Set Overlay Tile
    JButton setOverlay = new JButton("Overlay");
    setOverlay.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        handleTools(event);
      }

    });
    buttonsToolBar.add(setOverlay);

    // Fill
    JButton fill = new JButton("Fill");
    fill.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        handleTools(event);
      }

    });
    buttonsToolBar.add(fill);

    // Toggle Pen Mode
    togglePen = new JButton("Norm. Pen");
    togglePen.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {

        penMode++;
        if (penMode == 3) {
          penMode = 0;
        }

        if (penMode == 0) {
          togglePen.setText("Norm. Pen");
        } else if (penMode == 1) {
          togglePen.setText("Ovl. Pen");
        } else if (penMode == 2) {
          togglePen.setText("Fill Pen");
        }
        repaint();

      }

    });
    buttonsToolBar.add(togglePen);

    // Flip horizontal
    JButton flipHoriz = new JButton("Flip >");
    flipHoriz.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        handleTools(event);
      }

    });
    buttonsToolBar.add(flipHoriz);

    // Flip vertical
    JButton flipVert = new JButton("Flip ^");
    flipVert.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        handleTools(event);
      }

    });
    buttonsToolBar.add(flipVert);

    // Shift horizontal
    JButton shiftHoriz = new JButton("Shift >");
    shiftHoriz.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        handleTools(event);
      }

    });
    buttonsToolBar.add(shiftHoriz);

    // Shift vertical
    JButton shiftVert = new JButton("Shift ^");
    shiftVert.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        handleTools(event);
      }

    });
    buttonsToolBar.add(shiftVert);

    JPanel tpP = new JPanel();

    tpP.setLayout(new GridLayout(3, 3, 0, 0)); // for now

    tpTop = new JButton();
    tpBottom = new JButton();
    tpLeft = new JButton();
    tpRight = new JButton();
    tpOverlay = new JButton();
    tpLeftStair = new JButton();
    tpRightStair = new JButton();

    tpP.add(new JLabel(" "));
    tpP.add(tpTop);
    tpP.add(new JLabel(" "));
    tpP.add(tpRight);
    tpP.add(tpOverlay);
    tpP.add(tpLeft);
    tpP.add(tpLeftStair);
    tpP.add(tpBottom);
    tpP.add(tpRightStair);
    tpP.setPreferredSize(new Dimension(128, 128));

    layoutPanel.add(tpP);
    layoutPanel.add(buttonsToolBar);
    layoutPanel.add(tileTile);

    this.add(layoutPanel, BorderLayout.SOUTH);

    tpBottom.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        int prop = tileWindow.getTileSet().properties()[currentTile].value();
        tileWindow.getTileSet().properties()[currentTile] = new TileProperties(prop ^ 1);
        updateTileProps();
      }

    });

    tpRight.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        int prop = tileWindow.getTileSet().properties()[currentTile].value();
        tileWindow.getTileSet().properties()[currentTile] = new TileProperties(prop ^ 2);
        updateTileProps();
      }

    });

    tpLeft.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        int prop = tileWindow.getTileSet().properties()[currentTile].value();
        tileWindow.getTileSet().properties()[currentTile] = new TileProperties(prop ^ 4);
        updateTileProps();
      }

    });

    tpTop.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        int prop = tileWindow.getTileSet().properties()[currentTile].value();
        tileWindow.getTileSet().properties()[currentTile] = new TileProperties(prop ^ 8);
        updateTileProps();
      }

    });

    tpOverlay.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        int prop = tileWindow.getTileSet().properties()[currentTile].value();
        tileWindow.getTileSet().properties()[currentTile] = new TileProperties(prop ^ 16);
        updateTileProps();
      }

    });

    tpLeftStair.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        int prop = tileWindow.getTileSet().properties()[currentTile].value();
        tileWindow.getTileSet().properties()[currentTile] = new TileProperties(prop ^ 32);
        updateTileProps();
      }

    });

    tpRightStair.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        int prop = tileWindow.getTileSet().properties()[currentTile].value();
        tileWindow.getTileSet().properties()[currentTile] = new TileProperties(prop ^ 64);
        updateTileProps();
      }

    });

    this.pack();
    this.setTitle(title);
    this.setVisible(true);
    this.repaint();

    zoomIn.addActionListener(new ActionListener() { // Anonymous Listener.
          public void actionPerformed(ActionEvent event) {
            System.out.println("Zooming in.");
            setZoomFactor(zoomFactor + 1);

          }

        });

    zoomOut.addActionListener(new ActionListener() { // Anonymous Listener.
          public void actionPerformed(ActionEvent event) {
            System.out.println("Zooming out.");
            setZoomFactor(zoomFactor - 1);
          }

        });

    this.setResizable(false);
    this.updateGraphics();
  }

  private void handleTools(ActionEvent event) {
    String commandString = event.getActionCommand();

    if ("Lighten".equals(commandString)) {
      for (int i = 0; i < tile.length; i++) {
        for (int j = 0; j < tile[0].length; j++) {
          int curColor = tile[i][j];
          if (curColor != 0 && curColor != 255) {
            curColor++;
            if (curColor == 255) {
              curColor = 1;
            }
            tile[i][j] = curColor;
          }
        }
      }

    } else if ("Darken".equals(commandString)) {
      for (int i = 0; i < tile.length; i++) {
        for (int j = 0; j < tile[0].length; j++) {
          int curColor = tile[i][j];
          if (curColor != 0 && curColor != 255) {
            curColor--;
            if (curColor == 0) {
              curColor = 254;
            }
            tile[i][j] = curColor;
          }
        }
      }

    } else if ("Overlay".equals(commandString)) {
      this.overlayTile = tile;

    } else if ("Fill".equals(commandString)) {
      for (int i = 0; i < tile.length; i++) {
        for (int j = 0; j < tile[0].length; j++) {
          tile[i][j] = Main.currentColor;
        }
      }

    } else if ("Flip >".equals(commandString)) {
      int[][] tempTile = copyTile();
      for (int i = 0; i < tile.length; i++) {
        for (int j = 0; j < tile[0].length; j++) {
          tile[i][tile[0].length - j - 1] = tempTile[i][j];
        }
      }

    } else if ("Flip ^".equals(commandString)) {
      int[][] tempTile = copyTile();
      for (int i = 0; i < tile.length; i++) {
        for (int j = 0; j < tile[0].length; j++) {
          tile[tile.length - i - 1][j] = tempTile[i][j];
        }
      }

    } else if ("Shift ^".equals(commandString)) {
      int[][] tempTile = copyTile();
      for (int j = 0; j < tile[0].length; j++) {
        tile[0][j] = tempTile[tile.length - 1][j];
      }
      for (int i = 1; i < tile.length; i++) {
        for (int j = 0; j < tile[0].length; j++) {
          tile[i][j] = tempTile[i - 1][j];
        }
      }

    } else if ("Shift >".equals(commandString)) {
      int[][] tempTile = copyTile();
      for (int i = 0; i < tile.length; i++) {
        for (int j = 0; j < tile[0].length - 1; j++) {
          tile[i][j] = tempTile[i][j + 1];
        }
        tile[i][tile[0].length - 1] = tempTile[i][0];
      }
    }

    this.repaint();
    this.tileWindow.repaint();
  }

  
  private int[][] copyTile() {
    int[][] tempTile = new int[this.tile.length][this.tile[0].length];
    for (int i = 0; i < this.tile.length; i++) {
      for (int j = 0; j < this.tile[0].length; j++) {
        tempTile[i][j] = tile[i][j];
      }
    }
    return tempTile;
  }

  private void handleClicks(MouseEvent event, int whichWindow) {
    // evidently this is more proper than a switch thingy

    if (whichWindow == 3) { // zoom window(
      int tud = (int) ((event.getY() - dosGraphics.getY()) / zoomFactor / 2);
      int tlr = (int) ((event.getX() - dosGraphics.getX()) / zoomFactor / 2);

      // System.out.println("In ZoomWindow -- " + tud + " " + tlr);

      if (tud < this.tile.length && tlr < this.tile[0].length) {
        if (!event.isMetaDown()) { // right click
          if (this.penMode == 0) { // normal pen
            this.tile[tud][tlr] = Main.currentColor;
            System.out.println("Set color " + Main.currentColor);
          } else if (this.penMode == 1) {
            this.tile[tud][tlr] = this.overlayTile[tud][tlr];
          } else if (this.penMode == 2) {
            FloodFill.floodFill(this.tile, Main.currentColor, tud, tlr);
          }
        } else {
          Main.currentColor = tile[tud][tlr];
          Main.paletteWindow.updateSpinners();
          Main.paletteWindow.repaint();
          System.out.println("Got color " + Main.currentColor);
        }
      }
      // this.tileWindow.repaint(); // Only repaint the tileWindow when the
      // mouse is released
      this.repaint();
    }

  }

  /**
   * Redraw the zoomed tile.
   */
  public void drawTile() {
    if (this.tile != null) {
      for (int i = 0; i < tile.length; i++) {
        for (int j = 0; j < tile[0].length; j++) {
          for (int k = 0; k < zoomFactor; k++) {
            for (int l = 0; l < zoomFactor; l++) {
              dosGraphics.setPixel(i * zoomFactor + k, j * zoomFactor + l, tile[i][j]);
            }
          }
        }
      }
      dosGraphics.repaint();
    }
  }


  /**
   * Set the tile to view and edit.
   * 
   * @param tileSet       array describing tile set
   * @param whichTile     which tile in the tile set
   */
  public void setTile(int[][] tile, int whichTile) {
    this.tile = tile;
    currentTile = whichTile;
    this.tileHeight = tile.length;
    this.tileWidth = tile[0].length;
    updateGraphics();
  }

  
  /**
   * Set the tile to view and edit.
   * 
   * @param tempTile      array describing the tile
   */
  public void setTile(int[][] tempTile) {
    tile = tempTile;
    this.tileHeight = tile.length;
    this.tileWidth = tile[0].length;
    updateGraphics();
  }

  public int[][] getTile() {
    return tile;
  }

  public void setTileWindow(TilesEditorWindow tileWindow) {
    this.tileWindow = tileWindow;
  }

  public int getZoomFactor() {
    return zoomFactor;
  }

  public void setZoomFactor(int newZoomFactor) {
    zoomFactor = newZoomFactor;
    updateGraphics();
  }

  public DosGraphics getDosGraphics() {
    return dosGraphics;
  }

  private void updateGraphics() {
    graphicsPanel.remove(dosGraphics);
    if (tile != null) {
      tileHeight = tile.length;
      tileWidth = tile[0].length;
    }
    dosGraphics = new DosGraphics(
        tileHeight * zoomFactor, tileWidth * zoomFactor, 2);
    dosGraphics.setRgbPalette(this.rgbPalette);
    graphicsPanel.add(dosGraphics);
    this.pack();
    this.repaint();
    dosGraphics.repaint();
  }

  private void updateTileProps() {
    int tpb = 0;
    if (this.tileWindow.getTileSet().properties().length > 0) {
      tpb = this.tileWindow.getTileSet().properties()[currentTile].value();
    }

    Color onColor = new Color(128, 0, 128);
    Color offColor = new Color(0, 0, 0);
    
    if ((tpb & 1) == 0) {
      tpBottom.setBackground(onColor);
    } else {
      tpBottom.setBackground(offColor);
    }
    if ((tpb & 2) == 0) {
      tpRight.setBackground(onColor);
    } else {
      tpRight.setBackground(offColor);
    }
    if ((tpb & 4) == 0) {
      tpLeft.setBackground(onColor);
    } else {
      tpLeft.setBackground(offColor);
    }
    if ((tpb & 8) == 0) {
      tpTop.setBackground(onColor);
    } else {
      tpTop.setBackground(offColor);
    }
    if ((tpb & 16) > 0) {
      tpOverlay.setBackground(onColor);
    } else {
      tpOverlay.setBackground(offColor);
    }
    if ((tpb & 32) > 0) {
      tpLeftStair.setBackground(onColor);
    } else {
      tpLeftStair.setBackground(offColor);
    }
    if ((tpb & 64) > 0) {
      tpRightStair.setBackground(onColor);
    } else {
      tpRightStair.setBackground(offColor);
    }

  }

  
  /**
   * Draw the component.
   */
  public void paint(Graphics gr) {
    super.paint(gr);

    dosGraphics.updateClut();
    this.tileTile.updateClut();

    this.drawTile();

    // Draw the repeated tile for tiling purposes
    if (this.tileHeight == 16) {
      for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
          tileTile.drawTile(this.tile, i * this.tileHeight, j * this.tileWidth);
        }
      }
    }
    // draw the tileprops.
    this.updateTileProps();
  }

}
