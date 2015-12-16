// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// 1-8-13: Separated tileset model data fields into TileSet class.

package bdzimmer.secondary.editor.view;

import bdzimmer.secondary.editor.model.DosGraphics;
import bdzimmer.secondary.editor.model.TileOptions;
import bdzimmer.secondary.editor.model.Tiles;

import java.awt.BorderLayout;
import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;

public class TilesEditorWindow extends JFrame {
  
  private static final long serialVersionUID = 0; // Meaningless junk.
  
  private final String tilesDir;
  private Tiles tileSet;
  public String tileFileName;
  private final String title;

  public DosGraphics dosGraphics;
  private PaletteWindow paletteWindow;
  private ZoomedTileWindow zoomWindow;

  private JPanel graphicsPanel = new JPanel();
  private StatusBar myStatusBar = new StatusBar();

  private int currentTile;
  private int scale = 3;

  
  /**
   * Create a TilesEditorWindow.
   * 
   * @param tilesDir            tiles directory
   * @param tiles               tile set to edit in the window
   * @param title               title for window
   * @param fileName            file name of tiles (for save menu option)
   * @param paletteWindow       palette window to edit 
   */
  public TilesEditorWindow(
      String tilesDir,
      Tiles tiles,
      String title,
      String fileName,
      PaletteWindow paletteWindow) { 
    
    this.tilesDir = tilesDir;    
    this.tileSet = tiles;
    this.tileFileName = fileName;
    this.title = title;

    setTitle(generateTitle());
    this.paletteWindow = paletteWindow;

    ////// UI stuff
    
    addFocusListener(new FocusAdapter() {
      public void focusGained(FocusEvent event) {
        repaint();
        // System.out.println("Focus gained!");
      }
    });

    JMenuBar mainMenu = new JMenuBar();
    setJMenuBar(mainMenu);

    // file menu
    JMenu fileMenu = new JMenu("File");

    JMenuItem jmOpen = new JMenuItem("Open");
    JMenuItem jmSave = new JMenuItem("Save");
    JMenuItem jmSaveAs = new JMenuItem("Save As..");
    JMenuItem jmChange = new JMenuItem("Change settings...");

    fileMenu.add(jmOpen);
    fileMenu.add(jmSave);
    fileMenu.add(jmSaveAs);
    fileMenu.addSeparator();
    fileMenu.add(jmChange);
    mainMenu.add(fileMenu);

    // tools menu 6-17-10

    JMenu toolsMenu = new JMenu("Tools");

    JMenuItem jmSwap = new JMenuItem("Swap transparency");
    JMenuItem jmBlacken = new JMenuItem("Blacken");

    toolsMenu.add(jmSwap);
    toolsMenu.add(jmBlacken);
    mainMenu.add(toolsMenu);

    this.getContentPane().addMouseListener(new MouseAdapter() {
      public void mouseClicked(MouseEvent event) {
        System.out.println("Clicked -- " + event.getX() + " " + event.getY());
        handleClicks(event);
      }
    });

    jmOpen.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        chooseLoadTileset();
      }
    });

    jmSave.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        tileSet.save(new File(tileFileName), dosGraphics); 
      }
    });

    jmSaveAs.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        chooseSaveTileset();
      }
    });

    jmChange.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        changeTiles();
      }
    });

    jmSwap.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        swapTransparency();
        repaint();
      }
    });

    jmBlacken.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        blacken();
        repaint();

      }
    });

    // Set the layout manager.

    // setLayout(new FlowLayout(FlowLayout.LEFT, 0,0));
    

    dosGraphics = createDosGraphics();
    
    dosGraphics.setRgbPalette(paletteWindow.getDosGraphics().getRgbPalette());
    graphicsPanel.add(dosGraphics);

    this.add(graphicsPanel, BorderLayout.NORTH);
    this.add(myStatusBar, BorderLayout.SOUTH);

    pack();
    this.setResizable(false);
    setVisible(true);

  }

  // create an appropriately sized scaled DosGraphics for the tileset
  private DosGraphics createDosGraphics() {
    return new DosGraphics(
        (int)Math.ceil((float)tileSet.attrs.count / tileSet.attrs.tilesPerRow) * this.tileSet.attrs.height,
        this.tileSet.attrs.tilesPerRow * this.tileSet.attrs.width,
        this.scale);
  }
  
  
  
  private void handleClicks(MouseEvent event) {
    
    int selectedTile = 
        (int)(event.getY() / (tileSet.attrs.height * scale)) * tileSet.attrs.tilesPerRow
        + (int)(event.getX() / (tileSet.attrs.width * scale));

    if (selectedTile > tileSet.attrs.count) {
      selectedTile = tileSet.attrs.count;
    }
    
    if (event.isMetaDown()) {
      // left click -- set tile in window
      this.currentTile = selectedTile;
      
      Main.currentTile = this.currentTile;
      Main.currentTileBitmap = this.tileSet.getTiles()[this.currentTile];

      if (this.zoomWindow == null || !this.zoomWindow.isVisible()) {
        this.zoomWindow = new ZoomedTileWindow(
            "Zoom",
            this.tileSet.getTiles()[currentTile],
            this.dosGraphics.getRgbPalette());
        this.zoomWindow.setTileWindow(this);
        this.zoomWindow.setLocationRelativeTo(this);

      } else {
        this.zoomWindow.setTile(this.tileSet.getTiles(), currentTile);
      }

    } else {
      
      int newTile = selectedTile;

      // Calculate maximum size we can copy...
      int udlength;
      int lrlength;
      if (this.tileSet.attrs.height > Main.currentTileBitmap.length) {
        udlength = Main.currentTileBitmap.length;
      } else {
        udlength = this.tileSet.attrs.height;
      }
      if (this.tileSet.attrs.width > Main.currentTileBitmap[0].length) {
        lrlength = Main.currentTileBitmap[0].length;
      } else {
        lrlength = this.tileSet.attrs.width;
      }

      for (int i = 0; i < udlength; i++) {
        for (int j = 0; j < lrlength; j++) {
          this.tileSet.getTiles()[newTile][i][j] = Main.currentTileBitmap[i][j];
        }
      }

      this.repaint();
    }
    this.myStatusBar.update(0, 0, "" + currentTile);

  }

  // convert all color indices that are RGB(0, 0, 0) to 0
  private void blacken() {

    boolean[] blackColors = new boolean[256];

    // determine which colors are equiv to black
    for (int i = 0; i < 256; i++) {
      if (this.dosGraphics.getRgbPalette()[i][0] == 0 
          && this.dosGraphics.getRgbPalette()[i][1] == 0
          && this.dosGraphics.getRgbPalette()[i][2] == 0) {
        blackColors[i] = true;
      }
    }

    for (int i = 0; i < this.tileSet.getTiles().length; i++) {
      for (int j = 0; j < this.tileSet.getTiles()[0].length; j++) {
        for (int k = 0; k < this.tileSet.getTiles()[0][0].length; k++) {
          if (blackColors[this.tileSet.getTiles()[i][j][k]]) {
            // temporarily...
            this.tileSet.getTiles()[i][j][k] = 0;
          }
        }
      }
    }

  }
  
  // swap colors 0 and 255
  private void swapTransparency() {
    for (int i = 0; i < tileSet.tiles.length; i++) {
      for (int j = 0; j < tileSet.tiles[0].length; j++) {
        for (int k = 0; k < tileSet.tiles[0][0].length; k++) {
          int tempColor = tileSet.tiles[i][j][k];
          if (tempColor == 0) {
            tileSet.tiles[i][j][k] = 255;
          }
          if (tempColor == 255) {
            tileSet.tiles[i][j][k] = 0;
          }
        }
      }
    }
  }

  /**
   * Show a tile attributes chooser, change the tile set type,
   * resize, and redraw the window.
   */
  public void changeTiles() {

    this.tileSet = new Tiles(TileOptions.getOptions());

    int[][] rgbPalette = this.dosGraphics.getRgbPalette();

    graphicsPanel.remove(dosGraphics);
    dosGraphics = createDosGraphics();
    dosGraphics.setRgbPalette(rgbPalette);

    graphicsPanel.add(dosGraphics);
    this.pack();
    this.repaint();
    this.dosGraphics.repaint();

  }

  public void setTilePropsFlag(boolean tilePropsFlag) {
    this.tileSet.attrs.tileProperties = tilePropsFlag;
  }

  /**
   * Select a tiles file to load with a file chooser, then load it
   * and redraw.
   * 
   */
  public void chooseLoadTileset() {
    JFileChooser jfc = new JFileChooser();
    jfc.setDialogType(JFileChooser.OPEN_DIALOG);
    jfc.setCurrentDirectory(new File(tilesDir));     
    if (jfc.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) { 
      
      File tilesFile = jfc.getSelectedFile();
      try {
        tileSet = new Tiles(this.tileSet.attrs, tilesFile, this.dosGraphics.getRgbPalette());
        setTitle(generateTitle());
        dosGraphics.updateClut();
        tileFileName = tilesFile.getAbsolutePath(); 
        paletteWindow.refreshPalette();
        repaint();
      } catch (NullPointerException e) {
        System.err.println(e);
        return;
      }
    }
  }

  /**
   * Select a tiles file to save with a file chooser, then save it.
   */
  public void chooseSaveTileset() {
    JFileChooser jfc = new JFileChooser();
    jfc.setDialogType(JFileChooser.SAVE_DIALOG);
    jfc.setCurrentDirectory(new File(tilesDir));
    if (jfc.showSaveDialog(null) == JFileChooser.APPROVE_OPTION) { 
      
      File tilesFile = jfc.getSelectedFile();
      try {
        // getSelectedFile returns the file that was selected
        this.tileSet.save(tilesFile, this.dosGraphics); 
        repaint();
      } catch (NullPointerException e) {
        System.err.println(e);
        return;
      }
    }
  }

  public Tiles getTileSet() {
    return this.tileSet;
  }

  public DosGraphics getDosGraphics() {
    return this.dosGraphics;
  }
  
  // generate a title
  private String generateTitle() {
    return title + " - " + this.tileFileName;  
  }

  /**
   * Draw the component.
   */
  @Override
  public void paint(Graphics graphics) {
    super.paint(graphics);

    dosGraphics.updateClut();
    dosGraphics.drawTileset(tileSet);
    dosGraphics.repaint();

  }

}
