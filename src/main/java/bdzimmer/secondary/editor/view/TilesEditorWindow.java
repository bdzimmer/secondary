// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Modified around 2015-12-17 for new Tileset class. A lot of the code
// in here should be moved to controller classes.

package bdzimmer.secondary.editor.view;

import bdzimmer.secondary.editor.controller.OldTilesetLoader;
import bdzimmer.secondary.editor.model.DosGraphics;
import bdzimmer.secondary.editor.model.Palette;
import bdzimmer.secondary.editor.model.TileOptions;
import bdzimmer.secondary.editor.model.Tileset;
import bdzimmer.secondary.editor.model.TileAttributes;

import java.awt.BorderLayout;
import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;



public class TilesEditorWindow extends JFrame {
  
  private static final long serialVersionUID = 0;
  
  private final String tilesDir;
  private final String title;
  
  private final StatusBar statusBar = new StatusBar();
  private final int scale = 3;
  
  private Tileset tileset;
  private TileAttributes attrs;
  private String tileFilename;

  private DosGraphics dosGraphics;
  private PaletteWindow paletteWindow;
  private ZoomedTileWindow zoomWindow;

  private JPanel graphicsPanel = new JPanel();

  private int currentTile;

  
  /**
   * Create a TilesEditorWindow.
   * 
   * @param tilesDir            tiles directory
   * @param tiles               tile set to edit in the window
   * @param attribute           TileAttributes for the tileset
   * @param title               general title for window
   * @param fileName            file name of tiles (for save menu option)
   * @param paletteWindow       palette window to edit 
   */
  public TilesEditorWindow(
      String tilesDir,
      Tileset tiles,
      TileAttributes attributes,
      String title,
      String filename,
      PaletteWindow paletteWindow) { 
    
    this.tilesDir = tilesDir;    
    this.tileset = tiles;
    this.attrs = attributes;
    
    this.tileFilename = filename;
    this.title = title;
    updateTitle();
    
    this.paletteWindow = paletteWindow;
  
    // UI stuff
    
    // redraw on focus gained
    addFocusListener(new FocusAdapter() {
      public void focusGained(FocusEvent event) { repaint(); }
    });
    
    // main menu
    setJMenuBar(mainMenu());
    
    // tileset visualization
    dosGraphics = createDosGraphics();
    dosGraphics.setRgbPalette(paletteWindow.getDosGraphics().getRgbPalette());
    graphicsPanel.add(dosGraphics);
    this.add(graphicsPanel, BorderLayout.NORTH);
    
    // clicking to select and manipulate tiles
    dosGraphics.addMouseListener(new MouseAdapter() {
      public void mouseClicked(MouseEvent event) { handleClicks(event); }
    });
    
    // status bar
    add(statusBar, BorderLayout.SOUTH);

    pack();
    setResizable(false);
    setVisible(true);

  }

  
  // create an appropriately sized and scaled DosGraphics for the tileset
  private DosGraphics createDosGraphics() {
    DosGraphics dg = new DosGraphics(
        (int)Math.ceil((float)tileset.tiles().length / tileset.tilesPerRow()) * tileset.height(),
        tileset.tilesPerRow() * tileset.width(),
        this.scale);
    
    dg.setGridDimensions(tileset.width(), tileset.height());
    return dg;
  }
  
  
  private void handleClicks(MouseEvent event) {
    
    int selectedTile = 
        (int)(event.getY() / (tileset.height() * scale)) * tileset.tilesPerRow()
        + (int)(event.getX() / (tileset.width() * scale));

    if (selectedTile > tileset.tiles().length) {
      selectedTile = tileset.tiles().length;
    }
    
    if (event.isMetaDown()) {
      // left click -- set tile in window
      
      this.currentTile = selectedTile;  
      Main.currentTile = currentTile;
      Main.currentTileBitmap = tileset.tiles()[currentTile].pixels();

      if (zoomWindow == null || !zoomWindow.isVisible()) {
        zoomWindow = new ZoomedTileWindow(
            "Zoom",
            tileset.tiles()[currentTile].pixels(),
            paletteWindow);
        
        zoomWindow.setTileWindow(this);
        zoomWindow.setLocationRelativeTo(this);
      } else {
        zoomWindow.setTile(tileset.tiles()[currentTile].pixels(), currentTile);
      }
      
      zoomWindow.toFront();

    } else {
      
      int newTile = selectedTile;

      // Calculate maximum size we can copy
      // The global tile bitmap here seems kind of dumb, but it's there to allow
      // copying tiles across tileset -- important functionality. 
      
      int udlength = Math.min(tileset.height(), Main.currentTileBitmap.length);
      int lrlength = Math.min(tileset.width(), Main.currentTileBitmap[0].length);
      
      for (int i = 0; i < udlength; i++) {
        for (int j = 0; j < lrlength; j++) {
          tileset.tiles()[newTile].pixels()[i][j] = Main.currentTileBitmap[i][j];
        }
      }

      // set the copy as the current tile
      this.currentTile = selectedTile;  
      Main.currentTile = currentTile;
      Main.currentTileBitmap = tileset.tiles()[currentTile].pixels();
      
      repaint();
    }
    
    statusBar.update(0, 0, "" + currentTile);

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

    for (int i = 0; i < tileset.tiles().length; i++) {
      for (int j = 0; j < tileset.tiles()[0].pixels().length; j++) {
        for (int k = 0; k < tileset.tiles()[0].pixels()[0].length; k++) {
          if (blackColors[tileset.tiles()[i].pixels()[j][k]]) {
            // temporarily...
            tileset.tiles()[i].pixels()[j][k] = 0;
          }
        }
      }
    }

  }
  
  
  // swap colors 0 and 255
  private void swapTransparency() {
    for (int i = 0; i < tileset.tiles().length; i++) {
      for (int j = 0; j < tileset.tiles()[0].pixels().length; j++) {
        for (int k = 0; k < tileset.tiles()[0].pixels()[0].length; k++) {
          int tempColor = tileset.tiles()[i].pixels()[j][k];
          if (tempColor == 0) {
            tileset.tiles()[i].pixels()[j][k] = 255;
          }
          if (tempColor == 255) {
            tileset.tiles()[i].pixels()[j][k] = 0;
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

    // mutate the TileAttributes and create a new tileset.
    this.attrs = TileOptions.getOptions();
    this.tileset = OldTilesetLoader.fromAttributes(this.attrs);

    // create a new DosGraphics instance, but keep the old palette
    int[][] rgbPalette = this.dosGraphics.getRgbPalette();
    graphicsPanel.remove(dosGraphics);
    dosGraphics = createDosGraphics();
    dosGraphics.setRgbPalette(rgbPalette);
    graphicsPanel.add(dosGraphics);
    
    pack();
    repaint();

  }

  
  // update the title of the window
  private void updateTitle() {
    setTitle(title + " - " + tileFilename);  
  }
 
  
  /**
   * Select a tiles file to load with a file chooser, then load it
   * and redraw.
   * 
   */
  private void chooseLoadTileset() {
    JFileChooser jfc = new JFileChooser();
    jfc.setDialogType(JFileChooser.OPEN_DIALOG);
    jfc.setCurrentDirectory(new File(tilesDir));     
    if (jfc.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {      
      File tilesFile = jfc.getSelectedFile();     
      if (tilesFile != null) {
        loadTileset(tilesFile.getPath());
      }
    }
  }

  
  /**
   * Select a tiles file to save with a file chooser, then save it.
   */
  private void chooseSaveTileset() {
    JFileChooser jfc = new JFileChooser();
    jfc.setDialogType(JFileChooser.SAVE_DIALOG);
    jfc.setCurrentDirectory(new File(tilesDir));
    if (jfc.showSaveDialog(null) == JFileChooser.APPROVE_OPTION) {     
      File tilesFile = jfc.getSelectedFile();
      if (tilesFile != null) {
        saveTileset(tilesFile.getPath()); 
      }
    }
  }
  

  // load the tileset and update the palette
  private void loadTileset(String filename) {
    tileset = new OldTilesetLoader(filename, attrs).load();
    Tileset.modPalette(tileset.palettes().apply(0), dosGraphics.getRgbPalette());
    tileFilename = filename;
    paletteWindow.repaint();
    updateTitle();
    repaint();
  }
 
 
  // grab the palette, update the tileset, and save it
  private void saveTileset(String filename) {
   
    // mutate the tileset's default palette before saving!!!
    Palette newPal = Tileset.extractPalette(tileset.palettes().apply(0), dosGraphics.getRgbPalette());
    Palette pal = tileset.palettes().apply(0);
    for (int i = 0; i < pal.colors().length; i++) {
      pal.colors()[i] = newPal.colors()[i];
    }       
    new OldTilesetLoader(filename, attrs).save(tileset);
   
    tileFilename = filename;
    updateTitle();
    repaint();
   
  }
  
  
  /**
   * Draw the component.
   */
  @Override
  public void paint(Graphics graphics) {
    super.paint(graphics);

    dosGraphics.updateClut();
    dosGraphics.drawTileset(tileset);
    dosGraphics.repaint();

  }
  
  
  private JMenuBar mainMenu() {
    
    final JMenuBar mainMenu = new JMenuBar();

    final JMenu fileMenu = new JMenu("File");

    final JMenuItem jmOpen = new JMenuItem("Open");
    final JMenuItem jmSave = new JMenuItem("Save");
    final JMenuItem jmSaveAs = new JMenuItem("Save As..");
    final JMenuItem jmReload = new JMenuItem("Reload");
    final JMenuItem jmChange = new JMenuItem("Change settings...");

    final JMenu toolsMenu = new JMenu("Tools");

    final JMenuItem jmSwap = new JMenuItem("Swap transparency");
    final JMenuItem jmBlacken = new JMenuItem("Blacken");
    
    final JMenu viewMenu = new JMenu("View");
    final JMenuItem gridShow = new JCheckBoxMenuItem("Grid");
    gridShow.setSelected(false);

    jmOpen.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        chooseLoadTileset();
      }
    });

    jmSave.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        if (!tileFilename.equals("")) {
          saveTileset(tileFilename);
        } else {
          chooseSaveTileset();
        }  
      }
    });

    jmSaveAs.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        chooseSaveTileset();
      }
    });

    jmReload.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        if (!tileFilename.equals("")) {
          loadTileset(tileFilename);
        } else {
          chooseLoadTileset();
        }
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
    
    gridShow.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        System.out.println("grid show: "+ gridShow.isSelected());
        dosGraphics.setShowGrid(gridShow.isSelected());
        dosGraphics.repaint();
      }
    });
    
    fileMenu.add(jmOpen);
    fileMenu.add(jmSave);
    fileMenu.add(jmSaveAs);
    fileMenu.add(jmReload);
    fileMenu.addSeparator();
    fileMenu.add(jmChange);  
    mainMenu.add(fileMenu);
    
    toolsMenu.add(jmSwap);
    toolsMenu.add(jmBlacken); 
    mainMenu.add(toolsMenu);
    
    viewMenu.add(gridShow);
    mainMenu.add(viewMenu);
       
    return mainMenu;
  }
  
  
  public Tileset getTileSet() {
    return this.tileset;
  }

  public DosGraphics getDosGraphics() {
    return this.dosGraphics;
  }
  
}
