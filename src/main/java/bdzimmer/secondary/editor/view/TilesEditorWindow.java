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

import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;

public class TilesEditorWindow extends JFrame {
  
  private static final long serialVersionUID = 0; // Meaningless junk.
  
  private final String tilesDir;
  private Tileset tileset;
  private TileAttributes attrs;
  public String tileFileName;
  private final String title;

  private DosGraphics dosGraphics;
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
   * @param title               general title for window
   * @param fileName            file name of tiles (for save menu option)
   * @param paletteWindow       palette window to edit 
   */
  public TilesEditorWindow(
      String tilesDir,
      Tileset tiles,
      TileAttributes attributes,
      String title,
      String fileName,
      PaletteWindow paletteWindow) { 
    
    this.tilesDir = tilesDir;    
    this.tileset = tiles;
    this.attrs = attributes;
    
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
        saveTileset(tileFileName);
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
        (int)Math.ceil((float)tileset.tiles().length / tileset.tilesPerRow()) * tileset.height(),
        tileset.tilesPerRow() * tileset.width(),
        this.scale);
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
      
      Main.currentTile = this.currentTile;
      Main.currentTileBitmap = tileset.tiles()[currentTile].pixels();

      if (this.zoomWindow == null || !this.zoomWindow.isVisible()) {
        this.zoomWindow = new ZoomedTileWindow(
            "Zoom",
            tileset.tiles()[currentTile].pixels(),
            this.dosGraphics.getRgbPalette());
        this.zoomWindow.setTileWindow(this);
        this.zoomWindow.setLocationRelativeTo(this);

      } else {
        this.zoomWindow.setTile(this.tileset.tiles()[currentTile].pixels(), currentTile);
      }

    } else {
      
      int newTile = selectedTile;

      // Calculate maximum size we can copy...
      int udlength;
      int lrlength;
      if (tileset.height() > Main.currentTileBitmap.length) {
        udlength = Main.currentTileBitmap.length;
      } else {
        udlength = tileset.height();
      }
      if (tileset.width() > Main.currentTileBitmap[0].length) {
        lrlength = Main.currentTileBitmap[0].length;
      } else {
        lrlength = tileset.width();
      }

      for (int i = 0; i < udlength; i++) {
        for (int j = 0; j < lrlength; j++) {
          tileset.tiles()[newTile].pixels()[i][j] = Main.currentTileBitmap[i][j];
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

    this.tileset = OldTilesetLoader.fromAttributes(TileOptions.getOptions());

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
    // TODO: tile properties flag!!!
    // this.tileSet.attrs.tileProperties = tilePropsFlag;
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
        
        // TODO: this could go in its own function like saveTileset if I implement "reload"
        tileset = new OldTilesetLoader(tilesFile.getPath(), attrs).load();
        Tileset.modPalette(tileset.palettes().apply(0), dosGraphics.getRgbPalette());
        tileFileName = tilesFile.getAbsolutePath();
        setTitle(generateTitle());
        dosGraphics.updateClut();
        paletteWindow.repaint();
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
        saveTileset(tilesFile.getPath()); 
        repaint();
      } catch (NullPointerException e) {
        System.err.println(e);
        return;
      }
    }
  }

  public Tileset getTileSet() {
    return this.tileset;
  }

  public DosGraphics getDosGraphics() {
    return this.dosGraphics;
  }
  
  // generate a title
  private String generateTitle() {
    return title + " - " + this.tileFileName;  
  }
  
  // grab the palette, update the tileset, and save it
  private void saveTileset(String filename) {
    
    // mutate the default palette before saving!!!
    Palette newPal = Tileset.extractPalette(tileset.palettes().apply(0), dosGraphics.getRgbPalette());
    Palette pal = tileset.palettes().apply(0);
    for (int i = 0; i < pal.colors().length; i++) {
      pal.colors()[i] = newPal.colors()[i];
    }       
    new OldTilesetLoader(filename, attrs).save(tileset);
    
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

}
