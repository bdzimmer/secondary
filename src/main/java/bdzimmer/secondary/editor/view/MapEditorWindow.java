// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Class implementing a view for displaying and editing maps.

// 2014-08-14: Refactored to sepearate out map view panel from functionality
//              specific to editing.

package bdzimmer.secondary.editor.view;

import bdzimmer.secondary.editor.model.Map;
import bdzimmer.secondary.editor.model.Tileset;
import bdzimmer.secondary.editor.view.MapViewPanel;
import bdzimmer.secondary.editor.view.TilesEditorWindow;

import java.awt.BorderLayout;
import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.io.File;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;



public class MapEditorWindow extends JFrame {
  
  private static final long serialVersionUID = 0; // Meaningless junk.
  
  private static final int TILE_SIZE = 16;
  
  private final String mapsDir;
  
  private Map map;
  public String mapFileName;
  // private Tileset tileset;
  private final TilesEditorWindow tilesEditorWindow;
  
  private final JCheckBoxMenuItem jmHasParallax = new JCheckBoxMenuItem("Parallax Layer");
  private MapViewPanel mapViewPanel;

  private int overlayEdit;
  private StatusBar statusBar = new StatusBar();


  /**
   * Create a new MapEditorWindow.
   * 
   * @param mapsDir       main content directory
   * @param map           Map to display
   * @param fileName      file name of map to load
   * @param tileSet       Tiles object to use
   * @param rgbPalette    2d int array of palette to update the view with
   */
  public MapEditorWindow(
      String mapsDir,
      Map map,
      String fileName,
      TilesEditorWindow tilesEditorWindow) { // constructor

    this.mapsDir = mapsDir;
    this.map = map;
    this.mapFileName = fileName;
    // this.tileset = tileset;
    this.tilesEditorWindow = tilesEditorWindow;
    
    setTitle(generateTitle());

    // ///////////// menu stuff ///////////////////////

    final JCheckBoxMenuItem jmDispOver = new JCheckBoxMenuItem("Display Overlay Layer");
    final JCheckBoxMenuItem jmDispBack = new JCheckBoxMenuItem("Display Background Layer");
    final JCheckBoxMenuItem jmDispBounds = new JCheckBoxMenuItem("Display Bounds");
    
    jmDispOver.setSelected(true);
    jmDispBack.setSelected(true);
    jmDispBounds.setSelected(false);
    jmHasParallax.setSelected(false);

    JMenuBar mainMenu = new JMenuBar();
    setJMenuBar(mainMenu);

    JMenu fileMenu = new JMenu("File");
    
    JMenuItem jmNew = new JMenuItem("New");
    fileMenu.add(jmNew);
    
    
    JMenuItem jmOpen = new JMenuItem("Open");
    fileMenu.add(jmOpen);
    
    JMenuItem jmSave = new JMenuItem("Save");
    fileMenu.add(jmSave);
    
    JMenuItem jmSaveAs = new JMenuItem("Save As");
    fileMenu.add(jmSaveAs);
       
    mainMenu.add(fileMenu);

    JMenu editMenu = new JMenu("Edit");
    
    JMenuItem jmSetTitle = new JMenuItem("Set title...");
    editMenu.add(jmSetTitle);
    
    editMenu.addSeparator();
    
    JMenuItem jmEditOverlay = new JMenuItem("Editing Background Layer");
    editMenu.add(jmEditOverlay);
    
    editMenu.addSeparator();
    
    editMenu.add(jmDispOver);
    editMenu.add(jmDispBack);
    editMenu.add(jmDispBounds);
    
    editMenu.addSeparator();
    
    editMenu.add(jmHasParallax);
    
    mainMenu.add(editMenu);

    jmNew.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent ae) {
        MapEditorWindow.this.map.erase();
        mapViewPanel.vud = 0;
        mapViewPanel.vlr = 0;
        repaint();
      }
    });

    jmOpen.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent ae) {
        chooseLoadMap();
      }
    });

    jmSave.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent ae) {
        MapEditorWindow.this.map.save(new File(mapFileName));
      }
    });

    jmSaveAs.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent ae) {
        chooseSaveMap();
      }
    });

    jmSetTitle.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent ae) {
        MapEditorWindow.this.map.mapDesc = JOptionPane.showInputDialog("Enter new title:");
        MapEditorWindow.this.map.tileFileName = JOptionPane
            .showInputDialog("Enter new tile file name:");
        setTitle(generateTitle());
      }
    });

    jmEditOverlay.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent ae) {
        if (overlayEdit == 0) {
          ((JMenuItem) ae.getSource()).setText("Editing Overlay Layer");
          overlayEdit = 1;
          mapViewPanel.setParallaxEdit(false);
        } else if (overlayEdit == 1) {
          ((JMenuItem) ae.getSource()).setText("Editing Parallax Layer");
          overlayEdit = 2;
          mapViewPanel.setParallaxEdit(true);

        } else if (overlayEdit == 2) {
          ((JMenuItem) ae.getSource()).setText("Editing Background Layer");
          overlayEdit = 0;
          mapViewPanel.setParallaxEdit(false);
        }

        repaint();

      }

    });

    // repaint map when choosing whether to display layers / bounds
    jmDispOver.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent ae) {
        System.out.println(jmDispOver.isSelected());
        mapViewPanel.setDispOver(jmDispOver.isSelected());
        repaint();

      }
    });

    jmDispBack.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent ae) {
        System.out.println(jmDispBack.isSelected());
        mapViewPanel.setDispBack(jmDispBack.isSelected());
        repaint();

      }
    });

    jmDispBounds.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent ae) {
        mapViewPanel.setDispBounds(jmDispBounds.isSelected());
        repaint();
      }
    });

    // listener for scrolling with arrow keys
    this.setFocusable(true);
    this.addKeyListener(new KeyListener() {

      @Override
      public void keyPressed(KeyEvent ae) {
        handleKeys(ae);
      }

      @Override
      public void keyReleased(KeyEvent ae) {}
      
      @Override
      public void keyTyped(KeyEvent ae) {}

    });

    this.addMouseWheelListener(new MouseWheelListener() {

      @Override
      public void mouseWheelMoved(MouseWheelEvent ae) {
        int notches = ae.getWheelRotation();
        notches = Integer.signum(notches);
        zoom(notches);

      }

    });

    // Set the layout manager.
    this.setLayout(new BorderLayout());

    this.mapViewPanel = new MapViewPanel(
        this.map,
        this.tilesEditorWindow.getTileSet(),
        this.tilesEditorWindow.getDosGraphics().getRgbPalette());
    this.add(mapViewPanel, BorderLayout.NORTH);

    this.add(statusBar, BorderLayout.SOUTH);
    this.pack();

    // Clicking, dragging on map to get / set tiles
    // Moving mouse updates coordinate view--
    this.mapViewPanel.addMouseMotionListener(new MouseMotionListener() {

      public void mouseDragged(MouseEvent event) {
        handleClicks(event);
      }

      public void mouseMoved(MouseEvent event) {
        statusBar.update(
            mapViewPanel.vlr
                + (int) (event.getX() / (MapEditorWindow.TILE_SIZE * mapViewPanel.scale)),
            mapViewPanel.vud
                + (int) (event.getY() / (MapEditorWindow.TILE_SIZE * mapViewPanel.scale)),
            "");
      }

    });

    // clicking mouse
    this.mapViewPanel.addMouseListener(new MouseListener() {

      @Override
      public void mousePressed(MouseEvent me) {
        handleClicks(me);
      }
      
      @Override
      public void mouseClicked(MouseEvent me) {}
      
      @Override
      public void mouseEntered(MouseEvent me) {}
      
      @Override
      public void mouseExited(MouseEvent me) {}
      
      @Override
      public void mouseReleased(MouseEvent me) {}

    });

    // experimenting 2-2-14
    // this.setResizable(false);

    // resize listener

    this.addComponentListener(new ComponentAdapter() {
      public void componentResized(ComponentEvent ae) {
        System.out.println("component resized!!");
        System.out.println(ae.getComponent());
      }
    });

    // ////////////////////////////////
    setVisible(true);
    repaint();

  }

  // / helper functions for handling events
  // ------------------------------------------------------

  private void zoom(int amount) {
    this.mapViewPanel.scale += amount;
    if (this.mapViewPanel.scale < 1) {
      this.mapViewPanel.scale = 1;
    }
    this.updateGraphics();

  }

  private void handleClicks(MouseEvent ae) {

    int ctud = mapViewPanel.vud
        + (ae.getY() / (MapEditorWindow.TILE_SIZE * this.mapViewPanel.scale));
    int ctlr = mapViewPanel.vlr
        + (ae.getX() / (MapEditorWindow.TILE_SIZE * this.mapViewPanel.scale));
    if (ctud < 0 || ctud > 127) {
      return;
    }
    if (ctlr < 0 || ctlr > 127) {
      return;
    }
    

    
    if (!ae.isMetaDown()) {
      if (this.overlayEdit == 0) {
        this.map.map[ctud][ctlr] = Main.currentTile; // setting tile
      } else if (overlayEdit == 1) {
        this.map.overMap[ctud][ctlr] = Main.currentTile;
      } else if (overlayEdit == 2) {
        this.map.paraMap[ctud][ctlr] = Main.currentTile;
      }
      repaint();
    } else {
      
      if (overlayEdit == 0) {
        Main.currentTile = this.map.map[ctud][ctlr]; // getting tile
      } else if (overlayEdit == 1) {
        Main.currentTile = this.map.overMap[ctud][ctlr];
      } else if (overlayEdit == 2) {
        Main.currentTile = this.map.paraMap[ctud][ctlr];
      }
    }
 
  }
  
  
  private void handleKeys(KeyEvent ae) {
    
    if (ae.getKeyCode() == KeyEvent.VK_UP) {
      if (!ae.isAltDown()) {
        mapViewPanel.vud -= 1;
      } else {
        for (int i = mapViewPanel.vud; i < 127; i++) {
          for (int j = 0; j < 128; j++) {
            map.map[i][j] = map.map[i + 1][j];
            map.overMap[i][j] = map.overMap[i + 1][j];
          }
        }
      }
    } else if (ae.getKeyCode() == KeyEvent.VK_DOWN) {
      if (!ae.isAltDown()) {
        mapViewPanel.vud += 1;
      } else {
        for (int i = 127; i >= (mapViewPanel.vud + 1); i--) {
          for (int j = 0; j < 128; j++) {
            map.map[i][j] = map.map[i - 1][j];
            map.overMap[i][j] = map.overMap[i - 1][j];
          }
        }
      }
    } else if (ae.getKeyCode() == KeyEvent.VK_LEFT) {
      if (!ae.isAltDown()) {
        mapViewPanel.vlr -= 1;
      } else {
        for (int i = 0; i < 128; i++) {
          for (int j = mapViewPanel.vlr; j < 127; j++) {
            map.map[i][j] = map.map[i][j + 1];
            map.overMap[i][j] = map.overMap[i][j + 1];
          }
        }
      }
    } else if (ae.getKeyCode() == KeyEvent.VK_RIGHT) {
      if (!ae.isAltDown()) {
        mapViewPanel.vlr += 1;
      } else {
        for (int i = 0; i < 128; i++) {
          for (int j = 127; j >= mapViewPanel.vlr + 1; j--) {
            map.map[i][j] = map.map[i][j - 1];
            map.overMap[i][j] = map.overMap[i][j - 1];
          }
        }
      }
    }

    repaint();
    
  }

  // //// loading and saving maps
  // ---------------------------------------------------------

  /**
   * Choose a map file from a file chooser and load it.
   */
  public void chooseLoadMap() {

    JFileChooser jfc = new JFileChooser();
    jfc.setDialogType(JFileChooser.OPEN_DIALOG);
    jfc.setCurrentDirectory(new File(mapsDir));
    
    if (jfc.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) { 
      
      File selFile = jfc.getSelectedFile();
      try {
        map = new Map(selFile);
        jmHasParallax.setSelected(map.hasParallax);
        mapFileName = selFile.getAbsolutePath();
        setTitle(generateTitle());

        System.out.println("Map file name: " + mapFileName);
        mapViewPanel.setMap(map);
        mapViewPanel.vud = 0;
        mapViewPanel.vlr = 0;
        mapViewPanel.updateGraphics();
        mapViewPanel.repaint();
        repaint();
        
      } catch (NullPointerException e) {
        System.err.println(e);
        return;
      }
    }
  }

  /**
   * Choose a map file from a file chooser and save it.
   */
  public void chooseSaveMap() {
    JFileChooser jfc = new JFileChooser();
    jfc.setDialogType(JFileChooser.SAVE_DIALOG);
    jfc.setCurrentDirectory(new File(mapsDir));
    jfc.setSelectedFile(new File(this.mapFileName));
    
    // call up the dialog and examine what it returns.
    
    if (jfc.showSaveDialog(null) == JFileChooser.APPROVE_OPTION) { 
      
      File mapFile = jfc.getSelectedFile();
      try {
        map.save(mapFile); // getSelectedFile returns the file that was selected
        repaint();
      } catch (NullPointerException e) {
        System.err.println(e);
        return;
      }
    }
  }
  
  
  // generate a title
  private String generateTitle() {
    return map.mapDesc.trim() + " (" + map.tileFileName + ") - " + this.mapFileName;  
  }
  
  

  // updating graphics
  // --------------------------------------------------------

  private void updateGraphics() {
    this.mapViewPanel.setTileset(tilesEditorWindow.getTileSet());
    this.mapViewPanel.updateGraphics();
    this.pack();
    this.repaint(); 
  }

 
  public void paint(Graphics gr) {
    super.paint(gr);
    this.mapViewPanel.repaint();  
  }
 

}
