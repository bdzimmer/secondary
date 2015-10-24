// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.editor.view;

import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.BorderLayout;
import java.util.ArrayList;

import javax.swing.border.EmptyBorder;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JScrollBar;


public abstract class WorldObjectWindow extends JFrame {

  private static final long serialVersionUID = 1L;
  
  private static final int numColumns = 2;
  private static final int margin = 20;
  
  protected final Main main;
  private final ArrayList<WorldObject> worldObjects;

  private JPanel scrollingSurface = new JPanel();

  /**
   * Create a new WorldObjectWindow.
   * 
   * @param inputDir      input directory for world data
   * @param title         title for window
   */
  public WorldObjectWindow(Main main, String inputDir, String title) {
    
    this.main = main;
    this.setTitle(title);

    this.worldObjects = this.populateObjects(inputDir);
    
    int numRows = (int)Math.ceil(this.worldObjects.size() / (double)numColumns);
    
    this.scrollingSurface.setLayout(
        new GridLayout(numRows, numColumns, margin, margin));
       
    WorldObject wo = this.worldObjects.get(0);
    
    int surfaceWidth = wo.width * numColumns + margin * (numColumns - 1);
    int surfaceHeight = wo.height * numRows + margin * (numRows - 1);
    int panelHeight = wo.height * 3 + margin * 2;
    
    this.scrollingSurface.setPreferredSize(
        new Dimension(surfaceWidth, surfaceHeight));

    JScrollPane scrollPane = new JScrollPane();
    scrollPane.getVerticalScrollBar().setUnitIncrement(20);
    scrollPane.setViewportView(this.scrollingSurface);
    scrollPane.setPreferredSize(new Dimension(surfaceWidth, panelHeight));
    scrollPane.setOpaque(true);
    
    // this is important, otherwise there will be extra padding
    // and a horizontal scrollbar will be created.
    scrollPane.setBorder(new EmptyBorder(0, 0, 0, 0));
    
    this.refresh();
    
    // TODO: what happens when there are not enough components for a vertical scrollbar?
    JScrollBar scrollBar = scrollPane.getVerticalScrollBar();
    scrollPane.remove(scrollBar);
    
    this.setLayout(new BorderLayout());
    this.add(scrollPane, BorderLayout.CENTER);
    this.add(scrollBar, BorderLayout.EAST);
    
    this.pack();
    this.repaint();
    this.setVisible(true);
    
  }

  public abstract ArrayList<WorldObject> populateObjects(String inputDir);

  private void refresh() {
    scrollingSurface.removeAll();
    for (WorldObject wo : worldObjects) {
      scrollingSurface.add(wo);
    }
    scrollingSurface.repaint();
    repaint();
  }

}
