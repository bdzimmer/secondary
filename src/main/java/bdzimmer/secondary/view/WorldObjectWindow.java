// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.view;


import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.util.ArrayList;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;


public abstract class WorldObjectWindow extends JFrame {

  private static final long serialVersionUID = 1L;
  
  protected final Main main;

  private ArrayList<WorldObject> worldObjects = new ArrayList<WorldObject>();

  private String inputDir;

  private JPanel scrollingSurface = new JPanel();

  /**
   * Create a new WorldObjectWindow.
   * 
   * @param inputDir      input directory for world data
   * @param title         title for window
   */
  public WorldObjectWindow(Main main, String inputDir, String title) {
    
    this.main = main;
    
    this.inputDir = inputDir;
    this.setTitle(title);

    this.scrollingSurface.setLayout(new GridLayout(20, 2, 20, 20));
    this.scrollingSurface.setLayout(new FlowLayout());

    this.scrollingSurface.setPreferredSize(new Dimension(800, 3200));

    JScrollPane scrollPane = new JScrollPane();
    scrollPane.getVerticalScrollBar().setUnitIncrement(20);
    scrollPane.setViewportView(this.scrollingSurface);
    scrollPane.setPreferredSize(new Dimension(960, 600));
    scrollPane.setOpaque(true);

    this.setContentPane(scrollPane);

    this.worldObjects = this.populateObjects(this.inputDir);

    // populate the scrolling surface with the objects
    this.refresh();

    this.pack();
    this.setVisible(true);

  }

  public abstract ArrayList<WorldObject> populateObjects(String inputDir);

  private void refresh() {

    this.scrollingSurface.removeAll();

    for (WorldObject wo : this.worldObjects) {
      this.scrollingSurface.add(wo);
    }

    this.scrollingSurface.repaint();

    this.repaint();
  }

}
