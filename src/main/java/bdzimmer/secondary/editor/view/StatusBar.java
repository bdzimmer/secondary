// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.editor.view;

import java.awt.BorderLayout;
import java.awt.FlowLayout;

import javax.swing.BorderFactory;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.BevelBorder;



public class StatusBar extends JPanel {
  private static final long serialVersionUID = 1L;
  private JLabel xcoordLabel;
  private JLabel tilesWindowTiles;
  private JLabel otherLabel;

  /**
   * Create a new StausBar.
   */
  public StatusBar() {
    this.xcoordLabel = new JLabel("X: ");
    xcoordLabel.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));
    this.tilesWindowTiles = new JLabel("Y: ");
    tilesWindowTiles.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));
    this.otherLabel = new JLabel("  ");
    otherLabel.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));

    this.setLayout(new FlowLayout(FlowLayout.LEFT));
    this.add(xcoordLabel);
    this.add(tilesWindowTiles);
    this.add(otherLabel);

  }

  /**
   * Update the information shown on the StatusBar.
   * 
   * @param xval          x value
   * @param yval          y value
   * @param otherLabel    additional string to display
   */
  public void update(int xval, int yval, String otherLabel) {
    this.xcoordLabel.setText("X: " + xval);
    this.tilesWindowTiles.setText("Y: " + yval);
    this.otherLabel.setText(otherLabel);
    this.repaint();
  }

}
