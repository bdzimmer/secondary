// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.editor.view;

import javax.swing.BorderFactory;
import java.awt.FlowLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.BevelBorder;


public class StatusBar extends JPanel {
  
  private static final long serialVersionUID = 1L;
  private final JLabel xcoordLabel;
  private final JLabel tilesWindowTiles;
  private final JLabel otherLabel;

  
  /**
   * Create a new StausBar.
   */
  public StatusBar() {
    xcoordLabel = new JLabel("X: ");
    xcoordLabel.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));
    tilesWindowTiles = new JLabel("Y: ");
    tilesWindowTiles.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));
    otherLabel = new JLabel("  ");
    otherLabel.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));

    setLayout(new FlowLayout(FlowLayout.LEFT));
    add(xcoordLabel);
    add(tilesWindowTiles);
    add(otherLabel);

  }

  
  /**
   * Update the information shown on the StatusBar.
   * 
   * @param xval          x value
   * @param yval          y value
   * @param text          additional text
   */
  public void update(int xval, int yval, String text) {
    xcoordLabel.setText("X: " + xval);
    tilesWindowTiles.setText("Y: " + yval);
    otherLabel.setText(text);
    repaint();
  }

}
