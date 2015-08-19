// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.view;

import java.awt.Dimension;

import javax.swing.JComponent;

class WorldObject extends JComponent {
  private static final long serialVersionUID = 1L;
  protected int width;
  protected int height;

  public WorldObject() {
    this(320, 200);
  }

  public WorldObject(int width, int height) {

    this.width = width;
    this.height = height;

    this.setAlignmentX(JComponent.RIGHT_ALIGNMENT);
  }

  public Dimension getPreferredSize() {
    return new Dimension(width, height);
  }

  public Dimension getSize() {
    return new Dimension(width, height);
  }

}