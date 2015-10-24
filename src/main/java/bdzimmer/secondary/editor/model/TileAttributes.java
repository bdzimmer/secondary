// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.editor.model;

public class TileAttributes {

  public int height;
  public int width;
  public int count;
  public int palStart;
  public int palEnd;
  public boolean tileProperties;
  public int tilesPerRow;

  /**
   * Create a new TileAttributes object.
   * 
   * @param height            height of each tile in pixels
   * @param width             width of each tile in pixels 
   * @param count             number of tiles in the set
   * @param palStart          palette start point
   * @param palEnd            palette end point
   * @param tileProperties    whether the tiles have bounds and overlay properties
   * @param tilesPerRow       tiles per row when displaying during editing 
   *                          and image export
   */
  public TileAttributes(int height, int width, int count, int palStart,
      int palEnd, boolean tileProperties, int tilesPerRow) {

    this.height = height;
    this.width = width;
    this.count = count;
    this.palStart = palStart;
    this.palEnd = palEnd;
    this.tileProperties = tileProperties;
    this.tilesPerRow = tilesPerRow;

  }

}