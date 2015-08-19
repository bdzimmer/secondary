// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.controller;

// Class for Flood Fill -- Adapted from Java code found at Code Codex


public class FloodFill {

  /**
   * Perform a flood fill on an image.
   * 
   * @param grid          grid to perform flood fill on
   * @param fillNumber    value to fill
   * @param ud            vertical position
   * @param lr            horizontal position
   */
  public static void floodFill(int[][] grid, int fillNumber, int ud, int lr) {
    if (ud < 0 || ud > grid.length || lr < 0 || lr > grid[0].length) {
      throw new IllegalArgumentException();
    }
    int targetNumber = grid[ud][lr];

    if (fillNumber == targetNumber) { // location is already of fillNumber
      return;
    }

    floodLoop(grid, ud, lr, fillNumber, targetNumber);

  }

  private static void floodLoop(int[][] grid, int ud, int lr, int fillNumber,
      int oldNumber) {

    // find left end, filling along the way
    int fillLeft = lr;
    do {
      grid[ud][fillLeft] = fillNumber;
      fillLeft--;
    } while (fillLeft >= 0 && grid[ud][fillLeft] == oldNumber);
    fillLeft++;

    // find left end, filling along the way
    int fillRight = lr;
    do {
      grid[ud][fillRight] = fillNumber;
      fillRight++;
    } while (fillRight < grid[0].length && grid[ud][fillRight] == oldNumber);
    fillRight--;

    // look for fills above and below
    for (int i = fillLeft; i <= fillRight; i++) {
      if (ud > 0 && grid[ud - 1][i] == oldNumber) {
        floodLoop(grid, ud - 1, i, fillNumber, oldNumber);
      }
      if (ud < grid.length - 1 && grid[ud + 1][i] == oldNumber) {
        floodLoop(grid, ud + 1, i, fillNumber, oldNumber);
      }
    }
  }

}
