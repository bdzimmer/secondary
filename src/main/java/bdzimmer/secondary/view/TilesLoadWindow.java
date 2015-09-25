// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.view;

import bdzimmer.secondary.model.DosGraphics;
import bdzimmer.secondary.model.TileOptionsNew;
import bdzimmer.secondary.model.Tiles;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;


import javax.swing.ImageIcon;
import javax.swing.JButton;


public class TilesLoadWindow extends WorldObjectWindow {

  private static final long serialVersionUID = 1L;

  public TilesLoadWindow(Main main, String inputDir) {
    super(main, inputDir, "Load Tiles");
  }

  @Override
  public ArrayList<WorldObject> populateObjects(String inputDir) {

    ArrayList<WorldObject> result = new ArrayList<WorldObject>();

    String listString = "";

    try {
      listString = new Scanner(new File(inputDir + "list.txt")).useDelimiter(
          "\\A").next();
    } catch (FileNotFoundException e) {
      e.printStackTrace();
    }

    String[] tileFilenames = listString.split("\r\n");

    for (String curLine : tileFilenames) {

      if (!curLine.equals("")) {

        String[] splitted = curLine.split(", ");
        String curTileFilename = splitted[1];
        curTileFilename = inputDir
            + curTileFilename.substring(1, curTileFilename.length() - 1)
            + ".til";
        String name = splitted[0].substring(1, splitted[0].length() - 1);

        result.add(new TilesIcon(curTileFilename, name));

      }
    }

    return result;

  }

  class TilesIcon extends WorldObject {
    private static final long serialVersionUID = 1L;

    private String tilesFile;
    private String name;
    private BufferedImage tilesImage;

    public TilesIcon(String tilesFileName, String name) {

      this.tilesFile = tilesFileName;
      this.name = name;

      Tiles tiles = new Tiles(TileOptionsNew.get("Tiles"));
      DosGraphics dosGraphics = new DosGraphics();
      tiles.load(new File(this.tilesFile), dosGraphics);

      this.tilesImage = tiles.getTilesImage(16, 16, dosGraphics.getPalette());

      this.width = this.tilesImage.getWidth();
      this.height = this.tilesImage.getHeight();

      JButton loader = new JButton();
      loader.setSize(this.tilesImage.getWidth(), this.tilesImage.getHeight());

      loader.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          main.createLinkedTileAndMapWindows(tilesFile, null);
        }
      });

      System.out.println(this.name);

      loader.setIcon(new ImageIcon(this.tilesImage));
      loader.setText("\n\n\n\n\n" + this.name);
      loader.setForeground(Color.WHITE);

      this.add(loader);

    }

  }

}
