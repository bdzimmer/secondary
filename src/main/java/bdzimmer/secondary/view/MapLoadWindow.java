// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.view;


import bdzimmer.secondary.model.DosGraphics;
import bdzimmer.secondary.model.Map;
import bdzimmer.secondary.model.TileOptionsNew;
import bdzimmer.secondary.model.Tiles;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.ArrayList;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;

public class MapLoadWindow extends WorldObjectWindow {

  private static final long serialVersionUID = 1L;

  public MapLoadWindow(String inputDir) {
    super(inputDir, "Load Maps");
  }

  @Override
  public ArrayList<WorldObject> populateObjects(String inputDir) {

    ArrayList<WorldObject> result = new ArrayList<WorldObject>();

    for (String mapFileName : new File(inputDir).list()) {
      result.add(new MapIcon(inputDir + mapFileName, 320, 200));
    }

    return result;
  }

  class MapIcon extends WorldObject {
    private static final long serialVersionUID = 1L;

    private String mapFile;
    private String tilesFile;
    private String description;
    private BufferedImage mapImage;

    public MapIcon(String mapFileName, int width, int height) {

      this.mapFile = mapFileName;


      Map curMap = new Map();
      curMap.load(new File(mapFileName));
      this.description = curMap.mapDesc;

      Tiles mapTiles = new Tiles(TileOptionsNew.get("Tiles"));
      this.tilesFile = Main.DATA_PATH + "tile/" + curMap.tileFileName + ".til";
      DosGraphics dg = new DosGraphics();
      mapTiles.load(new File(this.tilesFile), dg);
      this.mapImage = curMap.getMapImage(mapTiles, dg);

      this.width = width;
      this.height = height;

      this.setAlignmentX(JComponent.RIGHT_ALIGNMENT);

      JButton loader = new JButton();
      loader.setSize(this.width, this.height);
      loader.setIcon(new ImageIcon(this.mapImage));
      loader.setBackground(Color.BLACK);
      loader.setForeground(Color.WHITE);
      loader.setText("\n\n\n" + this.description + " -  "
          + new File(this.mapFile).getName());
      loader.setHorizontalTextPosition(JButton.CENTER);
      loader.setVerticalTextPosition(JButton.CENTER);

      loader.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          Main.createLinkedTileAndMapWindows(tilesFile, mapFile);
        }

      });

      this.add(loader);

    }

    /*
     * 
     * protected void paintComponent(Graphics g) { super.paintComponent(g);
     * 
     * g.setColor(Color.black); g.fillRect(0, 0, this.getWidth(),
     * this.getHeight());
     * 
     * 
     * g.drawImage(this.mapImage, (this.width - this.mapImage.getWidth()) / 2,
     * (this.height - this.mapImage.getHeight()) / 2, null);
     * 
     * g.setFont(new Font("Monospace", Font.BOLD, 12)); g.setColor(Color.white);
     * g.drawString(this.description + " -  " + new
     * File(this.mapFile).getName(), 10, this.height - 20);
     * 
     * 
     * }
     */

  }

}
