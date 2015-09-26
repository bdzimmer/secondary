// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.view;


import bdzimmer.secondary.model.DosGraphics;
import bdzimmer.secondary.model.Map;
import bdzimmer.secondary.model.TileOptionsNew;
import bdzimmer.secondary.model.Tiles;
import bdzimmer.secondary.model.ContentStructure;

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

  public MapLoadWindow(Main main, String inputDir) {
    super(main, inputDir, "Load Maps");
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
  
    private final String mapFile;
    private final String tilesFile;
    private final String description;
    private final BufferedImage mapImage;

    public MapIcon(String mapFileName, int width, int height) {

      this.mapFile = mapFileName;

      Map curMap = new Map(new File(mapFileName));
      this.description = curMap.mapDesc;
      
      this.tilesFile = main.contentDir + File.separator
          + ContentStructure.TileDir() + File.separator
          + curMap.tileFileName + ".til";
      
      DosGraphics dg = new DosGraphics();
      
      Tiles mapTiles = new Tiles(
          TileOptionsNew.get("Tiles"),
          new File(this.tilesFile), dg.getRgbPalette());
      dg.updateClut();
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
          MapLoadWindow.this.main.createLinkedTileAndMapWindows(tilesFile, mapFile);
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
