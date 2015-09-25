// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.view;

import bdzimmer.secondary.model.DosGraphics;
import bdzimmer.secondary.model.Map;
import bdzimmer.secondary.model.ScriptFile;
import bdzimmer.secondary.model.TileOptionsNew;
import bdzimmer.secondary.model.Tiles;
import bdzimmer.secondary.model.World;
import bdzimmer.secondary.model.ContentStructure;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;

import javax.swing.JButton;
import javax.swing.JPanel;



public class ScriptFileWindow extends WorldObjectWindow {

  private static final long serialVersionUID = 0; // Meaningless junk.

  public ScriptFileWindow(Main main, String inputDir) {
    super(main, inputDir, "Script Files");
  }

  @Override
  public ArrayList<WorldObject> populateObjects(String inputDir) {

    ArrayList<WorldObject> result = new ArrayList<WorldObject>();
    World world = new World(main.contentDir);

    for (ScriptFile curScriptFile : world.getScriptFiles()) {
      result.add(new ScriptObject(curScriptFile));
    }

    return result;
  }

  class ScriptObject extends WorldObject {

    private static final long serialVersionUID = 1L;

    private ScriptFile scriptFile;
    private JPanel buttonPanel;
    private ArrayList<BufferedImage> mapImages = new ArrayList<BufferedImage>();

    public ScriptObject(ScriptFile scFile) {

      super(420, 200);

      this.scriptFile = scFile;

      for (String curMapName : scriptFile.getMaps()) {

        DosGraphics dosGraphics = new DosGraphics();
        Map curMap = new Map();
        
        curMap.load(new File(
            main.contentDir + File.separator 
            + ContentStructure.MapDir() + File.separator 
            + curMapName + ".map"));
        
        Tiles curTiles = new Tiles(TileOptionsNew.get("Tiles"));
        
        curTiles.load(new File(
            main.contentDir + File.separator
            + ContentStructure.TileDir() + File.separator
            + curMap.tileFileName + ".til"), dosGraphics);
        
        this.mapImages.add(curMap.getMapImage(curTiles, dosGraphics));

      }

      this.setLayout(new BorderLayout());

      buttonPanel = new JPanel();
      buttonPanel.setPreferredSize(new Dimension(100, 200));
      buttonPanel.setLayout(new GridLayout(5, 1, 0, 0));

      JButton viewMapsButton = new JButton("View Maps");
      viewMapsButton.addActionListener(new ActionListener() {

        public void actionPerformed(ActionEvent event) {

          for (BufferedImage curMapImage : mapImages) {
            new ImageWindow(curMapImage);
          }

        }

      });
      buttonPanel.add(viewMapsButton);

      JButton editMapsButton = new JButton("Edit Maps");
      editMapsButton.addActionListener(new ActionListener() {

        public void actionPerformed(ActionEvent event) {

          for (String curMapName : scriptFile.getMaps()) {

            // get tileFileName
            String tileFileName;
            try {
              DataInputStream mapIn = new DataInputStream(new FileInputStream(
                  main.contentDir + File.separator
                  + ContentStructure.MapDir() + File.separator + curMapName + ".map"));
              
              // TODO: this is duplicate code
              
              // read description
              int[] mapDescB = new int[30];
              for (int i = 0; i < 30; i++) {
                mapDescB[i] = (0x000000FF & (int) mapIn.readByte());
              }
              // this.mapDesc = new String(mapDesc);

              // read tileset
              int[] tileFileNameB = new int[8];
              char[] tileFileNameC = new char[8];
              for (int i = 0; i < 8; i++) {
                tileFileNameB[i] = (0x000000FF & (int) mapIn.readByte());
                tileFileNameC[i] = (char) tileFileNameB[i];
              }
              tileFileName = new String(tileFileNameC).trim();

              mapIn.close();
            } catch (FileNotFoundException ex) {
              System.err.println(ex); // print exception if the file doesn't
                                      // exist.
              return;
            } catch (IOException ex) {
              System.err.println(ex);
              return;
            }

            tileFileName = main.contentDir + File.separator
                + ContentStructure.TileDir() + File.separator
                + tileFileName + ".til";
            
            String fullMapName = main.contentDir + File.separator
                + ContentStructure.MapDir() + File.separator
                + curMapName + ".map";

            System.out.println(fullMapName);

            ScriptFileWindow.this.main.createLinkedTileAndMapWindows(tileFileName, fullMapName);

          }

        }

      });
      buttonPanel.add(editMapsButton);

      JButton editScriptButton = new JButton("Edit Script");
      editScriptButton.addActionListener(new ActionListener() {

        public void actionPerformed(ActionEvent event) {

          // /new ScriptWindow(scriptFile);
          try {
            Runtime.getRuntime().exec(
                "notepad.exe " + main.contentDir + File.separator
                + "script" + File.separator 
                + scriptFile.getFileName());
          } catch (IOException e1) {
            e1.printStackTrace();
          }

        }

      });
      buttonPanel.add(editScriptButton);

      buttonPanel.setVisible(true);
      this.add(buttonPanel, BorderLayout.EAST);

    }

    protected void paintComponent(Graphics graphics) {
      super.paintComponent(graphics);
      graphics.setColor(Color.black);
      graphics.fillRect(0, 0, this.getWidth(), this.getHeight());

      BufferedImage curImage = null;
      if (this.mapImages.size() > 0) {
        curImage = this.mapImages.get(0);
      }
      if (curImage != null) {
        graphics.drawImage(curImage, (320 - curImage.getWidth()) / 2,
            (200 - curImage.getHeight()) / 2, null);
        // g.drawImage(cImage, (this.getWidth() - cImage.getWidth()) / 2,
        // (this.getHeight() - cImage.getHeight()) / 2, null);
      }

      graphics.setFont(new Font("Monospace", Font.BOLD, 12));
      graphics.setColor(Color.white);
      graphics.drawString(this.scriptFile.getTitle(), 10, 20);
    }

  }

}
