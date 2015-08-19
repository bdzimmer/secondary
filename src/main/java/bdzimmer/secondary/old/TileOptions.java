// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.old;

import bdzimmer.secondary.model.TileAttributes;

import java.awt.Dialog;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JRadioButton;



public class TileOptions {

  private JDialog myDialog;

  private TileAttributes attrs = new TileAttributes(16, 16, 256, 128, 255,
      true, 16);

  public static TileAttributes TILES = new TileAttributes(16, 16, 256, 128,
      255, true, 16);
  public static TileAttributes MEMBERS = new TileAttributes(24, 16, 64, 16, 47,
      false, 16);
  public static TileAttributes NEW_MEMBERS = new TileAttributes(24, 16, 128,
      16, 47, false, 16);
  public static TileAttributes NPC = new TileAttributes(24, 16, 64, 48, 95,
      false, 12);
  public static TileAttributes LARGE = new TileAttributes(32, 32, 12, 48, 95,
      false, 12); // get rid of
  public static TileAttributes NEW_LARGE = new TileAttributes(32, 32, 16, 48,
      95, false, 16); // get rid of
  public static TileAttributes NEW_LARGE_2 = new TileAttributes(32, 32, 32, 48,
      95, false, 16);
  public static TileAttributes WEAPON = new TileAttributes(24, 16, 16, 224,
      255, false, 8);
  public static TileAttributes EFFECTS = new TileAttributes(16, 16, 36, 96,
      127, false, 4);
  public static TileAttributes PORTRAITS = new TileAttributes(96, 96, 4, 0, 0,
      false, 4);

  /**
   * Show a dialog to get various TileAtrributes commonly used.
   */
  public void getOptions() {
    myDialog = new JDialog();
    myDialog.setTitle("Choose sprite type");
    myDialog.setLayout(new GridLayout(6, 1, 0, 0));

    JRadioButton jrTiles = new JRadioButton(
        "Tiles..256 16x16 Tiles + Pal 128-255 + Props");
    JRadioButton jrMemb = new JRadioButton(
        "Memb..64 24x16 Sprites + Pal. 16-47");
    JRadioButton jrNewMemb = new JRadioButton(
        "New Memb..128 24x16 Sprites + Pal. 16-47");
    JRadioButton jrNpc = new JRadioButton("NPC..64 24x16 Sprites + Pal. 48-95");
    JRadioButton jrLarge = new JRadioButton(
        "Large..12 32x32 Sprites + Pal. 48-95"); // get rid of
    JRadioButton jrLarge2 = new JRadioButton(
        "New Large...16 32x32 Sprites + Pal. 48-95"); // get rid of
    JRadioButton jrLarge3 = new JRadioButton(
        "New Large 2...32 32x32 Sprites + Pal. 48-95");
    JRadioButton jrWeapon = new JRadioButton(
        "Weapon..16 24x16 Sprites + Pal. 224-255");
    JRadioButton jrFx = new JRadioButton(
        "Effects..36 16x16 Tiles + Pal. 96-127");
    JRadioButton jrPor = new JRadioButton(
        "Portraits..4 96x96 Sprites, no palette");

    ButtonGroup myGroup = new ButtonGroup();
    myGroup.add(jrTiles);
    myGroup.add(jrMemb);
    myGroup.add(jrNewMemb);
    myGroup.add(jrNpc);
    myGroup.add(jrLarge);
    myGroup.add(jrLarge2);
    myGroup.add(jrLarge3);
    myGroup.add(jrWeapon);
    myGroup.add(jrFx);
    myGroup.add(jrPor);

    myDialog.add(jrTiles);
    myDialog.add(jrMemb);
    myDialog.add(jrNewMemb);
    myDialog.add(jrNpc);
    myDialog.add(jrLarge);
    myDialog.add(jrLarge2);
    myDialog.add(jrLarge3);
    myDialog.add(jrWeapon);
    myDialog.add(jrFx);
    myDialog.add(jrPor);

    JButton okButton = new JButton("OK");
    okButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        myDialog.dispose();
      }

    });
    myDialog.add(okButton);

    myDialog.pack();
    myDialog.setModalityType(Dialog.DEFAULT_MODALITY_TYPE);
    myDialog.setVisible(true);

    if (jrTiles.isSelected()) {
      this.attrs = TileOptions.TILES;

    } else if (jrMemb.isSelected()) {
      this.attrs = TileOptions.MEMBERS;

    } else if (jrNewMemb.isSelected()) {
      this.attrs = TileOptions.NEW_MEMBERS;

    } else if (jrNpc.isSelected()) {
      this.attrs = TileOptions.NPC;

    } else if (jrLarge.isSelected()) {
      this.attrs = TileOptions.LARGE;

    } else if (jrLarge2.isSelected()) {
      this.attrs = TileOptions.NEW_LARGE;

    } else if (jrLarge3.isSelected()) {
      this.attrs = TileOptions.NEW_LARGE_2;

    } else if (jrWeapon.isSelected()) {
      this.attrs = TileOptions.WEAPON;

    } else if (jrFx.isSelected()) {
      this.attrs = TileOptions.EFFECTS;

    } else if (jrPor.isSelected()) {
      this.attrs = TileOptions.PORTRAITS;
    }

    this.displayOptions();

  }

  private void displayOptions() {
    System.out.println(attrs.height + " " + attrs.width + " " + attrs.count
        + " " + attrs.palStart + " " + attrs.palEnd);

  }

  public TileAttributes getAttrs() {
    return this.attrs;
  }

}