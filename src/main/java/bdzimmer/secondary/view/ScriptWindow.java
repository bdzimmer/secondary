// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.view;

import bdzimmer.secondary.model.ScriptFile;

import java.util.ArrayList;

import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.AbstractTableModel;



public class ScriptWindow extends JFrame {

  private static final long serialVersionUID = 1L;

  private ScriptFile scriptFile;

  private JTable table;

  /**
   * Create a new script window from a ScriptFile object.
   * 
   * @param scriptFile  object to create window from
   */
  public ScriptWindow(ScriptFile scriptFile) {
    this.scriptFile = scriptFile;

    this.setTitle(this.scriptFile.getFileName());

    table = new JTable(new ScriptTableModel(this.scriptFile));

    JScrollPane scrollPane = new JScrollPane(table);
    table.setFillsViewportHeight(true);
    this.setContentPane(scrollPane);

    this.setVisible(true);

  }

  class ScriptTableModel extends AbstractTableModel {

    private static final long serialVersionUID = 1L;

    private ArrayList<String> lines;

    public ScriptTableModel(ScriptFile scriptFile) {
      this.lines = scriptFile.getLines();
    }

    @Override
    public int getColumnCount() {
      return 1;
    }

    @Override
    public int getRowCount() {
      return this.lines.size();
    }

    @Override
    public Object getValueAt(int rowIndex, int columnIndex) {
      if (columnIndex == 0) {
        return this.lines.get(rowIndex);
      }
      return null;
    }

  }
}
