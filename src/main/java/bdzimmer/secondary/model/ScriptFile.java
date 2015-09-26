// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.model;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;


public class ScriptFile implements Comparable<ScriptFile> {

  private final String contentDir;
  private final String fileName;
  private final String title;
  
  private ArrayList<String> lines = new ArrayList<String>();
  private ArrayList<String> links = new ArrayList<String>();
  private ArrayList<String> maps = new ArrayList<String>();

  /**
   * Create a new ScriptFile object.
   * 
   * @param fileName    name of file to load
   */
  public ScriptFile(String contentDir, String fileName) {

    this.contentDir = contentDir;
    this.fileName = fileName;
    
    File inputFile = new File(this.contentDir + File.separator + "script" + File.separator + this.fileName);

    
    // Read in file
    try {

      BufferedReader scriptIn = new BufferedReader(new FileReader(inputFile));

      String curLine;
      while ((curLine = scriptIn.readLine()) != null) {
        lines.add(curLine);

        String tempLine = curLine.trim();
        String[] curWords = tempLine.split("\\s+");

        if (curWords[0].toLowerCase().equals("mapfile")
            && !this.maps.contains(curWords[2])) {
          this.maps.add(curWords[2]);
        }

        if (curWords[0].toLowerCase().equals("scriptfile")) {
          this.links.add(curWords[2] + ".spt");
        }

      }

      scriptIn.close();

    } catch (FileNotFoundException e) {
      System.err.println(e); // print exception if the file doesn't exist.
    } catch (IOException e) {
      System.err.println(e); // print exception if the file doesn't exist.
    }
    
    // Set title of scriptfile object
    this.title = lines.get(0).substring(1, lines.get(0).length());

  }

  
  
  public String getFileName() {
    return this.fileName;
  }

  public String getTitle() {
    return this.title;
  }

  public ArrayList<String> getLines() {
    return this.lines;
  }

  public ArrayList<String> getLinks() {
    return this.links;
  }

  public ArrayList<String> getMaps() {
    return this.maps;
  }

  @Override
  public int compareTo(ScriptFile sf) {
    return this.fileName.compareTo(sf.fileName);
  }

  @Override
  public boolean equals(Object obj) {
    if (obj.getClass() != this.getClass()) {
      return false;
    }
    if (this.compareTo((ScriptFile) obj) != 0) {
      return false;
    }
    return true;

  }

  @Override
  public String toString() {
    return this.fileName.substring(this.fileName.lastIndexOf('/') + 1,
        this.fileName.length()) + ": " + this.title;
  }

}
