// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.editor.model;

import java.util.ArrayList;

public class World {

  private ArrayList<ScriptFile> scriptFiles = new ArrayList<ScriptFile>();

  private final String contentDir;
  
  /**
   * Create a new World object.
   */
  public World(String contentDir) {
    
    this.contentDir = contentDir;
    
    this.addScriptFile("start.spt");

    // Collections.sort(this.scriptFiles);

    for (ScriptFile curScriptFile : this.scriptFiles) {
      System.out.println(curScriptFile);
    }

  }

  // recursive method to populate list of scripts
  private ScriptFile addScriptFile(String filename) {
    ScriptFile curScriptFile = new ScriptFile(contentDir, filename);

    // if we haven't already done this
    if (!this.scriptFiles.contains(curScriptFile)) {

      this.scriptFiles.add(curScriptFile);

      for (String link : curScriptFile.getLinks()) {
        this.addScriptFile(link);
      }

    }

    return curScriptFile;

  }

  public ArrayList<ScriptFile> getScriptFiles() {
    return this.scriptFiles;
  }

}
