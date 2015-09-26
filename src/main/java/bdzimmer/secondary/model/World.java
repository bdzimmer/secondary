// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.model;

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

  // recursive sub to populate list of scripts
  private ScriptFile addScriptFile(String filename) {
    ScriptFile curScriptFile = new ScriptFile(contentDir, filename);

    // if we haven't already done this
    if (!this.scriptFiles.contains(curScriptFile)) {

      this.scriptFiles.add(curScriptFile);

      /*
       * 
       * //get the scriptfile...now populate its links for (String cLine :
       * cScriptFile.getLines()) { String tempLine = cLine.trim(); String[]
       * cWords = tempLine.split("\\s+");
       * 
       * if (cWords[0].toLowerCase().equals("scriptfile")) {
       * cScriptFile.getLinks().add(addScriptFile(cWords[2] + ".spt"));
       * 
       * }
       * 
       * }
       */

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
