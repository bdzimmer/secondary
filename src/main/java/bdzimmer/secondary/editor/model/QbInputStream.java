// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.editor.model;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;

public class QbInputStream extends DataInputStream {

  public QbInputStream(InputStream is) {
    super(is);
  }
  
  
  /**
   * Read an unsigned byte as an int.
   */
  public int readQbUnsignedByte() throws IOException {
    return 0x000000FF & (int)readByte();
  }
  
  /**
   * Read an unsigned short and return the low byte as an int.
   */
  public int readQbUnsignedShortLow() throws IOException {
    final int result = 0x000000FF & (int)readByte();
    readByte();
    return result;
  }

}
