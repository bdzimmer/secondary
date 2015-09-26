// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.model;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;

public class QbOutputStream extends DataOutputStream {

  public QbOutputStream(OutputStream os) {
    super(os);
  }
  
  // write an int as an unsigned byte
  public void writeQbUnsignedByte(int xb) throws IOException {
    writeByte(xb & 0xFF);
  }
 
  // write an int as an unsigned short, discarding the high byte
  public void writeQbUnsignedShortLow(int xb) throws IOException {
    writeByte(xb & 0xFF);
    writeByte(0);
  }
  
}
