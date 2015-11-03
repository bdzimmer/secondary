// Copyright (c) 2015 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.editor.model

import org.apache.commons.io.FileUtils
import java.io.File
import scala.collection.JavaConverters._

case class AssetMetadata(id: String, assetType: String, name: String, filename: String, info: String)

object AssetMetadataUtils {

  val FieldSep = "\t"

  // save asset metadata
  def saveAssetMetadata(filename: String, items: List[AssetMetadata]): Unit = {
    val pw = new java.io.PrintWriter(new File(filename))
    items.foreach(x => {
      // scalastyle:ignore regex
      pw.println(
          x.id + FieldSep
          + x.assetType + FieldSep
          + x.name + FieldSep
          + x.filename + FieldSep
          + x.info)
    })
    pw.close
  }

  // load asset metadata
  def loadAssetMetadata(filename: String): List[AssetMetadata] = {
    val lines = FileUtils.readLines(new File(filename), "UTF-8").asScala
    lines.map(line => {
      val x = line.split("\t")
      AssetMetadata(x(0), x(1), x(2), x(3), x(4))
    }).toList
  }

}
