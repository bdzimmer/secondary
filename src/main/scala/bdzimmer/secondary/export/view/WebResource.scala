// Copyright (c) 2016 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.export.view

import java.io.File
import java.net.URL

import org.apache.commons.io.FilenameUtils

import bdzimmer.util.StringUtils._


class WebResource(val url: URL, val localRelDirname: String) {
  val filename = new File(url.getPath).getName
  val localRelFilename = localRelDirname / filename
}


object WebResource {

  // directories inside the project web dir

  val StylesRelDirname = "styles"
  val FontsRelDirname  = "fonts"
  val ImagesRelDirname = "images"
  val TreeRelDirname   = "tree"

  // generated stylesheet and other scripts that aren't web resources

  val MainStylesheet  = StylesRelDirname / "secondary.css"
  val FontsStylesheet = FontsRelDirname  / "fonts.css"

  // web resources to copy out of the project JAR

  val TreeJs = new WebResource(
      getClass.getResource("/tree/drawtree.js"),
      TreeRelDirname)

  val TreeCss = new WebResource(
      getClass.getResource("/tree/tree.css"),
      TreeRelDirname)

  // web resources to download from the internet

  val BootstrapZip = WebResource(
      "https://github.com/twbs/bootstrap/releases/download/v3.3.5/bootstrap-3.3.5-dist.zip",
      StylesRelDirname)

  val BootstrapExtracted = FilenameUtils.removeExtension(BootstrapZip.localRelFilename)
  val BootstrapJs  = BootstrapExtracted / "js/bootstrap.min.js"
  val BootstrapCss = BootstrapExtracted / "css/bootstrap.min.css"

  val Jquery = WebResource(
      "https://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js",
      StylesRelDirname)

  val D3 = WebResource(
      "https://cdnjs.cloudflare.com/ajax/libs/d3/3.5.6/d3.min.js",
      StylesRelDirname)

  private val DataTablesBase = "https://cdn.datatables.net/1.10.11"

  val DataTablesJs = WebResource(
      DataTablesBase / "js/jquery.dataTables.min.js",
      StylesRelDirname)

  val DataTablesCss = WebResource(
      DataTablesBase / "css/jquery.dataTables.min.css",
      StylesRelDirname)

  val SortAsc = WebResource(
      DataTablesBase / "images/sort_asc.png",
      ImagesRelDirname)

  val SortDesc = WebResource(
      DataTablesBase / "images/sort_desc.png",
      ImagesRelDirname)

  val SortBoth = WebResource(
      DataTablesBase / "images/sort_both.png",
      ImagesRelDirname)


  def apply(urlString: String, localRelDirname: String): WebResource = {
    val url = new URL(urlString)
    new WebResource(url, localRelDirname)
  }

}
