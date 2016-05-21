// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Parse Google Fonts API results to assist in saving local copies.

package bdzimmer.secondary.export.controller

import java.io.File
import java.net.URL
import scala.util.matching.Regex
import scala.util.Try

import org.apache.commons.io.IOUtils

import bdzimmer.util.StringUtils._


object Fonts {

  val GoogleFontsUrl = "https://fonts.googleapis.com/css?family=";
  val UrlMatcher = "url\\((.*?)\\)".r


  def convert(fontDescription: String): (String, List[(String, String)]) = {

    Try({

      val queryUrl = GoogleFontsUrl + fontDescription
      val fontCss = IOUtils.toString(new URL(queryUrl))

      val fontUrls =  UrlMatcher.findAllMatchIn(fontCss).map(m => {
        val url = m.group(1)
        (url, new File(new URL(url).getPath).getName)
      }).toList

      val updatedFontCss = UrlMatcher.replaceAllIn(fontCss, m => {
        "url(" + new File(new URL(m.group(1)).getPath).getName + ")"
      })

      (updatedFontCss, fontUrls)

    }).toOption.getOrElse("", List())

  }

}
