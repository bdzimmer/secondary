// Copyright (c) 2018 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.export.controller

import bdzimmer.secondary.export.model.WorldItems.WorldItem
import bdzimmer.secondary.export.model.WorldItems
import bdzimmer.secondary.export.view.Html._
import bdzimmer.secondary.export.view.WebResource
import bdzimmer.secondary.export.view.Markdown



object Search {
  
  val SearchStyles =
    s"""<link href="${WebResource.SearchCss.localRelFilename}" rel="stylesheet">""" + "\n" +
    s"""<script src="${WebResource.SearchJs.localRelFilename}" charset="utf-8"></script>""" + "\n"
  
    
  def render(id: String, items: List[WorldItem]): String = {
      SearchStyles + 
      "<script>\n" +
      "var " + id + "Items = [" + items.map(x => Search.itemToJs(x)).mkString(",\n") + "];\n" +
      "search(\"" + id + "\", " + id + "Items);\n" + 
      "</script>\n"
  }
    
    
  def itemToJs(item: WorldItem): String = {
    val tags = item match {
      case x: WorldItems.CharacterItem => x.nameParts.getOrElse(List(x.name))
      case _                           => List(item.name)
    }
    
    s"""{
  "id": "${item.id}",
  "name": "${Markdown.processLine(item.name)}",
  "link": "${RenderPages.itemPageName(item)}",
  "tags": [${tags.map(x => "\"" + x.toLowerCase.replace("*", "") + "\"").mkString(", ")}]
}"""
    
  }
  
  
}