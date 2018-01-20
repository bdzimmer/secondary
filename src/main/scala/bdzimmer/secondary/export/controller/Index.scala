// Copyright (c) 2018 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.export.controller

import bdzimmer.secondary.export.model.WorldItems._
import bdzimmer.secondary.export.view.Html._
import bdzimmer.secondary.export.view.Markdown


object Index {

  // render an index given a list of items
  def render(items: List[WorldItem]): String = {

    val groupedItems = (items
      // .drop(1)  // get rid of master collection - assumes master is first in list
      .filter(!_.isInstanceOf[RefItem])
      .groupBy(getName(_).replaceAll("""\p{Punct}""", "")(0).toUpper)
      .toList
      .sortBy(_._1))

    groupedItems.map({case (letter, items) => {
      h4(letter.toString) +
      listGroup(
        items
          .map(x => (getName(x), x))
          .sortBy(_._1)
          .map(x => listItem(
              link(Markdown.processLine(x._1), RenderPages.itemPageName(x._2)))))
    }}).mkString(br)

  }

  // get formatted name of an item, using nameparts if it's a person
  private def getName(item: WorldItem): String = item match {
    case x: CharacterItem => x.nameParts match {
      case None        => x.name
      case Some(parts) => parts match {
        case fst :: snd :: Nil  => snd + ", " + fst
        case fst :: snd :: rest => snd + ", " + fst + " " + rest.mkString(" ")
        case _                  => x.name
      }
    }
    case _ => item.name
  }

}
