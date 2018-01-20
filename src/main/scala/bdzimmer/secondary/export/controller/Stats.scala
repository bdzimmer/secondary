// Copyright (c) 2018 Ben Zimmer. All rights reserved.

package bdzimmer.secondary.export.controller

import bdzimmer.secondary.export.model.WorldItems.WorldItem
import bdzimmer.secondary.export.model.WorldItems
import bdzimmer.secondary.export.view.Html._


object Stats {

  def render(item: WorldItem): String = {

    val items = WorldItems.collectionToList(item)

    val wordCount = items.map(_.notes.split("\\s").length).sum
    val tagCount  = items.map(_.tags.size).sum

    h4("Counts") +
    p(b("Items: ") + items.length) +
    p(b("Words: ") + wordCount) +
    p(b("Tags: ")  + tagCount)

  }
}
