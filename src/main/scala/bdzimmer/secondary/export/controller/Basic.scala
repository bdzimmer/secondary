// Copyright (c) 2019 Ben Zimmer. All rights reserved.

// Prepare minimally-formated HTML documents suitable for automated conversion to doc / docx.

package bdzimmer.secondary.export.controller

import bdzimmer.secondary.export.model.WorldItems.WorldItem
import bdzimmer.secondary.export.model.Tags.ParsedTag


object Basic {

    def export(
        filename: String,
        item: WorldItem,
        tags: Map[Int, ParsedTag],
        renderTags: RenderTags,
        localExportPath: String): Unit = {

      val body = renderTags.transform(item.notes, tags)

      val contents = (
s"""<!DOCTYPE html>
<html lang="en">
  <head>
    <title>${item.name}</title>
  </head>
  <body>
    ${body}
  </body>
</html>""")

      val fileWriter = new java.io.FileWriter(filename, false)
      fileWriter.write(contents)
      fileWriter.close()
    }

}
