// Copyright (c) 2019 Ben Zimmer. All rights reserved.

// Prepare minimally-formated HTML documents suitable for automated conversion to doc / docx.

package bdzimmer.secondary.export.controller

import bdzimmer.secondary.export.model.Tags.ParsedTag


object Basic {

    def export(
        filename: String,
        notes: String,
        title: String,
        tags: Map[Int, ParsedTag],
        renderTags: RenderTags,
        localExportPath: String): Unit = {

      // get sections and ignore potential cover image
      val (sections, _) = Book.sections(notes, tags, Some(renderTags))

      // assume that the first section that comes back from sections
      // is the title page and toss it
      // TODO: detect whether there is a title page

      val body = sections.tail.map(_.content).mkString("\n")

      val contents = (
s"""<!DOCTYPE html>
<html lang="en">
  <head>
    <title>${title}</title>
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
