// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Class for page exports. Refactored from Export.

// 2015-08-09: Created.
// 2015-08-27: Image page creation.
// 2015-08-31: Enhanced character image functionality.
// 2015-09-01: More on above.
// 2015-09-02: More image functions for jumbotron functionality (needs cleaning up).
//             Master page and article pages all use the same template.
// 2015-09-08: Tasks page uses tags.

package bdzimmer.secondary.export.controller

import java.io.File

import scala.collection.JavaConverters.asScalaBufferConverter

import com.google.api.client.util.DateTime
import org.apache.commons.io.FilenameUtils

import bdzimmer.secondary.export.model._
import bdzimmer.secondary.export.view.Tags._
import bdzimmer.secondary.export.view.{Markdown, PageTemplates}



class ExportPages(
    master: CollectionItem,
    world: List[WorldItem],
    val location: String,
    license: String) {

  val np = new RenderSecTags(world)

  val metaItems = WorldItem.filterList[MetaItem](world)


  def exportPagesList(items: List[WorldItem]): List[String] = {
    items map(item => exportPageDispatch(item)) filter (!_.equals(""))
  }


  def exportPageDispatch(item: WorldItem): String = item match {
    case x: CollectionItem => createCollectionPage(x)
    case x: CharacterItem => createCharacterPage(x)
    case x: ImageItem => createImagePage(x)
    case x: MapItem => createMapPage(x)
    case x: TileMetaItem => createTilePage(x)
    case _ => createItemPage(item)
  }


  /// /// ///

  def createMasterPage(): String = {

    val relFilePath = ExportPages.MasterPageFile

    val toolbar = Some(List(
            link("Index", ExportPages.IndexPageFile),
            // link("Family Trees", ExportPages.FamilyTreesPageFile),
            link("Tasks", ExportPages.TasksPageFile),
            link("Edit",
                ExportPages.notepadURL(master))).mkString("&nbsp;&nbsp;&middot;&nbsp;&nbsp;") + hr)

    PageTemplates.createArticlePage(
        location + File.separator + relFilePath,
        master.name,
        master.description,

        toolbar,

        column(Column12, np.transform(master.notes) + hr +
        master.children.map(x => {

            val curCollection = x.asInstanceOf[CollectionItem]

            column(Column6,
              "<h3>" + curCollection.name + "</h3>\n" +
              listGroup(curCollection.children
              //   .map(x => ExportPages.getCollectionLinksWithDescription(x))))
                .map(x => ExportPages.getCollectionLinksCollapsible(x))))

          }).grouped(2).map(_.mkString("\n") + """<div class="clearfix"></div>""" + "\n").mkString("\n")),

        license)

    relFilePath

  }



  def createTasksPage(): String = {

    val relFilePath = ExportPages.TasksPageFile

    def taskList(todoFunc: WorldItem => List[String]): String = {
      listGroup(world
              .map(x => (x, todoFunc(x)))
              .filter(_._2.length > 0)
              .map(x => listItem(ExportPages.notepadLink(x._1) + "&nbsp;" + ExportPages.textLinkPage(x._1) +
                  listGroup(x._2.map(text => listItem(Markdown.processLine(text)))))))
    }

    // get task strings
    def getTask(item: WorldItem)(prefix: String): List[String] = {
      item.tags.filter(_.kind.equals(prefix)).map(_.value)
    }

    PageTemplates.createArticlePage(
        location + File.separator + relFilePath,
        "Tasks", "",
        None,

       // Todos and thoughts
       column(Column6, "<h3>To-dos</h3>\n" + taskList(getTask(_)("todo"))) +

       // Thoughts
       column(Column6, "<h3>Thoughts</h3>\n" + taskList(getTask(_)("thought"))) +

       // Empty notes
       column(Column6,
         "<h3>Empty Notes</h3>\n" +
         listGroup(world
           .filter(_.notes.equals(""))
           .map(x => listItem(ExportPages.notepadLink(x) + "&nbsp;" + ExportPages.textLinkPage(x))))
       ),

       license)

    relFilePath
  }



  def createIndexPage(): String = {

    val relFilePath = ExportPages.IndexPageFile

    PageTemplates.createArticlePage(

        location + File.separator + relFilePath,
        "Index", "",

        None,

        column(Column6, {
            val groupedItems = (world
              .drop(1)  // get rid of master collection - assumes master is first in list
              .filter(!_.isInstanceOf[MetaItem])
              .groupBy(_.name.replaceAll("""\p{Punct}""", "")(0).toUpper))

            groupedItems.toList.sortBy(_._1).map({case (letter, items) => {
              "<h4>" + letter + "</h4>\n" +
              listGroup(
                items.sortBy(_.name).map(item => listItem(ExportPages.textLinkPage(item)))
              )
            }}).mkString("\n")
          }

        ),

        license)

    relFilePath
  }


  ///

  private def createCharacterPage(character: CharacterItem): String = {

    val relFilePath = ExportPages.itemPageName(character)

    PageTemplates.createArticlePage(
        location + File.separator + relFilePath,
        character.name, character.description,

        Some(ExportPages.getToolbar(character)),

        column(Column12,
            ExportPages.panel(
                ExportImages.imageLinkPage(character, metaItems, false, 320, false, 12), true, false) +
            np.transform(character.notes)),

        license)


    relFilePath
  }



  private def createMapPage(map: MapItem): String = {

    val relFilePath = ExportPages.itemPageName(map)

    PageTemplates.createArticlePage(
        location + File.separator + relFilePath,
        map.name, map.description,

        Some(ExportPages.getToolbar(map)),

        column(Column12, ExportImages.pixelImageLinkResponsive(map) + hr) +
        column(Column12, np.transform(map.notes)),

        license)

    relFilePath
  }



  private def createTilePage(tileset: TileMetaItem): String = {

    val relFilePath = ExportPages.itemPageName(tileset)

    PageTemplates.createArticlePage(
        location + File.separator + relFilePath,
        tileset.name, tileset.description,

        Some(ExportPages.getToolbar(tileset)),

        column(Column12, ExportImages.pixelImageLinkResponsive(tileset) + hr) +
        column(Column12, np.transform(tileset.notes)),

        license)

    relFilePath
  }



  private def createCollectionPage(collection: CollectionItem): String = {

    val relFilePath = ExportPages.itemPageName(collection)

    PageTemplates.createArticlePage(
        location + File.separator + relFilePath,
        collection.name, collection.description,
        Some(ExportPages.getToolbar(collection)),

        column(Column12, np.transform(collection.notes) + hr) +

        // links to child pages with images
        collection.children.map(x => {
          column(Column3, ExportImages.imageLinkPage(x, metaItems, responsive = true))
        }).grouped(4).map(_.mkString("\n") + """<div class="clearfix"></div>""" + "\n").mkString("\n"),

        license)

    relFilePath
  }



  private def createImagePage(imageItem: ImageItem): String = {

    val relFilePath = ExportPages.itemPageName(imageItem)

    val wikiNameOption = (imageItem.filename.startsWith("wikimedia:") match {
      case true => Some(imageItem.filename.split(":")(1))
      case false => None
    })

    val licenseDescription = (for {
      wikiName <- wikiNameOption
      json <- ImageDownloader.getWikimediaJson(wikiName)
      wm <- ImageDownloader.parseWikimediaJson(json)
      description =
        "Artist: " + wm.artist + br +
        "License: " + wm.license + br +
        (if (wm.attribution) "Attribution required." + br else "") +
        link("Source", wm.descriptionurl) + hr
    } yield description).getOrElse("")

    PageTemplates.createArticlePage(
        location + File.separator + relFilePath,
        imageItem.name, imageItem.description,
        Some(ExportPages.getToolbar(imageItem)),

        column(Column8, image(ExportImages.imageItemImagePath(imageItem), responsive = true)) +
        column(Column4, "") +
        column(Column12,
            hr +
            licenseDescription +
            np.transform(imageItem.notes)),
        license)

    relFilePath
  }


  private def createItemPage(item: WorldItem): String = {

    val relFilePath = ExportPages.itemPageName(item)

    PageTemplates.createArticlePage(
        location + File.separator + relFilePath,
        item.name, item.description,
        Some(ExportPages.getToolbar(item)),
        column(Column12,
            np.transform(item.notes)),
        license )

    relFilePath
  }


}



object ExportPages {

  // constants

  val MasterPageFile = "index.html"
  val IndexPageFile = "indexpage.html"
  val TasksPageFile = "tasks.html"
  // val FamilyTreesPageFile = "familytrees.html"

  // recursively generate nested lists of links from a CollectionItem
  def getCollectionLinks(item: WorldItem): String =  item match {
    case x: CollectionItem => listItem(textLinkPage(x) + listGroup(x.children.map(x => getCollectionLinks(x))))
    case _ => listItem(textLinkPage(item))
  }


  // recursively generated nested lists of links with descriptions from a CollectionItem
  def getCollectionLinksWithDescription(item: WorldItem): String =  item match {
    case x: CollectionItem => listItem(
        textLinkPage(x) +
        (if (x.description.length > 0 ) " - " + Markdown.processLine(x.description) else "") +
        listGroup(x.children.map(x => getCollectionLinksWithDescription(x))))
    case _ => listItem(
        textLinkPage(item) +
        (if (item.description.length > 0 ) " - " + Markdown.processLine(item.description) else ""))
  }


  // recursively generated collapsible lists
  def getCollectionLinksCollapsible(item: WorldItem): String =  item match {
    case x: CollectionItem => {

      val collapsibleLink = s"""<input type="checkbox" id="${x.id}" class="swivel" />""" +
         "\n" + textLinkPage(x) + s"""<label for="${x.id}"></label>"""

      listItem(
          collapsibleLink + listGroup(x.children.map(x => getCollectionLinksCollapsible(x))),
          className = "swivel")

    }
    case _ => listItem(textLinkPage(item))
  }



  // panel that can be pulled right
  def panel(contents: String, pullRight: Boolean = false, border: Boolean = true): String = {

    val pullClass = pullRight match {
      case true => " pull-right"
      case false => ""
    }

    val leftMargin = pullRight match {
      case true => "margin-left:32px;"
      case false => ""
    }

    val borderStyle = border match {
      case true => ""
      case false => "border: 0; box-shadow: 0 0 0; border-radius: 0;"
    }

    (s"""<div class="panel panel-default${pullClass}" style="${leftMargin} ${borderStyle}"><div class="panel-body">${contents}""" +
    """</div></div>""")

  }


  // add ".html" to the end of the ID
  def itemPageName(x: WorldItem): String = x.id + ".html"


  // generate HTML for a text link to an item's page
  def textLinkPage(item: WorldItem): String = {
    link(Markdown.processLine(item.name), itemPageName(item))
  }

  // glyph link to page
  def glyphLinkPage(item: WorldItem): String = {
    link("""<small><span class="glyphicon glyphicon-link"></span></small>""",
        itemPageName(item))
  }

  // generate HTML for a text link to an item's page with an anchor
  def textLinkPage(item: WorldItem, anchor: String, name: String): String = {
    link(Markdown.processLine(name), itemPageName(item) + "#" + anchor)
  }

  // get a toolbar for an article page for a world item
  def getToolbar(item: WorldItem): String = {
    List(// link("Edit Local", localURL(item, localDriveMapDir)),
         link("Home", "index.html"),
         link("Edit", notepadURL(item))).mkString("&nbsp;&nbsp;&middot;&nbsp;&nbsp;") + hr
  }

  def notepadLink(item: WorldItem): String = {
    link("""<small><span class="glyphicon glyphicon-pencil"></span></small>""", notepadURL(item))
  }

  // get the URL of the YAML source on Drive
  def notepadURL(item: WorldItem): String = {
    "https://drivenotepad.appspot.com/app?state=%7B%22ids%22:%5B%22" + item.remoteid + "%22%5D,%22action%22:%22open%22%7D"
  }


}


