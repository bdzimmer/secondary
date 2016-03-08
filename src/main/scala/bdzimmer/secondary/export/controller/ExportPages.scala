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

import bdzimmer.util.Fail
import bdzimmer.util.StringUtils._


class ExportPages(
    master: CollectionItem,
    world: List[WorldItem],
    val location: String,
    license: String,
    navbars: Boolean,
    editLinks: Boolean) {

  val np = new RenderSecTags(world)

  val metaItems = WorldItem.filterList[MetaItem](world)

  // which other items' tags reference each item
  val references = world.map(item => {
    (item.id, world.filter(otherItem => !otherItem.id.equals(item.id) && otherItem.tags.exists(x => {
      (item.id.equals(x.value) || item.name.equals(x.value))
    })))
  }).toMap


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

    PageTemplates.createArticlePage(
        location / relFilePath,
        master.name,
        master.description,
        masterNavbar,

        column(Column12,
            np.transform(master.notes) +
            (if (master.children.length > 0) {
              hr +
              master.children.collect({case curCollection: CollectionItem => {
                  column(Column6,
                      """<h3 style="display: inline-block">""" + curCollection.name + "</h3>" + nbsp +
                      ExportPages.glyphLinkPage(curCollection) + "\n" +
                      listGroup(curCollection.children.map(x =>
                        ExportPages.getCollectionLinksCollapsible(x))))
              }}).grouped(2).map(_.mkString("\n") +
              """<div class="clearfix"></div>""" + "\n").mkString("\n")
            } else {
              ""
            })
        ),

        license)

    relFilePath

  }



  def createTasksPage(): String = {

    val relFilePath = ExportPages.TasksPageFile

    def linkWithEdit(x: WorldItem): String = {
      (if (editLinks) ExportPages.notepadLink(x) + nbsp else "") +
      ExportPages.textLinkPage(x)
    }

    def taskList(todoFunc: WorldItem => List[String]): String = {
      listGroup(world
              .map(x => (x, todoFunc(x)))
              .filter(_._2.length > 0)
              .map(x => {
                listItem(
                    linkWithEdit(x._1) +
                    listGroup(x._2.map(text => listItem(Markdown.processLine(text)))))}))
    }

    // get task strings
    def getTask(item: WorldItem)(prefixes: List[String]): List[String] = {
      item.tags.filter(x => prefixes.contains(x.kind)).map(_.value)
    }

    // get invalid tags
    def getInvalidTags(item: WorldItem): List[String] = {
      item.tags.map(np.validateTag(_)).collect({case Fail(x) => x})
    }

    val allTasks = world.flatMap(item => item.tags.filter(x => SecTags.TaskTagKinds.contains(x.kind)).map(x => (x, item)))

    PageTemplates.createArticlePage(
        location / relFilePath,
        "Tasks", "",  pageNavbar(None),

        column(12, Tasks.table(allTasks.map(x => Tasks.createTask(x._1, x._2)))) +

        // Empty notes

        column(
            Column6,
            h4("Empty Notes") +
            listGroup(world
              .filter(_.notes.equals(""))
              .map(x => listItem(linkWithEdit(x))))
        ) +

        // Invalid tags
        column(Column6, h4("Invalid Tags") + taskList(getInvalidTags)) +

        // Todos and thoughts - deprecated soon.
        column(Column6, h4("Thoughts") + taskList(getTask(_)(List(SecTags.Thought)))) +
        column(Column6, h4("Tasks") + taskList(getTask(_)(SecTags.TaskTagKinds))),

        license)

    relFilePath
  }



  def createIndexPage(): String = {

    val relFilePath = ExportPages.IndexPageFile

    def getName(item: WorldItem): String = item match {
      case x: CharacterItem => x.nameParts match {
        case None => x.name
        case Some(parts) => parts match {
          case fst :: snd :: Nil =>  snd + ", " + fst
          case fst :: snd :: rest => snd + ", " + fst + " " + rest.mkString(" ")
          case _ => x.name
        }
      }
      case _ => item.name
    }

    PageTemplates.createArticlePage(

        location / relFilePath,
        "Index", "", pageNavbar(None),

        column(Column6, {
          val groupedItems = (world
            .drop(1)  // get rid of master collection - assumes master is first in list
            .filter(!_.isInstanceOf[MetaItem])
            .groupBy(getName(_).replaceAll("""\p{Punct}""", "")(0).toUpper))

          groupedItems.toList.sortBy(_._1).map({case (letter, items) => {
            h4(letter.toString) +
            listGroup(
              items
                .map(x => (getName(x), x))
                .sortBy(_._1)
                .map(x => listItem(
                    link(Markdown.processLine(x._1), ExportPages.itemPageName(x._2)))))
          }}).mkString(br)
        }),

        license)

    relFilePath
  }



  def createStatsPage(): String = {

    val relFilePath = ExportPages.StatsPageFile

    PageTemplates.createArticlePage(

        location / relFilePath,
        "Stats", "",  pageNavbar(None),

        column(Column12, {

          val wordCount = world.map(_.notes.split("\\s").length).sum
          val tagCount = world.map(_.tags.length).sum

          h4("Counts") +
          p(b("Items: ") + world.length) +
          p(b("Words: ") + wordCount) +
          p(b("Tags: ") + tagCount)
        }),

        license)

    relFilePath
  }



  private def createCharacterPage(character: CharacterItem): String = {

    val relFilePath = ExportPages.itemPageName(character)

    PageTemplates.createArticlePage(
        location / relFilePath,
        character.name,
        character.description,
        pageNavbar(Some(character)),

        column(Column12,
            // ExportPages.panel(
            //    ExportImages.imageLinkPage(character, metaItems, false, 320, false, 12), true, false) +
            np.transform(character.notes) + refItems(character)),

        license)


    relFilePath
  }



  private def createMapPage(map: MapItem): String = {

    val relFilePath = ExportPages.itemPageName(map)

    PageTemplates.createArticlePage(
        location / relFilePath,
        map.name,
        map.description,
        pageNavbar(Some(map)),

        column(Column12, ExportImages.pixelImageLinkResponsive(map) + hr) +
        column(Column12, np.transform(map.notes) + refItems(map)),

        license)

    relFilePath
  }



  private def createTilePage(tileset: TileMetaItem): String = {

    val relFilePath = ExportPages.itemPageName(tileset)

    PageTemplates.createArticlePage(
        location / relFilePath,
        tileset.name,
        tileset.description,
        pageNavbar(Some(tileset)),

        column(Column12, ExportImages.pixelImageLinkResponsive(tileset) + hr) +
        column(Column12, np.transform(tileset.notes) + refItems(tileset)),

        license)

    relFilePath
  }



  private def createCollectionPage(collection: CollectionItem): String = {

    val relFilePath = ExportPages.itemPageName(collection)

    PageTemplates.createArticlePage(
        location / relFilePath,
        collection.name,
        collection.description,
        pageNavbar(Some(collection)),

        column(Column12,
            np.transform(collection.notes) + refItems(collection) + hr +
            (if (collection.children.length > 0) {
              h4("Subarticles") +
              listGroup(collection.children.map(x => ExportPages.getCollectionLinksCollapsible(x)))
            } else {
              ""
            })),

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
        location / relFilePath,
        imageItem.name, imageItem.description,
        pageNavbar(Some(imageItem)),

        column(Column8, image(ExportImages.imageItemImagePath(imageItem), responsive = true)) +
        column(Column4, "") +
        column(Column12,
            hr +
            licenseDescription +
            np.transform(imageItem.notes) + refItems(imageItem)),
        license)

    relFilePath
  }


  private def createItemPage(item: WorldItem): String = {

    val relFilePath = ExportPages.itemPageName(item)

    PageTemplates.createArticlePage(
        location / relFilePath,
        item.name, item.description,
        pageNavbar(Some(item)),
        column(Column12, np.transform(item.notes) + refItems(item)),
        license)

    relFilePath
  }


  private def masterNavbar(): Option[String] = navbars match {
    case true => {
      val links = List(
          link("Index", ExportPages.IndexPageFile),
          link("Tasks", ExportPages.TasksPageFile),
          link("Stats", ExportPages.StatsPageFile))
      val linksWidthEdit = editLinks match {
        case true =>  links :+ link("Edit",  ExportPages.notepadURL(master))
        case false => links
      }
      val mainCollectionLinks = master.children.map(ExportPages.textLinkPage)
      val bar = (linksWidthEdit ++ mainCollectionLinks).mkString(PageTemplates.NavbarSeparator) + hr
      Some(bar)
    }
    case false => None
  }


  // get a navbar for an article page for a world item
  private def pageNavbar(item: Option[WorldItem]): Option[String] = navbars match {
    case true => {
      val links = List(link("Home", ExportPages.MasterPageFile))
      val linksWidthEdit = editLinks match {
        case true =>  links ++ item.map(x => link("Edit",  ExportPages.notepadURL(x)))
        case false => links
      }
      val mainCollectionLinks = master.children.map(ExportPages.textLinkPage)
      val bar = (linksWidthEdit ++ mainCollectionLinks).mkString(PageTemplates.NavbarSeparator) + hr
      Some(bar)
    }
    case false => None
  }


  private def refItems(item: WorldItem): String = {
    val refs = references.get(item.id) match {
      case Some(x) => x
      case None => List()
    }

    if (refs.length > 0) {
       h4("Referenced By") +
       listGroup(refs.sortBy(_.name).map(x => listItem(ExportPages.textLinkPage(x))))
    } else {
      ""
    }
  }


}



object ExportPages {

  // constants
  val MasterPageFile = "index.html"
  val IndexPageFile = "indexpage.html"
  val TasksPageFile = "tasks.html"
  val StatsPageFile = "stats.html"

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
    link("""<small><span class="glyphicon glyphicon-link" style="display: inline"></span></small>""",
        itemPageName(item))
  }

  // generate HTML for a text link to an item's page with an anchor
  def textLinkPage(item: WorldItem, anchor: String, name: String): String = {
    link(Markdown.processLine(name), itemPageName(item) + "#" + anchor)
  }

  // TODO: may need to add "display: inline" style as in glyph link
  def notepadLink(item: WorldItem): String = {
    link("""<small><span class="glyphicon glyphicon-pencil"></span></small>""", notepadURL(item))
  }

  // get the URL of the YAML source on Drive
  def notepadURL(item: WorldItem): String = {
    "https://drivenotepad.appspot.com/app?state=%7B%22ids%22:%5B%22" + item.remoteid + "%22%5D,%22action%22:%22open%22%7D"
  }

}


