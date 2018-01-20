// Copyright (c) 2018 Ben Zimmer. All rights reserved.

// Render HTML pages for items.

package bdzimmer.secondary.export.controller

import scala.util.Try

import bdzimmer.secondary.export.model.{ Tags, WorldItems }
import bdzimmer.secondary.export.model.WorldItems._
import bdzimmer.secondary.export.view.Html._
import bdzimmer.secondary.export.view.{ Markdown, PageTemplates }


class RenderPages(
    master: CollectionItem,
    world: List[WorldItem],
    tags: Map[String, Map[Int, Tags.ParsedTag]],
    wikiCache: FilesystemCache,
    license: String,
    navbars: Boolean) {

  // derive some data structures that are used repeatedly throughout the rendering process

  // outward references
  val references = world.map(item =>
    (item.id, itemToTags(item).values.flatMap(x => Tags.items(x)).toList)).toMap

  // inward references
  val referencedBy = world.map(item => {
    (item.id, world.filter(otherItem =>
      !otherItem.id.equals(item.id) &&
        itemToTags(otherItem).values.flatMap(x => Tags.items(x)).exists(x => item.id.equals(x.id))))
  }).toMap

  // previous, next, and parent items
  val collections = world.collect({ case x: CollectionItem => x })

  val previous = collections.map(collection => {
    collection.children.drop(1).map(_.id).zip(collection.children.dropRight(1))
  }).flatten.toMap

  val next = collections.map(collection => {
    collection.children.dropRight(1).map(_.id).zip(collection.children.drop(1))
  }).flatten.toMap

  val parent = collections.map(collection => {
    collection.children.map(x => (x.id, collection))
  }).flatten.toMap

  // the main collection for each each item
  val groups = master.children.flatMap(group => WorldItems.collectionToList(group).map(item => (item.id, group))).toMap

  val stringToTags = (
    world.map(x => (x.id, tags.get(x.id)))
    ++ world.map(x => (x.name, tags.get(x.id)))).collect({ case (x, Some(y)) => (x, y) }).toMap

  val np = new RenderTags(stringToTags, world.collect({ case x: CharacterItem => x }))

  private def itemToTags(item: WorldItem): Map[Int, Tags.ParsedTag] = {
    tags.get(item.id).getOrElse(Map())
  }


  // generate page contents
  def itemPageDispatch(item: WorldItem) = item match {
    case x: CollectionItem => collectionPage(x)
    case x: CharacterItem  => characterPage(x)
    case x: ImageFileItem  => imagePage(x)
    case x: TripItem       => imagePage(x)
    case x: MapItem        => pixelArtPage(x)
    case x: TileRefItem    => pixelArtPage(x)
    case _                 => itemPage(item)
  }

  /// /// ///

  def masterPage(): String = {

    PageTemplates.articlePage(
      master.name,
      master.description,
      pageNavbar(master),

      column(Column12,
        np.transform(master.notes, itemToTags(master)) +
          (if (master.children.length > 0) {
            hr +
              master.children.collect({
                case curCollection: CollectionItem => {
                  column(Column6,
                    """<h3 style="display: inline-block">""" + curCollection.name + "</h3>" + nbsp +
                      RenderPages.glyphLinkPage(curCollection) + "\n" +
                      listGroup(curCollection.children.map(x =>
                        RenderPages.getCollectionLinksCollapsible(x))))
                }
              }).grouped(2).map(_.mkString("\n") +
                """<div class="clearfix"></div>""" + "\n").mkString("\n")
          } else {
            ""
          })),

      license)

  }


  private def collectionPage(collection: CollectionItem): String = {

    PageTemplates.articlePage(
      collection.name,
      collection.description,
      pageNavbar(collection),

      column(Column12,
        np.transform(collection.notes, itemToTags(collection)) + refItems(collection) + hr +
          (if (collection.children.length > 0) {
            h4("Subarticles") +
              listGroup(collection.children.map(x => RenderPages.getCollectionLinksCollapsible(x)))
          } else {
            ""
          })),

      license)
  }


  private def characterPage(character: CharacterItem): String = {

    PageTemplates.articlePage(
      character.name,
      character.description,
      pageNavbar(character),

      column(Column12, np.transform(character.notes, itemToTags(character)) + refItems(character)),

      license)
  }


  private def imagePage(imageItem: ImageItem): String = {

    // TODO: download the wikimedia JSON to a file when image is downloaded.
    // That way, an internet connection will only be required for the first export
    // of a wikimedia image.

    val imageDescription = imageItem match {
      case imageFileItem: ImageFileItem => {
        if (imageFileItem.filename.startsWith("wikimedia:")) {
          (for {
            wikiName <- Try(imageFileItem.filename.split(":")(1)).toOption
            // json     <- ImageDownloader.getWikimediaJson(wikiName)
            json <- wikiCache(wikiName)
            wm <- ImageDownloader.parseWikimediaJson(json)

            description = "Artist: " + wm.artist + br +
              "License: " + wm.license + br +
              (if (wm.attribution) "Attribution required." + br else "") +
              link("Source", wm.descriptionurl) + hr

          } yield description).getOrElse("")
        } else {
          ""
        }
      }
      case _ => ""
    }

    PageTemplates.articlePage(
      imageItem.name, imageItem.description,
      pageNavbar(imageItem),

      column(Column8, image(RenderImages.imagePath(imageItem), responsive = true)) +
        column(Column4, "") +
        column(Column12,
          hr +
            imageDescription +
            np.transform(imageItem.notes, itemToTags(imageItem)) + refItems(imageItem)),
      license)
  }


  private def pixelArtPage(imageItem: ImageItem): String = {

    PageTemplates.articlePage(
      imageItem.name,
      imageItem.description,
      pageNavbar(imageItem),

      column(Column12, RenderImages.pixelImageLinkResponsive(imageItem) + hr) +
        column(Column12, np.transform(imageItem.notes, itemToTags(imageItem)) + refItems(imageItem)),

      license)
  }


  private def itemPage(item: WorldItem): String = {

    PageTemplates.articlePage(
      item.name, item.description,
      pageNavbar(item),
      column(Column12, np.transform(item.notes, itemToTags(item)) + refItems(item)),
      license)
  }


  // get a navbar for an article page for a world item
  private def pageNavbar(item: WorldItem): Option[String] = navbars match {
    case true => {

      val relLinks = if (!master.equals(item) && !master.children.contains(item)) {
        // previous / parent / next
        List(
          previous.get(item.id).map(y => PageTemplates.LeftArrow + RenderPages.textLinkPage(y)),
          parent.get(item.id).map(RenderPages.textLinkPage),
          next.get(item.id).map(y => RenderPages.textLinkPage(y) + PageTemplates.RightArrow)).flatten
      } else {
        List()
      }

      val home = List(link("Home", RenderPages.MasterPageFile))
      val mainCollectionLinks = master.children.map(RenderPages.textLinkPage)
      val bar = (home ++ relLinks ++ mainCollectionLinks).mkString(PageTemplates.NavbarSeparator) + hr

      Some(bar)
    }
    case false => None
  }


  private def refItems(item: WorldItem): String = {
    val refs = referencedBy.get(item.id) match {
      case Some(x) => x
      case None    => List()
    }

    if (refs.length > 0) {
      h4("Referenced By") +
        listGroup(refs.sortBy(_.name).map(x => listItem(RenderPages.textLinkPage(x))))
    } else {
      ""
    }
  }

}



object RenderPages {

  val MasterPageFile = "index.html"

  // recursively generate nested lists of links from a CollectionItem
  def getCollectionLinks(item: WorldItem): String = item match {
    case x: CollectionItem => listItem(textLinkPage(x) + listGroup(x.children.map(x => getCollectionLinks(x))))
    case _                 => listItem(textLinkPage(item))
  }


  // recursively generated nested lists of links with descriptions from a CollectionItem
  def getCollectionLinksWithDescription(item: WorldItem): String = item match {
    case x: CollectionItem => listItem(
      textLinkPage(x) +
        (if (x.description.length > 0) " - " + Markdown.processLine(x.description) else "") +
        listGroup(x.children.map(x => getCollectionLinksWithDescription(x))))
    case _ => listItem(
      textLinkPage(item) +
        (if (item.description.length > 0) " - " + Markdown.processLine(item.description) else ""))
  }


  // recursively generated collapsible lists
  def getCollectionLinksCollapsible(item: WorldItem): String = item match {
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

    val pullClass = if (pullRight) " pull-right" else ""
    val leftMargin = if (pullRight) "margin-left:32px;" else ""

    val borderStyle = if (border) "" else "border: 0; box-shadow: 0 0 0; border-radius: 0;"

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

}
