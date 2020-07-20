// Copyright (c) 2018 Ben Zimmer. All rights reserved.

// Render HTML pages for items.

package bdzimmer.secondary.export.controller

import scala.util.Try

import bdzimmer.secondary.export.model.{ Tags, WorldItems }
import bdzimmer.secondary.export.model.WorldItems._
import bdzimmer.secondary.export.view.Html._
import bdzimmer.secondary.export.view.Bootstrap._
import bdzimmer.secondary.export.view.{ Markdown, PageTemplates, Styles }


class RenderPages(
    master: CollectionItem,
    world: List[WorldItem],
    tagsMap: Map[Int, Map[Int, Tags.ParsedTag]],
    wikiCache: FilesystemCache,
    license: String,
    navbars: Boolean,
    subarticles: Boolean,
    relativeLinks: Boolean,
    hiddenItems: List[WorldItem],
    unifiedJumbotron: Boolean,
    search: Boolean) {

  // derive some data structures that are used repeatedly throughout the rendering process

  // outward references
  val references: Map[Int, List[WorldItem]] = Timer.showTimeBrief("outward refs",
    (for {
      item <- world
      tag <- itemToTags(item).values
      tagItem <- Tags.items(tag)
    } yield {
      (item.uid, tagItem)
    })
    .filter(x => !(x._1.equals(x._2.uid)))
    .groupBy(_._1)
    .mapValues(x =>
      x.map(_._2).distinct.sortBy(_.name))
  )

  val referencedBy: Map[Int, List[WorldItem]] = Timer.showTimeBrief("inward refs",
    (for {
      item <- world
      tag <- itemToTags(item).values
      tagItem <- Tags.items(tag)
    } yield {
      (tagItem.uid, item)
    })
    .filter(x => !(x._1.equals(x._2.uid)))  // match uids
    .groupBy(_._1)                          // group by uid, resulting in a Map
    .mapValues(x =>                         //   of uid -> List[(uid, WorldItem)]
      x.map(_._2).distinct.sortBy(_.name))  // keep only items, distinct, and sort by name
  )

  // previous, next, and parent items
  val collections: Seq[CollectionItem] = world.collect({ case x: CollectionItem => x })

  val previous: Map[Int, WorldItem] = collections.flatMap(collection => {
    collection.children.drop(1).map(_.uid).zip(collection.children.dropRight(1))
  }).toMap

  val next: Map[Int, WorldItem] =  collections.flatMap(collection => {
    collection.children.dropRight(1).map(_.uid).zip(collection.children.drop(1))
  }).toMap

  val parent: Map[Int, WorldItem] = collections.flatMap(collection => {
    collection.children.map(x => (x.uid, collection))
  }).toMap

  // the main collection for each each item
  val groups: Map[Int, WorldItem] = master.children
    .flatMap(group => WorldItems.collectionToList(group)
    .map(item => (item.uid, group))).toMap

  val np: RenderTags = new RenderTags(
    tagsMap,
    world.collect({ case x: CharacterItem => x }),
    false,
    false,
    "default")

  val hiddenItemIds: Set[Int] = hiddenItems.map(_.uid).toSet


  // ~~~~ ~~~~ ~~~~ ~~~~


  private def itemToTags(item: WorldItem): Map[Int, Tags.ParsedTag] = {
    tagsMap.getOrElse(item.uid, Map())
  }
  

  // generate page contents
  def itemPageDispatch(item: WorldItem): String = item match {
    case x: CollectionItem => collectionPage(x)
    case x: ImageFileItem  => imagePage(x)
    case x: MapItem        => pixelArtPage(x)
    case x: TileRefItem    => pixelArtPage(x)
    case _                 => itemPage(item)
  }

  /// /// ///

  def masterPage(): String = {

    val collections = Timer.showTimeBrief("master page collections",
      if (master.children.nonEmpty) {
        master.children.filter(x => !hiddenItemIds.contains(x.uid)).collect({
          case curCollection: CollectionItem => {
            column(
              Column6,
              """<h3 style="display: inline-block">""" + curCollection.name + "</h3>" + nbsp +
              RenderPages.glyphLinkPage(curCollection) + "\n" +
              listGroup(
                curCollection.children
                .filter(x => !hiddenItemIds.contains(x.uid))
                .map(x => RenderPages.getCollectionLinksCollapsible(x, hiddenItemIds))))
          }
        }).grouped(2).map(_.mkString("\n") +
        """<div class="clearfix"></div>""" + "\n").mkString("\n")
      } else {
        ""
      }
    )
    
    PageTemplates.articlePage(
      master.name,
      master.description,
      pageNavbar(master, search),

      column(Column12,
        np.transform(master.notes, itemToTags(master))
      ) + collections,
          
      pageNavbar(master, false),
      license)

  }


  private def collectionPage(collection: CollectionItem): String = {
    
    val (name, description) = unifiedJumbotron match {
      case true  => (master.name, master.description)
      case false => (collection.name, collection.description)
    }

    val navbar = pageNavbar(collection, false)
    
    PageTemplates.articlePage(
      name,
      description,
      navbar,

      column(Column12,
        np.transform(collection.notes, itemToTags(collection)) + refItems(collection) + hr +
          (if (collection.children.nonEmpty && subarticles) {
            h4("Subarticles") +
              listGroup(
                  collection.children
                  .filter(x => !hiddenItemIds.contains(x.uid))
                  .map(x => RenderPages.getCollectionLinksCollapsible(x, hiddenItemIds)))
          } else {
            ""
          })),

      navbar,
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
    
    val (name, description) = unifiedJumbotron match {
      case true  => (master.name, master.description)
      case false => (imageItem.name, imageItem.description)
    }
    
    val navbar = pageNavbar(imageItem, false)

    PageTemplates.articlePage(
      name,
      description,
      navbar,

      column(Column8, image(RenderImages.imagePath(imageItem), responsive = true)) +
        column(Column4, "") +
        column(Column12,
          hr +
            imageDescription +
            np.transform(imageItem.notes, itemToTags(imageItem)) + refItems(imageItem)),
            
      navbar,
      license)
  }


  private def pixelArtPage(imageItem: ImageItem): String = {
    
    val (name, description) = unifiedJumbotron match {
      case true  => (master.name, master.description)
      case false => (imageItem.name, imageItem.description)
    }
    
    val navbar = pageNavbar(imageItem, false)

    PageTemplates.articlePage(
      name,
      description,
      navbar,

      column(Column12, RenderImages.pixelImageLinkResponsive(imageItem) + hr) +
        column(Column12, np.transform(imageItem.notes, itemToTags(imageItem)) + refItems(imageItem)),

      navbar,
      license)
  }


  private def itemPage(item: WorldItem): String = {

    val (name, description) = unifiedJumbotron match {
      case true  => (master.name, master.description)
      case false => (item.name, item.description)
    }

    val navbar = pageNavbar(item, false)

    val tags = itemToTags(item)
    // TODO: find the right way to do this - type erasure issue
    val sidenoteTags = tags.filter(x => x._2.isInstanceOf[Tags.Sidenote]).collect({case x: (Int, Tags.Sidenote) => x})

    val body = if (sidenoteTags.isEmpty) {
      column(Column12, np.transform(item.notes, itemToTags(item)) + refItems(item))
    } else {
      // TODO: move this logic into a separate function
      val sidenoteTagList = sidenoteTags
        .toList
        .sortBy(_._1)
        .map(x => (item.notes.lastIndexOf('\n', x._1), x._2)) // advance each tag position to the previous end of line character
        .groupBy(_._1)
        .mapValues(_.map(_._2))
        .toList
        .sortBy(_._1)
      val allPositions = List(0) ++ sidenoteTagList.map(_._1) ++ List(item.notes.length)

      // render by chunks
      val tags = itemToTags(item)
      val chunks = allPositions.sliding(2).map(x => (x(0), item.notes.substring(x(0), x(1)))).toList
      chunks.zipWithIndex.map({case ((startIdx, chunk), idx) => {
        val tagsMod = tags.map(x => (x._1 - startIdx, x._2))
        val sidenoteBody = if (idx == 0) {
          ""
        } else {
          val tagsInPara = sidenoteTagList(idx - 1)._2
          tagsInPara.map(tag => {
            val id = if (tag.id.equals("")) {
              ""
            } else {
              tag.id + ". "
            }
            val descTransformed = Markdown.processLine(tag.desc)
            s"""<p class="sidenote">$id$descTransformed</p>"""
          }).mkString("\n")
        }
        // TODO: properly transform markdown in sidenotes
        column(Column9, np.transform(chunk, tagsMod)) + column(Column3, sidenoteBody)
      }}).mkString("\n") + column(Column12, refItems(item))
    } + (item match {
      case _: BookItem => "<style>" + Styles.BookStyle + "</style>"
      case _           => ""
    })

    PageTemplates.articlePage(
      name, description,
      navbar,
      body,
      navbar,
      license)
  }


  // get a navbar for an article page for a world item
  private def pageNavbar(item: WorldItem, search: Boolean): Option[String] = navbars match {
    case true => {

      val relLinks = if (relativeLinks && !master.equals(item) && !master.children.contains(item)) {
        // previous / parent / next
        List(
          previous.get(item.uid).map(y => PageTemplates.LeftArrow + RenderPages.textLinkPage(y)),
          parent.get(item.uid).map(RenderPages.textLinkPage),
          next.get(item.uid).map(y => RenderPages.textLinkPage(y) + PageTemplates.RightArrow)).flatten
      } else {
        List()
      }
      
      val searchBar = if (search && master.equals(item)) {
        Search.render("master", world)
      } else {
        ""
      }

      val home = List(link("Home", RenderPages.MasterPageFile))
      val mainCollectionLinks = master.children.filter(x => !hiddenItemIds.contains(x.uid)).map(RenderPages.textLinkPage)
      val bar = (home ++ relLinks ++ mainCollectionLinks).mkString(PageTemplates.NavbarSeparator) + searchBar + hr

      Some(bar)
    }
    case false => None
  }


  private def refItems(item: WorldItem): String = {
    val refs = referencedBy.get(item.uid) match {
      case Some(x) => x
      case None    => List()
    }

    if (refs.nonEmpty) {
      h4("Referenced By") +
        listGroup(refs.sortBy(_.name).map(x => listItem(RenderPages.textLinkPage(x))))
    } else {
      ""
    }
  }

}



object RenderPages {

  val MasterPageFile = "index.html"


  // recursively generated collapsible lists
  def getCollectionLinksCollapsible(item: WorldItem, hiddenItemIds: Set[Int]): String = item match {
    case x: CollectionItem => {
    
      // TODO: think about this
      val id = x.id + "-swivel"
      
      val collapsibleLink = s"""<input type="checkbox" id="$id" class="swivel" />""" +
        "\n" + textLinkPage(x) + s"""<label for="$id"></label>"""

      listItem(
        collapsibleLink +
        listGroup(
            x.children
            .filter(x => !hiddenItemIds.contains(x.uid))
            .map(x => getCollectionLinksCollapsible(x, hiddenItemIds))),
            className = "swivel")

    }
    case _ => listItem(textLinkPage(item))
  }


  // panel that can be pulled right
  def panel(contents: String, pullRight: Boolean = false, border: Boolean = true): String = {

    val pullClass = if (pullRight) " pull-right" else ""
    val leftMargin = if (pullRight) "margin-left:32px;" else ""

    val borderStyle = if (border) "" else "border: 0; box-shadow: 0 0 0; border-radius: 0;"

    s"""<div class="panel panel-default$pullClass" style="$leftMargin $borderStyle"><div class="panel-body">$contents""" +
      """</div></div>"""
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
