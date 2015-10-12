// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Class for page exports. Refactored from Export.

// 2015-08-09: Created.
// 2015-08-27: Image page creation.
// 2015-08-31: Enhanced character image functionality.
// 2015-09-01: More on above.
// 2015-09-02: More image functions for jumbotron functionality (needs cleaning up).
//             Master page and article pages all use the same template.
// 2015-09-08: Tasks page uses tags.

package bdzimmer.secondary.export

import java.io.File

import scala.collection.JavaConverters.asScalaBufferConverter
import scala.util.Try

import com.google.api.client.util.DateTime
import org.apache.commons.io.FilenameUtils

import bdzimmer.secondary.export.Tags._


class ExportPages(
    master: CollectionItem,
    world: List[WorldItem],
    val location: String,
    license: String) {

  val np = new NotesParser(world)

  val indexPageFile = "indexpage.html"
  val tasksPageFile = "tasks.html"
  val familyTreesPageFile = "familytrees.html"

  val imagesLocation = location + File.separator + ExportImages.imagesDir + File.separator
  new File(imagesLocation).mkdir

  val metaItems = WorldItem.filterList[MetaItem](world)

  def exportPagesList(items: List[WorldItem]): List[String] = {

    items map(item => item match {
      case x: CharacterItem => createCharacterPage(x)
      case x: MapItem => createMapPage(x)
      case x: CollectionItem => createCollectionPage(x)
      case x: ImageItem => createImagePage(x)
      case _ => createItemPage(item)
    }) filter (!_.equals(""))

  }



  def createMasterPage(): String = {

    val relFilePath = "index.html"

    val toolbar = Some(List(
            link("Index", indexPageFile),
            link("Family Trees", familyTreesPageFile),
            link("Tasks", tasksPageFile),
            link("Edit", ExportPages.notepadURL(master))).mkString("&nbsp;&nbsp;&middot;&nbsp;") +
            hr)

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
                  map(x => ExportPages.getCollectionLinksWithDescription(x))))

          }).grouped(2).map(_.mkString("\n") + """<div class="clearfix"></div>""" + "\n").mkString("\n")),

        license)



    relFilePath

  }



  def createTasksPage(): String = {

    val relFilePath = tasksPageFile

    def taskList(todoFunc: WorldItem => List[String]): String = {
      listGroup(world
              map(x => (x, todoFunc(x)))
              filter(_._2.length > 0)
              map(x => listItem(ExportPages.notepadLink(x._1) + ExportPages.textLinkPage(x._1) +
                  listGroup(x._2.map(text => listItem(NotesParser.processLine(text)))))))
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
           filter(_.notes.equals(""))
           map(x => listItem(ExportPages.notepadLink(x) + ExportPages.textLinkPage(x))))
       ),

       license)

    relFilePath
  }



  def createIndexPage(): String = {

    val relFilePath = indexPageFile

    PageTemplates.createArticlePage(

        location + File.separator + relFilePath,
        "Index", "",

        None,

        column(Column6,

          {
            val groupedItems = (world
              drop(1)  // get rid of master collection - assumes master is first in list
              // filter(!_.isInstanceOf[CollectionItem])
              filter(!_.isInstanceOf[MetaItem])
              groupBy(_.name.replaceAll("""\p{Punct}""", "")(0).toUpper))

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




  def createFamilyTreesPage(): String = {

    val relFilePath = familyTreesPageFile
    val characters = WorldItem.filterList[CharacterItem](world)


    PageTemplates.createArticlePage(

        location + File.separator + relFilePath,
        "Family Trees", "",

        None,

        column(Column12,
          // for now
          FamilyTree.getJs(characters, np)
        ),

        license)

    relFilePath
  }




  def createCharacterPage(character: CharacterItem): String = {

    val relFilePath = character.id + ".html"

    PageTemplates.createArticlePage(
        location + File.separator + relFilePath,
        character.name, character.description,

        Some(ExportPages.getToolbar(character)),

        // column(column8, np.transform(character.notes)) +
        // column(column4, ExportPages.characterImage(character, metaItems, 12)),

        column(Column12,
            ExportPages.panel(
                ExportPages.imageLinkPage(character, metaItems, false, 320, false, 12), true, false) +
            np.transform(character.notes)),

        license)


    relFilePath
  }



  def createMapPage(map: MapItem): String = {

    val relFilePath = map.id + ".html"

    PageTemplates.createArticlePage(
        location + File.separator + relFilePath,
        map.name, map.description,

        Some(ExportPages.getToolbar(map)),

        column(Column12, np.transform(map.notes)) +
        column(Column12, ExportPages.imageLinkUpscale(map)),

        license)

    relFilePath
  }



  def createCollectionPage(collection: CollectionItem): String = {

    val relFilePath = collection.id + ".html"

    PageTemplates.createArticlePage(
        location + File.separator + relFilePath,
        collection.name, collection.description,
        Some(ExportPages.getToolbar(collection)),

        column(Column12, np.transform(collection.notes) + hr) +

        // links to child pages with images
        collection.children.map(x => {
          column(Column3, ExportPages.imageLinkPage(x, metaItems, responsive = true))
        }).grouped(4).map(_.mkString("\n") + """<div class="clearfix"></div>""" + "\n").mkString("\n"),

        license)

    relFilePath
  }



  def createImagePage(imageItem: ImageItem): String = {

    val relFilePath = imageItem.id + ".html"

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

        column(Column8, image(ExportPages.imageItemImagePath(imageItem), responsive = true)) +
        column(Column4, "") +
        column(Column12,
            hr +
            licenseDescription +
            np.transform(imageItem.notes)),
        license)

    relFilePath
  }


  def createItemPage(item: WorldItem): String = {

    val relFilePath = item.id + ".html"

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

  val Slash = "/"

  // recursively generate nested lists of links from a CollectionItem
  def getCollectionLinks(item: WorldItem): String =  item match {
    case x: CollectionItem => listItem(textLinkPage(x) + listGroup(x.children.map(x => getCollectionLinks(x))))
    case _ => listItem(textLinkPage(item))
  }


  // recursively generated nested lists of links with descriptions from a CollectionItem
  def getCollectionLinksWithDescription(item: WorldItem): String =  item match {
    case x: CollectionItem => listItem(
        textLinkPage(x) +
        (if (x.description.length > 0 ) " - " + NotesParser.processLine(x.description) else "") +
        listGroup(x.children.map(x => getCollectionLinksWithDescription(x))))
    case _ => listItem(
        textLinkPage(item) +
        (if (item.description.length > 0 ) " - " + NotesParser.processLine(item.description) else ""))
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


  def getCharacterImageInfo(ci: CharacterItem, metaItems: List[MetaItem]): (Option[MetaItem], Int) = {

    // split spritesheet attribute by comma
    // first part is item id, second part spritesheet row (if exists)
    val spriteSplit = ci.image.split(",\\s+")
    val (metaId, sheetRow) = spriteSplit.toList match {
      case x :: xs => {
        (x, xs.headOption.flatMap(s => Try(s.toInt).toOption).getOrElse(0))
      }
    }

    // there may not be a matching MetaItem in the collection
    // if it doesn't exist yet
    val metaOption = metaItems.filter(_.id.equals(metaId)).headOption
    (metaOption, sheetRow)

  }


  def characterImage(
      ci: CharacterItem, metaItems: List[MetaItem],
      scale: Int = 4,
      responsive: Boolean = true,
      maxWidth: Int = 480): String = {

    val metaOption = getCharacterImageInfo(ci, metaItems)._1
    val path = characterItemImagePath(ci, metaItems, scale)

    metaOption.map(meta => meta match {
      case ss: SpritesheetItem => image(path)
      case im: ImageItem => image(path, responsive, maxWidth)
      case _ => ""
    }).getOrElse("")

  }


  def characterItemImagePath(
      ci: CharacterItem,
      metaItems: List[MetaItem],
      scale: Int = 4): String = {

    val (metaOption, sheetRow) = getCharacterImageInfo(ci, metaItems)

    metaOption.map(meta => meta match {
      case ss: SpritesheetItem => {
        val imageFile = ExportImages.imagesDir + Slash + ci.id + "%s.png"
        imageFile.format(ExportImages.scalePostfix(scale))
      }
      case im: ImageItem => imageItemImagePath(im)
      case _ => ""
    }).getOrElse("")

  }


  // generate HTML for an item's 1x image, with a link to the 4x version
  def imageLinkUpscale(item: WorldItem): String = {
    link(
        image(ExportImages.imagesDir + Slash + item.id + ".png"),
        ExportImages.imagesDir + Slash + item.id + "_4x.png")
  }


  def imageItemImagePath(imageItem: ImageItem): String = {
    ExportImages.imagesDir + Slash + imageItem.id + "." + FilenameUtils.getExtension(imageItem.filename)
  }


  // generate HTML for a smaller image, with a link to the page
  // the attributes are kind of piling up on this function because of the many
  // different kinds of images and contexts where this is used.
  def imageLinkPage(
      item: WorldItem,
      metaItems: List[MetaItem],
      responsive: Boolean = true,
      maxWidth: Int = 480,
      showName: Boolean = true,
      scale: Int = 4): String = {

    val imageTag = item match {
      case x: MapItem => {
        val imageFile = ExportImages.imagesDir + Slash + item.id + "%s" + ".png"
        imageSprite(imageFile.format(ExportImages.scalePostfix(1)), 0, 0, 192, 192) // scalastyle:ignore magic.number}
      }
      case x: CharacterItem => ExportPages.characterImage(x, metaItems, scale, responsive, maxWidth) // scalastyle:ignore magic.number
      case x: ImageItem => image(imageItemImagePath(x), responsive, maxWidth)
      case _ => ""
    }

    val imageName = showName match {
      case true => (if (!responsive ) "<br>" else "" ) + NotesParser.processLine(item.name)
      case false => ""
    }

    link(imageTag + imageName, item.id + ".html")
  }


  // can this be combined with the above somehow?
  def itemImagePath(
      item: WorldItem,
      metaItems: List[MetaItem]): String = item match {

    case x: MapItem => ExportImages.imagesDir + Slash + item.id + "_4x" + ".png"
    case x: CharacterItem => characterItemImagePath(x, metaItems)
    case x: ImageItem => imageItemImagePath(x)
    case _ => ""
  }


  // generate HTML for a text link to an item's page
  def textLinkPage(item: WorldItem): String = {
    link(NotesParser.processLine(item.name), item.id + ".html")
  }


  // get a toolbar for an article page for a world item
  def getToolbar(item: WorldItem): String = {
    List(// link("Edit Local", localURL(item, localDriveMapDir)),
         link("Home", "index.html"),
         link("Edit", notepadURL(item))).mkString("&nbsp;&nbsp;&middot;&nbsp;") + hr
  }

  def notepadLink(item: WorldItem): String = {
    link("""<small><span class="glyphicon glyphicon-pencil"></span></small>""", notepadURL(item))
  }

  // get the URL of the YAML source on Drive
  def notepadURL(item: WorldItem): String = {
    "https://drivenotepad.appspot.com/app?state=%7B%22ids%22:%5B%22" + item.remoteid + "%22%5D,%22action%22:%22open%22%7D"
  }


  // functions for working with file info maps

  // save a FileModifiedMap to a text file
  def saveModifiedMap(outputFile: String, map: FileModifiedMap): Unit = {
    val pw = new java.io.PrintWriter(new java.io.File(outputFile))
    map foreach (x =>  pw.println(x._1 + "\t" + x._2._1 + "\t" + x._2._2.getValue))
    pw.close
  }

  // load a FileModifiedMap from a text file
  def loadModifiedMap(inputFile: String): FileModifiedMap = {
    val lines = scala.io.Source.fromFile(inputFile).getLines
    lines.map(x => x.split("\t")).map(x => (x(0), (x(1), new DateTime(x(2).toLong)))).toMap
  }

  // load a FileModifiedMap from a text file, returning an empty map
  // if the file doesn't exist.
  def loadOrEmptyModifiedMap(inputFile: String): FileModifiedMap = {
    (new java.io.File(inputFile).exists) match {
      case true => ExportPages.loadModifiedMap(inputFile)
      case false => getEmptyFileModifiedMap
    }
  }

  // get an empty FileModifiedMap
  def getEmptyFileModifiedMap(): FileModifiedMap = {
    List.empty[(String, (String, DateTime))].toMap
  }

  def getEmptyFileOutputsMap(): FileOutputsMap = {
    List.empty[(String, List[String])].toMap
  }

  // combine two dictionaries of (String -> List[String], merging the values of
  // corresponding keys
  def mergeFileOutputsMaps(map1: FileOutputsMap, map2: FileOutputsMap): FileOutputsMap = {
    // TODO: add a distinct here?
    map1 ++ map2.map{case (k, v) => k -> (v ++ map1.getOrElse(k, Nil))}
  }


  // merge FileModifiedMaps keeping newer dates / ids.
  def mergeDateTimes(map1: FileModifiedMap, map2: FileModifiedMap): FileModifiedMap = {
    map1 ++ map2.map{case (k, v) => k -> {
      map1.get(k) match {
        case Some(x) => if (x._2.getValue > v._2.getValue) x else v
        case None => v
      }
    }}
  }



}


