// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Class for page exports. Refactored from Export.

// Ben Zimmer

// 2015-08-09: Created.
// 2015-08-27: Image page creation.

package bdzimmer.secondary.export

import java.io.File

import scala.collection.JavaConverters.asScalaBufferConverter

import com.google.api.client.util.DateTime
import org.apache.commons.io.FilenameUtils


class ExportPages(val location: String, license: String) {

  val glossaryPageFile = "glossary.html"
  val tasksPageFile = "tasks.html"

  val imagesLocation = location + "/" + ExportImages.imagesDir + "/"
  new File(imagesLocation).mkdir

  val column3 = 3
  val column4 = 4
  val column6 = 6
  val column8 = 8
  val column12 = 12


  def exportPagesList(items: List[WorldItem]): List[String] = {

    items map(item => item match {
      case x: CharacterItem => createCharacterPage(x)
      case x: MapItem => createMapPage(x)
      case x: CollectionItem => createCollectionPage(x)
      case x: ImageItem => createImagePage(x)
      case x: WorldItem => createItemPage(x)
    }) filter (!_.equals(""))

  }



  def createMasterPage(masterCollection: CollectionItem): String = {

    val relFilePath = "index.html"

    PageTemplates.createPage(
        location + "/" + relFilePath,
        masterCollection.name,
        "",

        Tags.container(

          Tags.jumboTron("<h1>%s</h1><h3>%s</h3>".format(masterCollection.name, masterCollection.description)) +

          Tags.column(column12,
            NotesParser.transform(masterCollection.notes) +

            Tags.hr +
            (
              // (masterCollection.children.asScala.toList.map(x => {
              //  Export.textLinkPage(x)
              // }) ++
              List(Tags.link("Glossary", glossaryPageFile),
                   Tags.link("Tasks", tasksPageFile),
                   Tags.link("Edit", ExportPages.notepadURL(masterCollection)))).mkString("&nbsp;&middot;") +
            Tags.hr

          ) +

          masterCollection.children.map(x => {

            val curCollection = x.asInstanceOf[CollectionItem]

            Tags.column(column6,
              "<h3>" + curCollection.name + "</h3>\n" +
              Tags.listGroup(curCollection.children
                  map(x => ExportPages.getCollectionLinksWithDescription(x))))

          }).grouped(2).map(_.mkString("\n") + """<div class="clearfix"></div>""" + "\n").mkString("\n")

        ) +

        Tags.container(Tags.hr + Tags.centered(license)))

    relFilePath

  }



  def createTasksPage(masterCollectionList: List[WorldItem]): String = {

    val relFilePath = tasksPageFile

    def taskList(todoFunc: WorldItem => List[String]): String = {
      Tags.listGroup(masterCollectionList
              map(x => (x, todoFunc(x)))
              filter(_._2.length > 0)
              map(x => Tags.listItem(ExportPages.notepadLink(x._1) + ExportPages.textLinkPage(x._1) +
                  Tags.listGroup(x._2.map(Tags.listItem(_))))))
    }

    // get task strings from a WorldItem's notes
    def getTask(item: WorldItem)(prefix: String): List[String] = {
      item.notes.split("\n").filter(_.startsWith(prefix)).map(_.substring(prefix.length)).toList
    }


    PageTemplates.createArticlePage(

        location + "/" + relFilePath,

        "Tasks", "",

        None,

       // Todos and thoughts
       Tags.column(column6, "<h3>To-dos</h3>\n" + taskList(getTask(_)("TODO: "))) +

       // Thoughts
       Tags.column(column6, "<h3>Thoughts</h3>\n" + taskList(getTask(_)("THOUGHT: "))) +


       // Empty notes
       Tags.column(column6,
         "<h3>Empty Notes</h3>\n" +
         Tags.listGroup(masterCollectionList
           filter(_.notes.equals(""))
           map(x => Tags.listItem(ExportPages.notepadLink(x) + ExportPages.textLinkPage(x))))
       ),

       license)


    relFilePath
  }



  def createGlossaryPage(masterCollectionList: List[WorldItem]): String = {

    val relFilePath = glossaryPageFile

    PageTemplates.createArticlePage(

        location + "/" + relFilePath,
        "Glossary", "",

        None,

        // Testing...glossary

        Tags.column(column6,
          "<h3>Glossary</h3>\n" +

          {
            val groupedItems = (masterCollectionList
              drop(1)  // get rid of master collection - assumes master is first in list
              // filter(!_.isInstanceOf[CollectionItem])
              filter(!_.isInstanceOf[MetaItem])
              groupBy(_.name.replaceAll("""\p{Punct}""", "")(0).toUpper))

            groupedItems.toList.sortBy(_._1).map({case (letter, items) => {
              "<h4>" + letter + "</h4>\n" +
              Tags.listGroup(
                items.sortBy(_.name).map(item => Tags.listItem(ExportPages.textLinkPage(item)))
              )
            }}).mkString("\n")
          }

        ),

        license)

    relFilePath
  }



  def createCharacterPage(character: CharacterItem): String = {

    val relFilePath = character.id + ".html"

    // println(character.id)

    PageTemplates.createArticlePage(
        location + "/" + relFilePath,
        character.name, character.description,

        Some(ExportPages.getToolbar(character)),

        Tags.column(column8, NotesParser.transform(character.notes)) +
        Tags.column(column4, Tags.image(ExportImages.imagesDir + "/" + character.id + "_12x.png")),

        license)


    relFilePath
  }



  def createMapPage(map: MapItem): String = {

    val relFilePath = map.id + ".html"

    // println(map.id)

    PageTemplates.createArticlePage(
        location + "/" + relFilePath,
        map.name, map.description,

        Some(ExportPages.getToolbar(map)),

        Tags.column(column12, NotesParser.transform(map.notes)) +
        Tags.column(column12, ExportPages.imageLinkUpscale(map)),

        license)
        // Export.IMAGES_DIR + "/" + map.id + "_4x.png")

    relFilePath
  }



  def createCollectionPage(collection: CollectionItem): String = {

    val relFilePath = collection.id + ".html"

    PageTemplates.createArticlePage(
        location + "/" + relFilePath,
        collection.name, collection.description,
        Some(ExportPages.getToolbar(collection)),

        Tags.column(column12, NotesParser.transform(collection.notes)) +

        Tags.hr +

        // links to child pages with images
        collection.children.map(x => {
          Tags.column(column3, ExportPages.imageLinkPage(x))
        }).mkString("\n"),

        license)

    relFilePath
  }



  def createImagePage(imageItem: ImageItem): String = {

    val relFilePath = imageItem.id + ".html"

    // TODO: move this into an attribute of the case class or something
    // TODO: think about doing image download here

    val wikiNameOption = (imageItem.filename.startsWith("wikimedia:") match {
      case true => Some(imageItem.filename.split(":")(1))
      case false => None
    })

    val licenseDescription = (for {
      wikiName <- wikiNameOption
      json <- ImageDownloader.getWikimediaJson(wikiName)
      wm = ImageDownloader.parseWikimediaJson(json)
      description =
        "Artist: " + wm.artist + Tags.br +
        "License: " + wm.license + Tags.br +
        (if (wm.attribution) "Attribution required." + Tags.br else "") +
        Tags.link("Source", wm.descriptionurl) + Tags.hr
    } yield description).getOrElse("")

    PageTemplates.createArticlePage(
        location + "/" + relFilePath,
        imageItem.name, imageItem.description,
        Some(ExportPages.getToolbar(imageItem)),

        Tags.column(column8, Tags.image(ExportPages.imageItemPath(imageItem), responsive = true)) +
        Tags.column(column4, "") +
        Tags.column(column12,
            Tags.hr +
            licenseDescription +
            NotesParser.transform(imageItem.notes)),
        license)

    relFilePath
  }


  def createItemPage(item: WorldItem): String = {

    val relFilePath = item.id + ".html"

    // println(collection.id)

    PageTemplates.createArticlePage(
        location + "/" + relFilePath,
        item.name, item.description,
        Some(ExportPages.getToolbar(item)),
        Tags.column(column12,
            NotesParser.transform(item.notes)),
        license )

    relFilePath
  }


}



object ExportPages {

  // recursively generate nested lists of links from a CollectionItem
  def getCollectionLinks(worldItem: WorldItem): String =  worldItem match {
    case x: CollectionItem => Tags.listItem(Tags.link(x.name, x.id + ".html") + Tags.listGroup(x.children.map(x => getCollectionLinks(x))))
    case x: WorldItem => Tags.listItem(Tags.link(x.name, x.id + ".html"))
  }


  // recursively generated nested lists of links with descriptions from a CollectionItem
  def getCollectionLinksWithDescription(worldItem: WorldItem): String =  worldItem match {
    case x: CollectionItem => Tags.listItem(Tags.link(x.name, x.id + ".html") + (if (x.description.length > 0 ) " - " + x.description else "") + Tags.listGroup(x.children.map(x => getCollectionLinksWithDescription(x))))
    case x: WorldItem => Tags.listItem(Tags.link(x.name, x.id + ".html") + (if (x.description.length > 0 ) " - " + x.description else ""))
  }


  // generate HTML for an item's 1x image, with a link to the 4x version
  def imageLinkUpscale(item: WorldItem): String = {
    Tags.link(Tags.image(ExportImages.imagesDir + "/" + item.id + ".png"), ExportImages.imagesDir + "/" + item.id + "_4x.png")
  }


  def imageItemPath(imageItem: ImageItem): String = {
    ExportImages.imagesDir + "/" + imageItem.id + "." + FilenameUtils.getExtension(imageItem.filename)
  }


  // generate HTML for a smaller image, with a link to the page
  def imageLinkPage(item: WorldItem): String = {

    val imageFile = ExportImages.imagesDir + "/" + item.id + "%s" + ".png"

    val imageTag = item match {
      case x: MapItem => Tags.imageSprite(imageFile.format(ExportImages.scalePostfix(1)), 0, 0, 192, 192) // scalastyle:ignore magic.number
      case x: CharacterItem => Tags.image(imageFile.format(ExportImages.scalePostfix(4))) // scalastyle:ignore magic.number
      case x: WorldItem => ""
    }
    Tags.link(imageTag + "<br />" + item.name, item.id + ".html")
  }

  /*
  // generate HTML for a smaller image, with a link to the page
  def imageLinkPage(item: WorldItem): String = {
    val scale = item match {
      case x: MapItem => 1
      case _ => 4
    }
    Tags.link(Tags.image(IMAGES_DIR + "/" + item.id + scalePostfix(scale) + ".png") + item.name, item.id + ".html")
  }
  */

  // generate HTML for a text link to an item's page
  def textLinkPage(item: WorldItem): String = {
    Tags.link(item.name, item.id + ".html")
  }


  // get a toolbar for an article page for a world item
  def getToolbar(item: WorldItem): String = {
    List(// Tags.link("Edit Local", localURL(item, localDriveMapDir)),
         Tags.link("Edit", notepadURL(item))).mkString("&nbsp;&middot;")
  }

  def notepadLink(item: WorldItem): String = {
    Tags.link("""<small><span class="glyphicon glyphicon-pencil"></span></small>""", notepadURL(item))
  }

  // get the URL of the YAML source on Drive
  def notepadURL(item: WorldItem): String = {
    "https://drivenotepad.appspot.com/app?state=%7B%22ids%22:%5B%22" + item.remoteid + "%22%5D,%22action%22:%22open%22%7D"
  }

  // get a local URL (local URL for mapped Google Drive)
  // def localURL(item: WorldItem, localDriveMapDir: String): String = {
  //   localDriveMapDir + "/" + item.srcyml
  // }


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


  // marge FileModifiedMapsm keeping newer dates / ids.
  def mergeDateTimes(map1: FileModifiedMap, map2: FileModifiedMap): FileModifiedMap = {
    map1 ++ map2.map{case (k, v) => k -> {
      map1.get(k) match {
        case Some(x) => if (x._2.getValue > v._2.getValue) x else v
        case None => v
      }
    }}
  }



}


