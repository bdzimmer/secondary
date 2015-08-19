// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Class for page exports. Refactored from Export.

// Ben Zimmer

// 2015-08-09: Created.

package bdzimmer.secondary.export

import java.io.File

import scala.collection.JavaConverters.asScalaBufferConverter

import org.pegdown.PegDownProcessor

import com.google.api.client.util.DateTime


class ExportPages(val location: String, license: String) {

  val GLOSSARY_PAGE_FILE = "glossary.html"
  val TASKS_PAGE_FILE = "tasks.html"

  val imagesLocation = location + "/" + ExportImages.IMAGES_DIR + "/"
  new File(imagesLocation).mkdir

  val COLUMN_3 = 3
  val COLUMN_4 = 4
  val COLUMN_6 = 6
  val COLUMN_8 = 8
  val COLUMN_12 = 12

  // TODO: getPegDown should maybe be moved the companion object
  // I think it might be handy to have the export hold
  // config info for pegdown.


  // get a new pegdown processor
  def getPegDown(): PegDownProcessor = {
    // new PegDownProcessor(Extensions.HARDWRAPS)
    new PegDownProcessor
  }



  def exportPagesList(items: List[WorldItem]): List[String] = {

    items map(item => item match {
      case x: CharacterItem => createCharacterPage(x)
      case x: MapItem => createMapPage(x)
      case x: CollectionItem => createCollectionPage(x)
      case x: WorldItem => createItemPage(x)
    }) filter (!_.equals(""))

  }



  def createMasterPage(masterCollection: CollectionItem): String = {

    val relFilePath = "index.html"

    val pp = getPegDown

    PageTemplates.createPage(
        location + "/" + relFilePath,
        masterCollection.name,
        "",

        Tags.container(

          Tags.jumboTron("<h1>%s</h1><h3>%s</h3>".format(masterCollection.name, masterCollection.description)) +

          Tags.column(COLUMN_12,
            pp.markdownToHtml(masterCollection.notes) +

            Tags.hr +
            (
              // (masterCollection.children.asScala.toList.map(x => {
              //  Export.textLinkPage(x)
              // }) ++
              List(Tags.link("Glossary", GLOSSARY_PAGE_FILE),
                   Tags.link("Tasks", TASKS_PAGE_FILE),
                   Tags.link("Edit", ExportPages.notepadURL(masterCollection)))).mkString("&nbsp;&middot;") +
            Tags.hr

          ) +

          masterCollection.children.map(x => {

            val curCollection = x.asInstanceOf[CollectionItem]

            Tags.column(COLUMN_6,
              "<h3>" + curCollection.name + "</h3>\n" +
              Tags.listGroup(curCollection.children
                  map(x => ExportPages.getCollectionLinksWithDescription(x))))

          }).grouped(2).map(_.mkString("\n") + """<div class="clearfix"></div>""" + "\n").mkString("\n")

        ) +

        Tags.container(Tags.hr + Tags.centered(license)))

    relFilePath

  }



  def createTasksPage(masterCollectionList: List[WorldItem]): String = {

    val relFilePath = TASKS_PAGE_FILE

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

         Tags.column(COLUMN_6,
           "<h3>To-dos</h3>\n" + taskList(getTask(_)("TODO: "))
         ) +


         // Thoughts

         Tags.column(COLUMN_6,
           "<h3>Thoughts</h3>\n" + taskList(getTask(_)("THOUGHT: "))
         ) +


         // Empty notes
         Tags.column(COLUMN_6,
           "<h3>Empty Notes</h3>\n" +
           Tags.listGroup(masterCollectionList
             filter(_.notes.equals(""))
             map(x => Tags.listItem(ExportPages.notepadLink(x) + ExportPages.textLinkPage(x))))
         ),

         license)


    relFilePath
  }



  def createGlossaryPage(masterCollectionList: List[WorldItem]): String = {

    val relFilePath = GLOSSARY_PAGE_FILE

    PageTemplates.createArticlePage(

        location + "/" + relFilePath,
        "Glossary", "",

        None,

        // Testing...glossary

        Tags.column(COLUMN_6,
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

    val pp = getPegDown

    PageTemplates.createArticlePage(
        location + "/" + relFilePath,
        character.name, character.description,

        Some(ExportPages.getToolbar(character)),

        Tags.column(COLUMN_8, pp.markdownToHtml(character.notes)) +
        Tags.column(COLUMN_4, """<img src="%s" />""".format(ExportImages.IMAGES_DIR + "/" + character.id + "_12x.png")),

        license)


    relFilePath
  }



  def createMapPage(map: MapItem): String = {

    val relFilePath = map.id + ".html"

    // println(map.id)

    val pp = getPegDown

    PageTemplates.createArticlePage(
        location + "/" + relFilePath,
        map.name, map.description,

        Some(ExportPages.getToolbar(map)),

        Tags.column(COLUMN_12, pp.markdownToHtml(map.notes)) +
        Tags.column(COLUMN_12, ExportPages.imageLinkUpscale(map)),

        license)
        // Export.IMAGES_DIR + "/" + map.id + "_4x.png")

    relFilePath
  }



  def createCollectionPage(collection: CollectionItem): String = {

    val relFilePath = collection.id + ".html"

    // println(collection.id)

    val pp = getPegDown


    PageTemplates.createArticlePage(
        location + "/" + relFilePath,
        collection.name, collection.description,
        Some(ExportPages.getToolbar(collection)),

        Tags.column(COLUMN_12, pp.markdownToHtml(collection.notes)) +

        Tags.hr +

        // links to child pages with images
        collection.children.map(x => {
          Tags.column(COLUMN_3, ExportPages.imageLinkPage(x))
        }).mkString("\n"),

        license)

    relFilePath
  }


  def createItemPage(item: WorldItem): String = {

    val relFilePath = item.id + ".html"

    // println(collection.id)

    val pp = getPegDown

    PageTemplates.createArticlePage(
        location + "/" + relFilePath,
        item.name, item.description,
        Some(ExportPages.getToolbar(item)),
        Tags.column(COLUMN_12, pp.markdownToHtml(item.notes)),
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
    Tags.link(Tags.image(ExportImages.IMAGES_DIR + "/" + item.id + ".png"), ExportImages.IMAGES_DIR + "/" + item.id + "_4x.png")
  }


  // generate HTML for a smaller image, with a link to the page
  def imageLinkPage(item: WorldItem): String = {

    val imageFile = ExportImages.IMAGES_DIR + "/" + item.id + "%s" + ".png"

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


