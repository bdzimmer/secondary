// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Tests for ContentTransformer.
// Generates a local copy of the Secondary documentation.

// 2015-09-03: Created. This will eventually become a test suite.

package bdzimmer.secondary.export

object ExportLocalTest {

  val inputDir = "doc/content"
  val outputDir = "doc/export"

  // test("export 'integration' test") {

  def main(args: Array[String]): Unit = {

    val masterCollectionName = "master"
    val mainCollectionNames = List("characters", "locations", "lore", "images", "tilesets", "sprites")
    val license = "Copyright (c) 2015 Ben Zimmer. All rights reserved."

    val masterCollection = WorldLoader.loadMasterCollection(
        inputDir,
        masterCollectionName, mainCollectionNames,
        ExportPages.getEmptyFileModifiedMap)

    val world = WorldLoader.collectionToList(masterCollection)

    val exportPages = new ExportPages(world, outputDir, license)
    val allPageOutputs = List(exportPages.createMasterPage(masterCollection),
                              exportPages.createTasksPage,
                              exportPages.createIndexPage) ++
                         exportPages.exportPagesList(world)

    val exportImages = new ExportImages(world, outputDir, license)
    val imageOutputs = exportImages.exportAllImages(world, inputDir)

    // problems here if no CharacterItems present
    val charsToExport = WorldItem.filterList[CharacterItem](world)
    val characterOutputs = exportImages.prepareCharacterImages(charsToExport, inputDir)


  }

}
