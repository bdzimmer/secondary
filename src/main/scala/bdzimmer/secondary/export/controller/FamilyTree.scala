// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Generate family trees using tags in characters.


package bdzimmer.secondary.export.controller

import bdzimmer.secondary.export.model.WorldItems.CharacterItem
import bdzimmer.secondary.export.view.{Html, WebResource}


object FamilyTree {


  case class TreeEntry(
    id: String,
    name: String,
    description: String,
    nodeType: String,
    parentType: String,
    children: Seq[TreeEntry],
    marriages: Seq[Marriage])


  // marriages are weird because they sort of turn spouses into siblings
  case class Marriage(
      hidden: TreeEntry,
      spouse: TreeEntry)


  val TreeStyles =
    s"""<script src="${WebResource.D3.localRelFilename}" charset="utf-8"></script>""" + "\n" +
    s"""<link href="${WebResource.TreeCss.localRelFilename}" rel="stylesheet">""" + "\n" +
    s"""<script src="${WebResource.TreeJs.localRelFilename}" charset="utf-8"></script>""" + "\n"


  def nameList(char: CharacterItem): List[String] = char.nameParts match {
    case Some(xs) => xs
    case None => List(char.name)
  }


  // for now, all of the family trees
  def getAllJs(characters: List[CharacterItem], np: RenderTags): String = {

    def isRoot(char: CharacterItem): Boolean = {
      val charTagKinds = char.tags.values.map(_.kind).toSet
      Genealogy.AncestorTags.intersect(charTagKinds).size == 0
    }

    // root characters are those without a parent or ancestor
    val rootChars = characters.filter(isRoot)
    val nonRootChars = characters.filter(!isRoot(_))

    val trees = rootChars.map(x => getJs(x, characters, np))
    TreeStyles + trees.mkString("\n" + Html.hr + "\n")

  }


  def getJs(character: CharacterItem, characters: List[CharacterItem], np: RenderTags): String = {

    // add a hidden parent to a tree
    def wrapTree(te: TreeEntry): String = {
      "var root = " +
      treeToJs(TreeEntry("root", "", "", "title", "none", List(te), List())) + ";"
    }

    def spouseLines(marriages: Seq[(String, String)]): String = {
      val spouseObjects = marriages.map(m => s"""{srcId: "${m._1}",  dstId: "${m._2}"}""").mkString(",")
      s"""var spouses = [${spouseObjects}];"""
    }

    def getAllMarriages(tree: TreeEntry): Seq[(String, String)] = {
      (tree.marriages.map(x => (tree.id, x.spouse.id))
        ++ tree.marriages.flatMap(x => getAllMarriages(x.hidden))
        ++ tree.children.flatMap(getAllMarriages))
    }

    val tree = buildTree(character, characters, "none", np)
    val marriages = getAllMarriages(tree)

    val treeDiv =
      "\n\n" + s"""<div id="${tree.id + "_tree"}">""" + "\n" +
      "<script>\n" + s"""${wrapTree(tree)}\n${spouseLines(marriages)}""" + "\n" + s"""drawTree(root, spouses, "#${tree.id + "_tree"}", 800, 480)""" +
      "\n</script>\n</div>\n\n"

    treeDiv
  }


  // build family tree given a root character and a list of characters to
  // search for descendants.
  def buildTree(
      char: CharacterItem,
      allCharacters: List[CharacterItem],
      parentType: String,
      np: RenderTags): TreeEntry = {

    val distinctChildren = Genealogy.findDistinctChildren(char, allCharacters)

    // println(node.id + " distinct children: " + children1.map(x => x._1.id + "-" + x._2) + children2.map(x => x._1.id + "-" + x._2))

    // group children by marriage (or no marriage)
    val childrenBySpouse = Genealogy.findParents(char, distinctChildren, allCharacters)

    val newAllCharacters = allCharacters.filter(!_.id.equals(char.id))

    val marriages = childrenBySpouse.toList.collect({case (Some(parent), children) => {
      Marriage(
          TreeEntry(
              char.id + "_" + parent.id, "", "", "marriage", "none",
              children.map({case (child, rel) => {
                buildTree(
                    child,
                    newAllCharacters.filter(!_.id.equals(parent.id)),
                    Genealogy.getParentType(rel), np)
              }}), List()),
          TreeEntry(
              parent.id, nameList(parent).mkString("\t") + "\t" + Genealogy.lifespan(parent),
              "", "character", "none", List(), List()))
    }})

    val singleParentChildren = childrenBySpouse.toList.collect({case (None, children) => {
      children.map({case (child, rel) => {
        buildTree(child, newAllCharacters, Genealogy.getParentType(rel), np)
      }})
    }}).flatten

    val tags = np.stringToTags.get(char.id).getOrElse(Map())

    val description = np.transform(
        char.notes, tags).split("\n").filter(_.length > 0).headOption.getOrElse("").replaceAll("\"", "\\\\\"")

    // println(char.name)
    // println("single parent children: " + singleParentChildren.map(_.name))
    // println("marriages: " + marriages.map(x => x.spouse.name + " " + x.hidden.children.map(_.name)))
    // println("***")

    TreeEntry(
        char.id, nameList(char).mkString("\t") + "\t" + Genealogy.lifespan(char),
        description, "character", parentType, singleParentChildren, marriages)
  }


  // convert a TreeEntry tree into JavaScript code for use with D3
  // TODO: get rid of flattens
  def treeToJs(te: TreeEntry): String = {

    val childrenString = if (te.children.length > 0) {
      s"children: [\n${te.children.map(child => {
        treeToJs(child) +: child.marriages.map(marriage => List(treeToJs(marriage.hidden), treeToJs(marriage.spouse))).flatten
      }).flatten.mkString(",\n")}]"
    } else {
      ""
    }

s"""{
  id: "${te.id}",
  name: "${te.name}",
  description: "${te.description}",
  nodeType: "${te.nodeType}",
  parentType: "${te.parentType}",
  ${childrenString}
}"""

  }
}
