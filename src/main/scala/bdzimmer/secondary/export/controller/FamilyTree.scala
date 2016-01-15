// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Generate family trees using tags in characters.

// 2015-10-05: Created. Spouse lines not supported yet.
// 2015-10-07: Updates for previous and ancestor vs. parent relationships.
// 2015-01-08: WIP marriages.

package bdzimmer.secondary.export.controller

import bdzimmer.secondary.export.model.{CharacterItem, SecTag}
import bdzimmer.secondary.export.view.Tags

case class TreeEntry(
    id: String,
    name: String,
    description: String,
    nodeType: String,
    parentType: String,
    children: List[TreeEntry],
    marriages: List[Marriage])

// marriages are weird because they sort of turn spouses into siblings
case class Marriage(
    hidden: TreeEntry,
    spouse: TreeEntry)

object FamilyTree {

  val TreeStyles =
    """<script src="https://cdnjs.cloudflare.com/ajax/libs/d3/3.5.6/d3.min.js" charset="utf-8"></script>""" + "\n" +
    """<link href="tree/tree.css" rel="stylesheet">""" + "\n" +
    """<script src="tree/drawtree.js" charset="utf-8"></script>""" + "\n"


  // for now, all of the family trees
  def getAllJs(characters: List[CharacterItem], np: RenderSecTags): String = {

    def isRoot(char: CharacterItem): Boolean = {
      val charTagKinds = char.tags.map(_.kind).toSet
      Genealogy.AncestorTags.intersect(charTagKinds).size == 0
    }

    // root characters are those without a parent or ancestor
    val rootChars = characters.filter(isRoot)
    val nonRootChars = characters.filter(!isRoot(_))

    val trees = rootChars.map(x => getJs(x, characters, np))
    TreeStyles + trees.mkString("\n" + Tags.hr + "\n")

  }


  def getJs(character: CharacterItem, characters: List[CharacterItem], np: RenderSecTags): String = {

    // add a hidden parent to a tree
    def wrapTree(te: TreeEntry): String = {
      "var root = " +
      treeToJs(TreeEntry("root", "", "", "title", "none", List(te), List())) + ";"
    }

    def spouseLines(marriages: List[(String, String)]): String = {
      val spouseObjects = marriages.map(m => s"""{srcId: "${m._1}",  dstId: "${m._2}"}""").mkString(",")
      s"""var spouses = [${spouseObjects}];"""
    }

    def getAllMarriages(tree: TreeEntry): List[(String, String)] = {
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
      node: CharacterItem,
      allCharacters: List[CharacterItem],
      parentType: String,
      np: RenderSecTags): TreeEntry = {

    val distinctChildren = Genealogy.findDistinctChildren(node, allCharacters)

    // println(node.id + " distinct children: " + children1.map(x => x._1.id + "-" + x._2) + children2.map(x => x._1.id + "-" + x._2))

    // group children by marriage (or no marriage)
    val childrenBySpouse = Genealogy.findParents(node, distinctChildren, allCharacters)

    val newAllCharacters = allCharacters.filter(!_.id.equals(node.id))

    val marriages = childrenBySpouse.toList.collect({case ((Some(parent), rel), children) => {
      Marriage(
          TreeEntry(
              node.id + "_" + parent.id, "", "", "marriage", "none",
              children.map(child => {
                buildTree(
                    child,
                    newAllCharacters.filter(!_.id.equals(parent.id)),
                    Genealogy.getParentType(rel), np)
              }), List()),
          TreeEntry(
              parent.id, parent.name.replace(' ', '\t') + "\t" + Genealogy.lifespan(parent),
              "", "character", "none", List(), List()))
    }})

    val singleParentChildren = childrenBySpouse.toList.collect({case ((None, rel), children) => {
      children.map(child => {
        buildTree(child, newAllCharacters, Genealogy.getParentType(rel), np)
      })
    }}).flatten

    val description = np.transform(
        node.notes.split("\n").filter(_.length > 0).headOption.getOrElse("")).replaceAll("\"", "\\\\\"")

    println(node.name)
    println("single parent children: " + singleParentChildren.map(_.name))
    println("marriages: " + marriages.map(x => x.spouse.name + " " + x.hidden.children.map(_.name)))

    println("***")

    TreeEntry(
        node.id, node.name.replace(' ', '\t') + "\t" + Genealogy.lifespan(node),
        description, "character", parentType, singleParentChildren, marriages)
  }


  // convert a TreeEntry tree into JavaScript code for use with D3
  def treeToJs(te: TreeEntry): String = {

    val childrenString = if (te.children.length > 0) {
      s"children: [\n${te.children.map(child => {
        treeToJs(child) :: child.marriages.map(marriage => List(treeToJs(marriage.hidden), treeToJs(marriage.spouse))).flatten
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
