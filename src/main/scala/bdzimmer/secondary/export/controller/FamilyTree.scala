// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Generate family trees using tags in characters.

// 2015-10-05: Created. Spouse lines not supported yet.
// 2015-10-07: Updates for previous and ancestor vs. parent relationships.

package bdzimmer.secondary.export.controller

import bdzimmer.secondary.export.model.{CharacterItem, SecTag}
import bdzimmer.secondary.export.view.Tags

case class TreeEntry(
    id: String,
    name: String,
    description: String,
    nodeType: String,
    parentType: String,
    children: List[TreeEntry])


object FamilyTree {

  val AncestorTags = Set("father", "mother", "parent", "ancestor")
  val DescendantTags = Set("son", "daughter", "descendant")

  val TreeStyles =
    """<script src="https://cdnjs.cloudflare.com/ajax/libs/d3/3.5.6/d3.min.js" charset="utf-8"></script>""" + "\n" +
    """<link href="tree/tree.css" rel="stylesheet">""" + "\n" +
    """<script src="tree/drawtree.js" charset="utf-8"></script>""" + "\n"


  // for now, all of the family trees
  def getAllTreesJs(characters: List[CharacterItem], np: RenderSecTags): String = {

    def isRoot(char: CharacterItem): Boolean = {
      val charTagKinds = char.tags.map(_.kind).toSet
      AncestorTags.intersect(charTagKinds).size == 0
    }

    // root characters are those without a parent or ancestor
    val rootChars = characters.filter(isRoot)
    val nonRootChars = characters.filter(!isRoot(_))

    val trees = rootChars.map(x => getTreeJs(x, characters, np))
    TreeStyles + trees.mkString("\n" + Tags.hr + "\n")

  }


  def getTreeJs(character: CharacterItem, characters: List[CharacterItem], np: RenderSecTags): String = {

    // add a hidden parent to a tree
    def wrapTree(te: TreeEntry): String = {
      "var root = " +
      treeToJs(TreeEntry("root", "", "", "title", "none", List(te))) + ";\n" +
      "var spouses = [];\n"
    }

    val tree = buildTree(character, characters, "none", np)

    val treeDiv =
      "\n\n" + s"""<div id="${tree.id + "_tree"}">""" + "\n" +
      "<script>\n" + s"""${wrapTree(tree)}""" + "\n" + s"""drawTree(root, "#${tree.id + "_tree"}", 800, 640)""" +
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

    // TODO: revise this logic
    def tagCharacter(tag: SecTag) = allCharacters.filter(x => {
      x.id.equals(tag.value) || x.name.equals(tag.value)
    }).headOption

    def filterTags(tags: List[SecTag], kinds: Set[String]): List[SecTag] = {
      tags.filter(x => kinds.contains(x.kind))
    }

    // it's a child if it has an ancestor tag for the current character
    val children1 = allCharacters.map(char => {
      val tags = filterTags(char.tags, AncestorTags).filter(x => {
        val matchChar = tagCharacter(x)
        matchChar.map(_.id.equals(node.id)).getOrElse(false)
      })
      (char, tags.map(_.kind))
    }).filter(_._2.length > 0).toMap

    // it's a child if the current character has a descendant tag for it
    val children2 =  allCharacters.map(char => {
      val tags = filterTags(node.tags, DescendantTags).filter(x => {
        val matchChar = tagCharacter(x)
        tagCharacter(x).map(_.id.equals(char.id)).getOrElse(false)
      })
      (char, tags.map(_.kind))
    }).filter(_._2.length > 0).toMap

    // distinct children / tags
    val distinctChildren = children1 ++ children2.map{
      case (k, v) => k -> (v ++ children1.getOrElse(k, Nil)).distinct}

    val childrenEntries = distinctChildren.toList.map({case (child, tagKinds) => {
      // remove the current node from the character list to avoid loops in the tree
      val newAllCharacters = allCharacters.filter(x => !x.id.equals(node.id))
      val parentType = if (tagKinds.contains("ancestor") || tagKinds.contains("descendant")) {
        "ancestor"
      } else {
        "parent"
      }
      buildTree(child, newAllCharacters, parentType, np)
    }})

    val description = np.transform(
        node.notes.split("\n").filter(_.length > 0).headOption.getOrElse("")).replaceAll("\"", "\\\\\"")

    TreeEntry(node.id, node.name, description, "character", parentType, childrenEntries)
  }


  // convert a TreeEntry tree into JavaScript code for use with D3
  def treeToJs(te: TreeEntry): String = {

    val childrenString = if (te.children.length > 0) {
      s"children: [\n${te.children.map(treeToJs(_)).mkString(",\n")}]"
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
