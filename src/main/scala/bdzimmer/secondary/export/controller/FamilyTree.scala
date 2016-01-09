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
    children: List[TreeEntry],
    marriages: List[Marriage])

// marriages are wierd because they sort of turn spouses into siblings
case class Marriage(
    hidden: TreeEntry,
    spouse: TreeEntry)

object FamilyTree {

  val AncestorTags = Set("father", "mother", "parent", "ancestor")
  val DescendantTags = Set("son", "daughter", "descendant")

  val TreeStyles =
    """<script src="https://cdnjs.cloudflare.com/ajax/libs/d3/3.5.6/d3.min.js" charset="utf-8"></script>""" + "\n" +
    """<link href="tree/tree.css" rel="stylesheet">""" + "\n" +
    """<script src="tree/drawtree.js" charset="utf-8"></script>""" + "\n"


  // for now, all of the family trees
  def getAllJs(characters: List[CharacterItem], np: RenderSecTags): String = {

    def isRoot(char: CharacterItem): Boolean = {
      val charTagKinds = char.tags.map(_.kind).toSet
      AncestorTags.intersect(charTagKinds).size == 0
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
      tree.marriages.map(x => (tree.id, x.spouse.id)) ++ tree.children.flatMap(getAllMarriages)
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
        matchChar.map(_.id.equals(char.id)).getOrElse(false)
      })
      (char, tags.map(_.kind))
    }).filter(_._2.length > 0).toMap

    // distinct children / tags
    val distinctChildren = children1 ++ children2.map{
      case (k, v) => k -> (v ++ children1.getOrElse(k, Nil)).distinct}

    // group children by marriage (or no marriage)
    val childrenBySpouse = distinctChildren.toList.map(x => {
      val child = x._1

      // keep the ancestors that are not the current character
      val otherParent = filterTags(child.tags, AncestorTags).map(ancestorTag => {
        tagCharacter(ancestorTag).map(char => (char, ancestorTag.kind))
      }).flatten.filter(!_._1.id.equals(node.id)).headOption

      // TODO: check other characters for descendant tags with this child!
      // Need two-way checking, just like children above

      val matchedParent = otherParent match {
        case Some((parent, rel)) => (Some(parent), rel)

        // do I really need to be carrying lists of tag kinds until this point?
        case None => (None, if (x._2.contains("ancestor") || x._2.contains("descendant")) {
          "ancestor"
        } else {
          "parent"
        })
      }

      (matchedParent, child)

    }).groupBy(_._1).mapValues(_.map(_._2)) // seems like this should be less complicated

    val newAllCharacters = allCharacters.filter(x => !x.id.equals(node.id))

    def getParentType(x: String) = if (x.equals("ancestor") || x.equals("descendant")) x else "parent"

    val marriages = childrenBySpouse.toList.collect({case ((Some(parent), rel), children) => {
      Marriage(
          TreeEntry(
              node.id + "_" + parent.id, "", "", "marriage", "none",
              children.map(child => {
                buildTree(child, newAllCharacters.filter(_.id.equals(parent.id)), getParentType(rel), np)
              }), List()),
          TreeEntry(parent.id, parent.name, "", "character", "none", List(), List()))
    }})

    val singleParentChildren = childrenBySpouse.toList.collect({case ((None, rel), children) => {
      children.map(child => {
        buildTree(child, newAllCharacters, getParentType(rel), np)
      })
    }}).flatten

    val description = np.transform(
        node.notes.split("\n").filter(_.length > 0).headOption.getOrElse("")).replaceAll("\"", "\\\\\"")

    println(node.name)
    println("single parent children: " + singleParentChildren.map(_.name))
    println("marriages: " + marriages.map(x => x.spouse.name + " " + x.hidden.children.map(_.name)))

    println("***")

    TreeEntry(node.id, node.name, description, "character", parentType, singleParentChildren, marriages)
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
