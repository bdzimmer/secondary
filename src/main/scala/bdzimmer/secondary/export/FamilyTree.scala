// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Generate family trees using tags in characters.

// 2015-10-05: Created. Spouse lines not supported yet.
// 2015-10-07: Updates for previous and ancestor vs. parent relationships.

package bdzimmer.secondary.export


case class TreeEntry(
    id: String,
    name: String,
    description: String,
    hidden: Boolean,
    parentType: String,
    children: List[TreeEntry])


object FamilyTree {

  val AncestorTags = Set("father", "mother", "parent", "ancestor")
  val DescendantTags = Set("son", "daughter", "descendant")

  // for now, all of the family trees
  def getJs(characters: List[CharacterItem], np: NotesParser): String = {

    def isRoot(char: CharacterItem): Boolean = {
      val charTagKinds = char.tags.map(_.kind).toSet
      AncestorTags.intersect(charTagKinds).size == 0
    }

    // root characters are those without a parent or ancestor
    val rootChars = characters.filter(isRoot)
    val nonRootChars = characters.filter(!isRoot(_))

    // build trees for the root characters, but toss the ones that don't have any children
    val trees = rootChars.map(x => buildTree(x, characters, true, "none", np)).filter(_.children.length > 0)

    // add a hidden parent to a tree
    def wrapTree(te: TreeEntry): String = {
      "var root = " +
      treeToJs(TreeEntry("root", "", "", true, "none", List(te))) + ";\n" +
      "var spouses = [];\n"
    }

    /*
    Tags.p("Root characters: ") + Tags.listGroup(rootChars.map(x => Tags.listItem(x.name))) +
    Tags.p("Non-root characters: ") + Tags.listGroup(nonRootChars.map(x => Tags.listItem(x.name))) +
    ("<pre>" + jsObjects.mkString("\n\n------\n\n") + "</pre>")
    */

    """<script src="https://cdnjs.cloudflare.com/ajax/libs/d3/3.5.6/d3.min.js" charset="utf-8"></script>""" +
    """<link href="tree/tree.css" rel="stylesheet">""" +
    """<script src="tree/drawtree.js" charset="utf-8"></script>""" +
    (trees.map(tree => {
      s"""<div id="${tree.id + "_tree"}"export></div>\n""" +
      s"""<script>${wrapTree(tree)}\ndrawTree(root, "#${tree.id + "_tree"}", 800, 640)</script>\n"""
    }).mkString("\n" + Tags.hr + "\n"))

  }


  // build family tree given a root character and a list of characters to
  // search for descendants.
  def buildTree(
      node: CharacterItem,
      allCharacters: List[CharacterItem],
      linkHidden: Boolean,
      parentType: String,
      np: NotesParser): TreeEntry = {

    // println(s"building tree for ${node.id}")

    def tagCharacter(tag: SecTag) = allCharacters.filter(_.id.equals(tag.value)).headOption

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
        // println("   " + node.id + " has " + x + " " + matchChar.map(_.id).getOrElse("none"))
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

      buildTree(child, newAllCharacters, false, parentType, np)
    }})

    // take the first paragraph
    val description = np.transform(
        node.notes.split("\n").filter(_.length > 0).headOption.getOrElse("")).replaceAll("\"", "\\\\\"")

    TreeEntry(node.id, node.name, description, false, parentType, childrenEntries)

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
  hidden: ${te.hidden},
  parent_type: "${te.parentType}",
  ${childrenString}
}"""

  }
}
