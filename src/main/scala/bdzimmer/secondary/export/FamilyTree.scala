// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Generate family trees using tags in characters.

// 2015-10-05: Created. Spouse lines not supported yet.

package bdzimmer.secondary.export


case class TreeEntry(id: String, name: String, hidden: Boolean, noparent: Boolean, children: List[TreeEntry])


object FamilyTree {

  val AncestorTags = Set("father", "mother", "parent", "ancestor")
  val DescendantTags = Set("son", "daughter", "descendant")

  // for now, all of the family trees
  def getJs(characters: List[CharacterItem]): String = {

    def isRoot(char: CharacterItem): Boolean = {
      val charTagKinds = char.tags.map(_.kind).toSet
      AncestorTags.intersect(charTagKinds).size == 0
    }

    // root characters are those without a parent or ancestor
    val rootChars = characters.filter(isRoot)
    val nonRootChars = characters.filter(!isRoot(_))

    val jsObjects = rootChars.map(x => {
      "var root = " +
      treeToJs(TreeEntry("root", "", true, true, List(buildTree(x, characters, true)))) + ";\n" +
      "var spouses = [];\n"
    })

    /*
    Tags.p("Root characters: ") + Tags.listGroup(rootChars.map(x => Tags.listItem(x.name))) +
    Tags.p("Non-root characters: ") + Tags.listGroup(nonRootChars.map(x => Tags.listItem(x.name))) +
    ("<pre>" + jsObjects.mkString("\n\n------\n\n") + "</pre>")
    */

    """<script src="https://cdnjs.cloudflare.com/ajax/libs/d3/3.5.6/d3.min.js" charset="utf-8"></script>""" +
    """<link href="tree/tree.css" rel="stylesheet">""" +
    """<script src="tree/drawtree.js" charset="utf-8"></script>""" +
    (rootChars.zip(jsObjects)).map({case (ch, js) => {
      s"""<div id="${ch.id + "_tree"}"export></div>\n""" +
      s"""<script>${js}\ndrawTree(root, "#${ch.id + "_tree"}", 800, 640)</script>\n"""
    }}).mkString("\n" + Tags.hr + "\n")

  }


  // build family tree given a root character and a list of characters to
  // search for descendants.
  def buildTree(node: CharacterItem, allCharacters: List[CharacterItem], root: Boolean): TreeEntry = {

    // println(s"building tree for ${node.id}")

    def tagCharacter(tag: SecTag) = allCharacters.filter(_.id.equals(tag.value)).headOption

    def filterTags(tags: List[SecTag], kinds: Set[String]): List[SecTag] = {
      tags.filter(x => kinds.contains(x.kind))
    }

    // it's a child if it has an ancestor tag for the current character
    val childrenReferencingThis = allCharacters.filter(char => {
      filterTags(char.tags, AncestorTags).map(x => {
        val matchChar = tagCharacter(x)
        // println("   " + char.id + " has " + x + " " + matchChar.map(_.id).getOrElse("none"))
        matchChar.map(_.id.equals(node.id)).getOrElse(false)
      }).exists(identity)
    })

    // it's a child if the current character has a descendant tag for it
    val childrenReferencedByThis =  allCharacters.filter(char => {
      filterTags(node.tags, DescendantTags).map(x => {
        val matchChar = tagCharacter(x)
        // println("   " + node.id + " has " + x + " " + matchChar.map(_.id).getOrElse("none"))
        tagCharacter(x).map(_.id.equals(char.id)).getOrElse(false)
      }).exists(identity)
    })

    // distinct children
    val distinctChildren = (childrenReferencingThis ++ childrenReferencedByThis).distinct

    // println("allCharacters: " + allCharacters.map(_.id).mkString(","))
    // println("distinct children: " + distinctChildren.map(_.id).mkString(","))

    // remove the distinct children from the allCharactersList to avoid loops in the tree
    val childrenEntries = distinctChildren.map(child => {
      val newAllCharacters = allCharacters.filter(x => !x.id.equals(node.id))
      buildTree(child, newAllCharacters, false)
    })

    TreeEntry(node.id, node.name, false, root, childrenEntries)

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
  hidden: ${te.hidden},
  no_parent: ${te.noparent},
  ${childrenString}
}"""

  }
}
