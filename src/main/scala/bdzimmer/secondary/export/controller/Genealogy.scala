// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Functions for deriving lifespans and family relationships from characters' tags.

package bdzimmer.secondary.export.controller

import bdzimmer.secondary.export.model.{CharacterItem, SecTag, SecTags}

object Genealogy {

  val AncestorTags = Set("father", "mother", "parent", "ancestor")
  val DescendantTags = Set("son", "daughter", "descendant")

  val timeline = new Timeline(Timeline.DefaultMonths)

  // get a text description of a character's lifespan
  def lifespan(character: CharacterItem): String = {
    val birthOpt = character.tags.filter(_.kind.equals(SecTags.Birth)).headOption
    val deathOpt = character.tags.filter(_.kind.equals(SecTags.Death)).headOption

    val birth = birthOpt.fold("")(x => timeline.parseDateTuple(x.value)._1.toString)
    val death = deathOpt.fold("")(x => " - " + timeline.parseDateTuple(x.value)._1)

    birth + death
  }

  // find the distinct children of a character
  def findDistinctChildren(
      character: CharacterItem,
      allCharacters: List[CharacterItem]): List[(CharacterItem, String)] = {

    // it's a child if it has an ancestor tag for the current character
    val children1 = for {
      char <- allCharacters
      ancestorTag <- filterTags(char.tags, AncestorTags)
      tagChar <- tagCharacter(ancestorTag, allCharacters)
      if tagChar.id.equals(character.id)
    } yield (char, ancestorTag.kind)

    // child if the current character as a descendant tag for it
    val children2 = for {
      char <- allCharacters
      descendantTag <- filterTags(character.tags, DescendantTags)
      tagChar <- tagCharacter(descendantTag, allCharacters)
      if tagChar.id.equals(char.id)
    } yield (tagChar, descendantTag.kind)

    // distinct children / tags
    (children1 ++ children2).toMap.toList

  }


  // given a character as well as its distinct children and their relationships,
  // find their parents and group
  def findParents(
      character: CharacterItem,
      children: List[(CharacterItem, String)],
      allCharacters: List[CharacterItem]): Map[(Option[CharacterItem], String), List[CharacterItem]] = {

    children.map({case (child, rel) => {

      // keep the ancestors of the child that are not the current character
      val otherParents1 = for {
        ancestorTag <- filterTags(child.tags, AncestorTags)
        tagChar <- tagCharacter(ancestorTag, allCharacters)
        if !tagChar.id.equals(character.id)
      } yield (tagChar, ancestorTag.kind)

      // check other characters for descendant tags with this child
      val otherParents2 = for {
        char <- allCharacters
        if !char.equals(character.id)
        descendantTag <- filterTags(child.tags, DescendantTags)
        tagChar <- tagCharacter(descendantTag, allCharacters)
        if !tagChar.id.equals(child.id)
      } yield (char, descendantTag.kind)

      val otherParent = (otherParents1 ++ otherParents2).headOption

      val matchedParent = otherParent match {
        case Some((parent, rel)) => (Some(parent), rel)
        case None => (None, getParentType(rel))
      }

      (matchedParent, child)

    }}).groupBy(_._1).mapValues(_.map(_._2)) // seems like this should be less complicated

  }


  def getParentType(x: String): String = {
    if (x.equals("ancestor") || x.equals("descendant")) x else "parent"
  }


  private def filterTags(tags: List[SecTag], kinds: Set[String]): List[SecTag] = {
    tags.filter(x => kinds.contains(x.kind))
  }

  private def tagCharacter(
      tag: SecTag, allCharacters: List[CharacterItem]) = allCharacters.filter(x => {
    x.id.equals(tag.value) || x.name.equals(tag.value)
  }).headOption


}
