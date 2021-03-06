// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Functions for deriving lifespans and family relationships from characters' tags.

// TODO: Geneaology still uses RawTags instead of ParsedTags - need to fix

package bdzimmer.secondary.export.controller

import scala.collection.immutable.Seq

import bdzimmer.secondary.export.model.Tags.RawTag
import bdzimmer.secondary.export.model.SecTags
import bdzimmer.secondary.export.model.WorldItems.CharacterItem

import bdzimmer.orbits.{DateTupleParser, DateTime}


object Genealogy {

  val AncestorTags = Set("father", "mother", "parent", "ancestor")
  val DescendantTags = Set("son", "daughter", "child", "descendant")
  val MarriageTags = Set("marriage")

  val dtp = new DateTupleParser(DateTime.DefaultMonths)


  // get a text description of a character's lifespan
  def lifespan(character: CharacterItem): String = {
    val birthOpt = character.tags.values.filter(_.kind.equals(SecTags.Birth)).headOption
    val deathOpt = character.tags.values.filter(_.kind.equals(SecTags.Death)).headOption

    val birth = birthOpt.fold("")(x => dtp.parse(x.value)._1.toString)
    val death = deathOpt.fold("")(x => " - " + dtp.parse(x.value)._1)

    birth + death
  }


  // find the distinct children of a character
  def findDistinctChildren(
      character: CharacterItem,
      allCharacters: Seq[CharacterItem]): Seq[(CharacterItem, String)] = {

    // TODO: might want to double check that toMap.toList is doing what I think
    findRelatedCharacters(character, allCharacters, DescendantTags, AncestorTags).toMap.toList

  }


  // given a character as well as its distinct children and their relationships,
  // find their parents and group
  def findParents(
      character: CharacterItem,
      children: Seq[(CharacterItem, String)],
      allCharacters: Seq[CharacterItem]): Map[Option[CharacterItem], Seq[(CharacterItem, String)]] = {

    val allExceptThisChar = allCharacters.filter(!_.id.equals(character.id))

    val parentsByChild = children.map({case (child, rel) => {
      val otherParent = findRelatedCharacters(child, allExceptThisChar, AncestorTags, DescendantTags).headOption
      val (matchedParent, matchedParentRel) = otherParent match {
        case Some((parent, parentRel)) => (Some(parent), parentRel)
        case None => (None, getParentType(rel))
      }

      (matchedParent, (child, matchedParentRel))
    }})

    // seems like this should be less complicated
    val result = parentsByChild.groupBy(_._1).mapValues(_.map(_._2))

    val extraSpouses = findRelatedCharacters(character, allCharacters, MarriageTags, MarriageTags)
    val extraSpousesMap = extraSpouses.map(x => (Option(x._1), Seq[(CharacterItem, String)]())).toMap

    extraSpousesMap ++ result

  }


  // find related characters by searching tags in two directions
  def findRelatedCharacters(
      character: CharacterItem,
      allCharacters: Seq[CharacterItem],
      outTagKinds: Set[String], inTagKinds: Set[String]): Seq[(CharacterItem, String)] = {

    // outward-directed relationships
    val outCharacters = for {
      char <- allCharacters
      tagValues = character.tags.values.toList
      outTag <- filterTags(tagValues, outTagKinds)
      tagChar <- tagCharacter(outTag, allCharacters)
      if tagChar.id.equals(char.id)
    } yield (tagChar, outTag.kind)

    // inward-directed relationships
    val inCharacters = for {
      char <- allCharacters
      tagValues = char.tags.values.toList
      inTag <- filterTags(tagValues, inTagKinds)
      tagChar <- tagCharacter(inTag, allCharacters)
      if tagChar.id.equals(character.id)
    } yield (char, inTag.kind)

    outCharacters ++ inCharacters
  }


  def getParentType(x: String): String = {
    if (x.equals("ancestor") || x.equals("descendant")) x else "parent"
  }

  private def filterTags(tags: Seq[RawTag], kinds: Set[String]): Seq[RawTag] = {
    tags.filter(x => kinds.contains(x.kind))
  }

  private def filterTags(tags: Seq[RawTag], kind: String): Seq[RawTag] = {
    tags.filter(x => x.kind.equals(kind))
  }

  private def tagCharacter(tag: RawTag, allCharacters: Seq[CharacterItem]): Option[CharacterItem] = {
    allCharacters.filter(x => {
      x.id.equals(tag.value) || x.name.equals(tag.value)
    }).headOption
  }

}
