// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Generate text.

package bdzimmer.secondary.export.controller

import scala.collection.immutable.Vector
import scala.collection.immutable.Seq

import scala.util.Random


object MarkovText {

  type MarkovModel = scala.collection.immutable.Map[Ngram, Vector[Word]]

  trait Word {
    val text: String
  }

  case class StartWord(text: String) extends Word
  case class AnyWord(text: String) extends Word
  case class EndWord(text: String) extends Word

  case class Ngram(words: Vector[Word]) {
    val string = words.map(_.text).mkString(" ")
    override def toString(): String = string
  }


  def fit(text: String, order: Int): MarkovModel = {

    val words = text.split("\\s+").map(buildWord(_))

    val sentences = scala.collection.mutable.Buffer[Vector[Word]]()
    val sentence = scala.collection.mutable.Buffer[Word]()

    words.foreach(word => {

      sentence += word

      // all sentences will end with an EndWord
      if (word.isInstanceOf[EndWord]) {
        val sentenceFixed = if (sentence.size > 1) {
          StartWord(sentence.head.text) +: sentence.tail
        } else {
          sentence
        }
        sentences += sentenceFixed.toVector
        sentence.clear()
      }

    })

    val model = (for {
      sentence <- sentences.toVector
      pair     <- pairs(sentence, order)
    } yield {
      pair
    }).groupBy(_._1).map({
      case (k, v) => (k, v.map(_._2).toVector)
    })

    model

  }


  def predict(model: MarkovModel, rnd: Random): String = {

    val sentence = scala.collection.mutable.Buffer[Word]()

    // val keys = model.keys.toVector
    val keys = model.keys.filter(
        key => key.words.headOption.fold(false)(_.isInstanceOf[StartWord])).toVector

    if (keys.size > 0) {

      var prefix = keys(rnd.nextInt(keys.length))
      sentence ++= prefix.words

      var suffix: Word = AnyWord("")

      while (!suffix.isInstanceOf[EndWord] && sentence.size < 250) {
        // the prefix should always be in the model.
        val suffixes = model(prefix)
        suffix = suffixes(rnd.nextInt(suffixes.length))
        sentence.append(suffix)

        // new prefix
        prefix = Ngram(prefix.words.drop(1) :+ suffix)
      }

    }

    sentence.map(_.text).mkString(" ")

  }


  private def buildWord(text: String): Word = {
    if (text.endsWith(".")) {
      val lower = text.toLowerCase
      val res = lower match {
        case "mr." | "mrs." | "ms." | "st." => AnyWord(text)
        case _ => EndWord(text)
      }
      res
    } else {
      AnyWord(text)
    }

  }


  private def pairs(words: Seq[Word], order: Int): Vector[(Ngram, Word)] = {
    if (order > 1 && words.size > order) {
      words.sliding(order + 1).map(group => {
      // words.grouped(order + 1).map(group => {
        (Ngram(group.dropRight(1).toVector), group.last)
      }).toVector
    } else {
      Vector()
    }

  }

}
