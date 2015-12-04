// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Minimal implementaion of a right-biased Either.

// Other salient features:
// - No "get" method.
// - Companion object for easy creation from expressions and other types.


package bdzimmer.util

import scala.util.{Try, Success, Failure}
import java.io.File

sealed abstract class Result[+A, +B] {

  // map right side
  def map[C](f: B => C): Result[A, C] = this match {
    case x @ Fail(_) => x
    case Pass(x) => Pass(f(x))
  }

  // map left side
  def mapLeft[C](f: A => C): Result[C, B] = this match {
    case Fail(x) => Fail(f(x))
    case x @ Pass(_) => x
  }

  // foreach right side
  def foreach[C](f: B => C): Unit = this.map(f)

  // bind right side
  def flatMap[AA >: A, C](f: B => Result[AA, C]): Result[AA, C] = this match {
    case x @ Fail(_) => x
    case Pass(x) => f(x)
  }

}


final case class Fail[+A](a: A) extends Result[A, Nothing]
final case class Pass[+B](b: B) extends Result[Nothing, B]


object Result {

  // create from a random expression
  def apply[A](a: => A): Result[String, A] = {
    fromTry(Try(a))
  }

  // create from Option
  def fromOption[A, B](o: Option[B], default: A): Result[A, B] = o match {
    case Some(x) => Pass(x)
    case None => Fail(default)
  }

  // create from Try
  def fromTry[A](t: Try[A]): Result[String, A] = t match {
    case Success(x) => Pass(x)
    case Failure(e) => Fail(e.getMessage)
  }

  // create from a filename
  def fromFilename(filename: String): Result[String, File] = {
    val file = new File(filename)
    if (file.exists) {
      Pass(file)
    } else {
      Fail(s"'${file.getPath}' does not exist")
    }
  }

}
