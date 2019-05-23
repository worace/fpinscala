package fpinscala.chapters

import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }
  }

  def getOrElse[B>:A](default: => B): B = {
    this match {
      case None => default
      case Some(a) => a
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }

  def filter(f: A => Boolean): Option[A] = {
    flatMap(a => if (f(a)) Some(a) else None)
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = {
    this match {
      case None => ob
      case Some(a) => Some(a)
    }
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Ch4 extends App with Chapter {
  override def main(args: Array[String]): Unit = {
    assertEq(Some(2), Some(1).map(_ + 1))
    assertEq(None, None.map(_.toString))
    assertEq(Some(1), Some(1).flatMap(i => Some(i)))
    assertEq(None, None.flatMap(i => Some(i)))
    assertEq(2, Some(2).getOrElse(3))
    assertEq(3, None.getOrElse(3))
    assertEq(Some(2), Some(2).filter( _ % 2 == 0 ))
    assertEq(None, Some(3).filter( _ % 2 == 0 ))
    assertEq(Some(3), Some(3).orElse(Some(4)))
    assertEq(Some(4), None.orElse(Some(4)))
  }
}
