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

// 4.6
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
  }

  def orElse[EE >: E, B](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(_) => b
      case Right(a: B) => Right(a)
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    flatMap(a => b.map(b => (a, b))).map { case (a, b) => f(a, b) }
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Ch4 extends App with Chapter {

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) {
      None
    } else {
      Some(xs.sum / xs.size)
    }
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs)
      .map(m => xs.map(x => Math.pow(x - m, 2)))
      .flatMap(mean)
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = {
    a.flatMap(a => b.map(b => (a,b)))
      .map { case (a, b) => f(a,b) }
  }

  def sequence[A](opts: List[Option[A]]): Option[List[A]] = {
    // Some(1), Some(2) -> Some(1,2)
    // Some(1), None -> None
    val start: Option[List[A]] = Some(List[A]())
    opts.foldLeft(start) { (mList: Option[List[A]], x: Option[A]) =>
      mList.flatMap(list => x.map(x => list :+ x))
    }
  }

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    val result: Option[List[B]] = Some(List[B]())
    as.foldLeft(result) { (result: Option[List[B]], a: A) =>
      result.flatMap(list => f(a).map(b => list :+ b))
    }
  }

  object ExEither {

  }
  override def main(args: Array[String]): Unit = {
    // 4.1
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

    // https://www.mathsisfun.com/data/standard-deviation.html
    // 4.2
    val xs = Seq[Double](600, 470, 170, 430, 300)
    assertEq(Some(21704), variance(xs))
    assertEq(None, variance(Seq()))

    // 4.3
    assertEq(None, map2[Int, Int, Int](None, None)(_ + _))
    assertEq(None, map2[Int, Int, Int](None, Some(2))(_ + _))
    assertEq(None, map2[Int, Int, Int](Some(1), None)(_ + _))
    assertEq(Some(3), map2(Some(1), Some(2))(_ + _))

    // 4.4
    assertEq(Some(List(1,2)), sequence(List(Some(1), Some(2))))
    assertEq(None, sequence(List(Some(1), None)))

    // 4.5
    def maybeStr(i: Int): Option[String] = if (i % 2 == 0) { Some(i.toString) } else { None }
    assertEq(Some(List("2","4")), traverse(List(2, 4))(maybeStr))
    assertEq(None, traverse(List(2, 1))(maybeStr))
    assertEq(None, traverse(List(1, 2))(maybeStr))

    //
  }
}
