package fpinscala.chapters

object Ch3 extends App with Chapter {
  // 3.1 - using std list cons instead of their example
  def patternMatch(): Int = {
    List(1,2,3,4,5) match {
      case x :: 2 :: 4 :: _ => x
      case Nil => 42
      case x :: y :: 3 :: 4 :: _ => x + y
      case h :: t => h + t.sum
      case _ => 101
    }
  }

  // 3.2
  def tail[A](list: List[A]): List[A] = {
    list match {
      case _ :: t => t
      case Nil => Nil
    }
  }

  // 3.3
  def setHead[A](list: List[A], a: A): List[A] = {
    list match {
      case Nil => a :: Nil
      case _ :: t => a :: t
    }
  }

  // 3.4
  @annotation.tailrec
  def drop[A](list: List[A], n: Int): List[A] = {
    if (n <= 0) {
      list
    } else {
      list match {
        case Nil => Nil
        case _ :: t => drop(t, n - 1)
      }
    }
  }

  // 3.5
  @annotation.tailrec
  def dropWhile[A](list: List[A], pred: (A) => Boolean): List[A] = {
    list match {
      case Nil => Nil
      case h :: t => {
        if (pred(h)) dropWhile(t, pred)
        else list
      }
    }
  }

  // 3.6
  def init[A](list: List[A]): List[A] = {
    list match {
      case Nil => Nil
      case h :: t :: Nil => h :: Nil
      case a :: b :: t => a :: b :: init(t)
    }
  }

  override def main(args: Array[String]): Unit = {
    assertEq(patternMatch(), 3)
    assertEq(List(2,3), tail(List(1,2,3)))
    assertEq(Nil, tail(Nil))
    assertEq(Nil, tail(List(1)))

    assertEq(List(4,2,3), setHead(List(1,2,3), 4))
    assertEq(List(1), setHead(List(), 1))

    assertEq(List(4,5), drop(List(1,2,3,4,5), 3))
    assertEq(List(), drop(List(1,2,3,4,5), 8))
    assertEq(List(1), drop(List(1), -1))

    def isEven(n: Int) = n % 2 == 0

    assertEq(List(1), dropWhile(List(2, 4, 6, 1), isEven))
    assertEq(List(), dropWhile(List(2), isEven))

    assertEq(List(1,2,3), init(List(1,2,3,4)))
    assertEq(List(), init(List()))
    assertEq(List(1), init(List(1,2)))
  }
}
