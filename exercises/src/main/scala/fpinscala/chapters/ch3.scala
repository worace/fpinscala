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
  def dropWhile[A](list: List[A])(pred: (A) => Boolean): List[A] = {
    list match {
      case Nil => Nil
      case h :: t => {
        if (pred(h)) dropWhile(t)(pred)
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

  // 3.7-10
  def foldRight[A, B](list: List[A], zero: B)(f: (A, B) => B): B = {
    list match {
      case Nil => zero
      case h :: t => f(h, foldRight(t, zero)(f))
    }
  }

  // 3.10
  @annotation.tailrec
  def foldLeft[A, B](list: List[A], zero: B)(f: (B, A) => B): B = {
    list match {
      case Nil => zero
      case h :: t => foldLeft(t, f(zero, h))(f)
    }
  }

  def append[A](list: List[A], el: A): List[A] = {
    foldRight(list, el :: Nil)((a, b) => {
      a :: b
    })
  }

  def concat[A](lists: List[List[A]]): List[A] = {
    // 1,2,3
    // 4,5 -> 1,2,3,4,5

    // foldRight(lists, List[A]())((currList, appList) => {
    //   foldRight(currList, appList)((el, appList) => {
    //     el :: appList
    //   })
    // })

    foldLeft(lists, List[A]())((appList, currList) => {
      foldLeft(currList: List[A], appList: List[A])((appList: List[A], el: A) => {
        append(appList, el)
      })
    })
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, List[B]())((a, b) => {
      f(a) :: b
    })
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, List[A]())((a, b) => {
      if (f(a)) {
        a :: b
      } else {
        b
      }
    })
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, List[B]())((a, b) => {
      foldRight(f(a), b)((a, b) => {
        a :: b
      })
    })
  }

  def fmapFilter[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) { a :: Nil } else { Nil })
  }

  def zipWith[A](a: List[A], b: List[A])(f: (A, A) => A): List[A] = {
    (a, b) match {
      case (Nil, Nil) => Nil
      case (Nil, hb) => hb
      case (ha, Nil) => ha
      case (ha :: taila, hb :: tailb) => f(ha, hb) :: zipWith(taila, tailb)(f)
    }
  }

  def trees(): Unit = {
    sealed trait Tree[+A]
    case class Leaf[A](value: A) extends Tree[A]
    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

    // 3.25
    def size[A](t: Tree[A]): Int = {
      t match {
        case Leaf(_) => 1
        case Branch(l, r) => size(l) + size(r)
      }
    }

    // 3.26
    def treeMax(t: Tree[Int], maxSoFar: Int = 0): Int = {
      t match {
        case Leaf(i) => i.max(maxSoFar)
        case Branch(l, r) => treeMax(l, maxSoFar).max(treeMax(r, maxSoFar))
      }
    }

    // 3.27
    def treeDepth[T](t: Tree[T]): Int = {
      t match {
        case Leaf(i) => 1
        case Branch(l, r) => (1 + treeDepth(l)).max(1 + treeDepth(r))
      }
    }

    // 3.28
    def treeMap[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
      tree match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l, r) => Branch(treeMap(l)(f), treeMap(r)(f))
      }
    }

    val testTree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    val testTree2 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(8), Branch(Leaf(2), Leaf(4))))

    assertEq(3, size(testTree))

    assertEq(3, treeMax(testTree))
    assertEq(8, treeMax(testTree2))

    assertEq(3, treeDepth(testTree))
    assertEq(4, treeDepth(testTree2))

    val mapped1 = Branch(Branch(Leaf(2), Leaf(3)), Leaf(4))
    val mapped2 = Branch(Branch(Leaf(2), Leaf(4)), Branch(Leaf(16), Branch(Leaf(4), Leaf(8))))

    assertEq(mapped1, treeMap(testTree)(_ + 1))
    assertEq(mapped2, treeMap(testTree2)(_ * 2))

    def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = {
      tree match {
        case Leaf(v) => f(v)
        case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
      }
    }

    // size
    assertEq(3, fold(testTree)(_ => 1)(_ + _))
    assertEq(5, fold(testTree2)(_ => 1)(_ + _))

    // max
    assertEq(3, fold(testTree)(i => i)(_.max(_)))
    assertEq(8, fold(testTree2)(i => i)(_.max(_)))

    // depth
    assertEq(3, fold(testTree)(_ => 1)((a, b) => (a + 1).max(b + 1)))
    assertEq(4, fold(testTree2)(_ => 1)((a, b) => (a + 1).max(b + 1)))

    //map
    assertEq(mapped1, fold[Int, Tree[Int]](testTree)(l => Leaf(l + 1))((a, b) => Branch(a,b)))
    assertEq(mapped2, fold[Int, Tree[Int]](testTree2)(l => Leaf(l * 2))((a, b) => Branch(a,b)))
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

    assertEq(List(1), dropWhile(List(2, 4, 6, 1))(n => n % 2 == 0))
    assertEq(List(), dropWhile(List(2))(isEven))

    assertEq(List(1,2,3), init(List(1,2,3,4)))
    assertEq(List(), init(List()))
    assertEq(List(1), init(List(1,2)))

    // 3.7
    assertEq(4, foldRight(List(1,1,1,1), 0)(_ + _))
    assertEq(24, foldRight(List(2.0,3.0,4.0), 1.0)(_ * _))

    // 3.8
    assertEq(0, foldRight(List[Int](), 0)(_ + _))

    // 3.9
    assertEq(4, foldRight(List(1,2,3,4), 0)((_a,b) => b + 1))
    assertEq(1, foldRight(List(1), 0)((_a,b) => b + 1))

    // 3.10
    assertEq(4, foldLeft(List(1,2,3,4), 0)((b,_a) => b + 1))

    // 3.11
    assertEq(4, foldLeft(List(1,1,1,1), 0)(_ + _))
    assertEq(24, foldLeft(List(2.0,3.0,4.0), 1.0)(_ * _))
    assertEq(4, foldLeft(List(1,2,3,4), 0)((b,_a) => b + 1))

    // 3.12
    assertEq(List(4,3,2,1), foldLeft(List(1,2,3,4), List[Int]())((b,a) => a :: b))

    // 3.14
    assertEq(List(1,2,3), append(List(1,2), 3))

    // 3.15
    assertEq(List(1,2,3,4,5), concat(List(List(1,2,3), List(4,5))))
    assertEq(List(1,2,3,4,5,6,7), concat(List(List(1,2,3), List(4,5), List(6), List(7))))

    // 3.16
    val plus1 = foldRight(List(1,2,3), List[Int]())((a: Int, b: List[Int]) => {
      (a + 1) :: b
    })
    assertEq(List(2,3,4), plus1)

    // 3.17
    val strs = foldRight(List(1.0,2.0,3.0), List[String]())((a, b) => {
      a.toString :: b
    })
    assertEq(List("1.0","2.0","3.0"), strs)

    // 3.18
    assertEq(List(2,3,4), map(List(1,2,3))(_ + 1))

    // 3.19
    assertEq(List(2,4,6), filter(List(1,2,3,4,5,6))(_ % 2 == 0))

    // 3.20
    assertEq(List(0,1,0,1,2), flatMap(List(1,2))(i => (0 to i).toList))

    // 3.21
    assertEq(List(2,4,6), fmapFilter(List(1,2,3,4,5,6))(_ % 2 == 0))

    // 3.22 + 3.23
    assertEq(List(5,7,9), zipWith(List(1,2,3), List(4,5,6))(_ + _))

    trees()
  }
}
