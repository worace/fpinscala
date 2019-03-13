package fpinscala.chapters

object Ch2 extends App with Chapter {
  // 2.1
  // @annotation.tailrec
  def nthFib(n: Long): Long = {
    @annotation.tailrec
    def go(prev1: Long, prev2: Long, nth: Long): Long = {
      if (nth == n) prev1 + prev2
      else go(prev2, prev1 + prev2, nth + 1)
    }

    if (n == 0) 0
    else if (n == 1) 1
    else go(0, 1, 2)
  }

  // 2.2
  def isSorted[A](arr: Array[A], ordering: (A, A) => Boolean): Boolean = {
    def loop(i: Int): Boolean = {
      if (i == arr.size - 1) true
      else if (ordering(arr(i), arr(i+1))) loop(i + 1)
      else false
    }
    if (arr.size < 2) true
    else loop(0)
  }

  // 2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a) => (b) => f(a,b)
  }

  // 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  // 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a) => f(g(a))
  }

  override def main(args: Array[String]): Unit = {
    assertEq(nthFib(0), 0)
    assertEq(nthFib(1), 1)
    assertEq(nthFib(2), 2)
    assertEq(nthFib(8), 21)
    assertEq(nthFib(50l), 12586269025l)

    def ord(a: Int, b: Int): Boolean = a < b
    assertEq(isSorted(Array(0,1,2,3), ord), true)
    assertEq(isSorted(Array(0,1,4,3), ord), false)

    val add2 = (a: Int, b: Int) => a + b
    val addCurr = curry(add2)
    assertEq(addCurr(1)(2), 3)

    assertEq(uncurry(addCurr)(1, 2), 3)

    def plus2(a: Int) = a + 2
    def times3(a: Int) = a * 3
    assertEq(compose(plus2, times3)(2), 12)
    assertEq(compose(times3, plus2)(2), 8)
  }
}
