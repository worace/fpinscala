package fpinscala.chapters

trait Chapter {
  def assertEq[A](a: A, b: A): Unit = {
    val res = a == b
    println(s"$a == $b: $res")
  }
}
