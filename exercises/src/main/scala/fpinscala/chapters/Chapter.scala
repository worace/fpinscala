package fpinscala.chapters

trait Chapter {
  def assertEq[A](a: A, b: A): Unit = {
    if (a == b) {
      println(s"$a == $b: true")
    } else {
      throw new RuntimeException(s"Failure: $a != $b")
    }
  }
}
