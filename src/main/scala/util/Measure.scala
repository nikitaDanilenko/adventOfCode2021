package util

object Measure {

  def measure[A](a: => A): A = {
    val start = System.currentTimeMillis()
    val result = a
    val end = System.currentTimeMillis()
    pprint.log(s"${end - start}ms")
    result
  }

}
