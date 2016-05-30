package highperfscala.option

object OptionzTest {
  def main(args: Array[String]): Unit = {

    val some = OptOps.fold(OptOps.some(25))(7)(_ * 2)
    println(some)
    assert(some == 50)

    val none = OptOps.fold(OptOps.none[Int])(7)(_ * 2)
    println(none)
    assert(none == 7)
  }
}
