package highperfscala.specialization

object Specialization {

  case class ShareCount[@specialized(Long, Int) T](value: T)

  def newShareCount(l: Long): ShareCount[Long] = ShareCount(l)

  case class Foo[@specialized(Int, Long) X, @specialized(Int, Long) Y](
    value: X, result: Y)

  def makeFoo: Foo[Int, Int] = Foo(1, 2).copy(value = 22)


  case class Bar[X, Y](value: X, result: Y)

  def makeBar: Bar[Int, Int] = Bar(1, 2)

  case class Defined(value: Int, result: Int)

  def makeDefined: Defined = Defined(1, 2).copy(value = 22)
}
