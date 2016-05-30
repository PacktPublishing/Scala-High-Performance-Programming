package highperfscala.specialization

object Inheritance {

  class ParentFoo[@specialized T](t: T)
  class ChildFoo[T](t: T) extends ParentFoo[T](t)

  def newChildFoo(i: Int): ChildFoo[Int] = new ChildFoo(i)

  trait ParentBar[@specialized T] {
    def t(): T
  }

  class ChildBar[@specialized T](val t: T) extends ParentBar[T]

  def newChildBar(i: Int): ChildBar[Int] = new ChildBar(i)
}
