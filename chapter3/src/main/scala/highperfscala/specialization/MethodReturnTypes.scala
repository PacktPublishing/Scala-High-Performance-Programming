package highperfscala.specialization

object MethodReturnTypes {

  class Foo[T](t: T)

  object Foo {
    def create[T](t: T): Foo[T] = new Foo(t)
    def createSpecialized[@specialized T](t: T): Foo[T] = new Foo(t)
  }

  def boxed: Foo[Int] = Foo.create(1)

  def specialized: Foo[Int] = Foo.createSpecialized(1)
}
