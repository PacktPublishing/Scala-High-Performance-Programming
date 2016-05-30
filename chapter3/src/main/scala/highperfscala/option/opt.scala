package highperfscala.option

import scalaz.{@@, Tag}

sealed trait Opt

// Allocation free Option implementation inspired by the
// abstract ADTs using tagged types
object OptOps {

  def some[@specialized A](x: A): A @@ Opt = Tag(x)
  def nullCheckingSome[@specialized A](x: A): A @@ Opt =
    if (x == null) sys.error("Null values disallowed") else Tag(x)
  def none[A]: A @@ Opt = Tag(null.asInstanceOf[A])

  def isDefined[A](o: A @@ Opt): Boolean = o != null
  def isEmpty[A](o: A @@ Opt): Boolean = !isDefined(o)
  def unsafeGet[A](o: A @@ Opt): A =
    if (isDefined(o)) o.asInstanceOf[A] else sys.error("Cannot get None")

  def fold[A, B](o: A @@ Opt)(ifEmpty: => B)(f: A => B): B =
    if (isEmpty(o)) ifEmpty else f(o.asInstanceOf[A])
}
