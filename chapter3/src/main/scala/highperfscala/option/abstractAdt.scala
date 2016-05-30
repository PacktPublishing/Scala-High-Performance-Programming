package highperfscala.option

import scala.language.higherKinds

// Taken from https://bertails.org/2015/02/15/abstract-algebraic-data-type/
trait OptionSig {
  type Option[+_]
  type Some[+A] <: Option[A]
  type None <: Option[Nothing]
}

abstract class OptionOps[Sig <: OptionSig] {
  def some[A](x: A): Sig#Some[A]
  def none: Sig#None
  def fold[A, B](opt: Sig#Option[A])(ifNone: => B, ifSome: A => B): B
}

object OptionOps {
  def apply[Sig <: OptionSig](implicit ops: OptionOps[Sig]): OptionOps[Sig] =
    ops
}

import scalaz.Show

class OptionShow[Sig <: OptionSig : OptionOps] {

  def optionShow[A: Show]: Show[Sig#Option[A]] = {

    // retrieving the typeclass instances
    val showA = Show[A]
    val ops = OptionOps[Sig]

    val instance = new Show[Sig#Option[A]] {
      override def shows(opt: Sig#Option[A]): String = ops.fold(opt)(
        "none",
        x => s"some(${showA.shows(x)})"
      )
    }

    instance
  }

}

object OptionShow {

  implicit def apply[Sig <: OptionSig : OptionOps]: OptionShow[Sig] =
    new OptionShow[Sig]
}

trait ScalaOption extends OptionSig {

  type Option[+A] = scala.Option[A]
  type Some[+A] = scala.Some[A]
  type None = scala.None.type

}

object ScalaOption {

  implicit object ops extends OptionOps[ScalaOption] {

    def some[A](x: A): ScalaOption#Some[A] = scala.Some(x)

    val none: ScalaOption#None = scala.None

    def fold[A, B](opt: ScalaOption#Option[A])(ifNone: => B, ifSome: A => B): B =
      opt match {
        case scala.None => ifNone
        case scala.Some(x) => ifSome(x)
      }

  }

}

trait NullOption extends OptionSig {

  type Option[+A] = Any
  type Some[+A] = Any
  type None = Null

}

object NullOption {

  implicit object ops extends OptionOps[NullOption] {

    def some[A](x: A): NullOption#Some[A] = x

    val none: NullOption#None = null

    def fold[A, B](opt: NullOption#Option[A])(ifNone: => B, ifSome: A => B): B = {
      if (opt == null) ifNone
      else ifSome(opt.asInstanceOf[A])
    }

  }

}
