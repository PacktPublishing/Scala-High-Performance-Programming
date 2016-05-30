package highperfscala.patternmatch

import scala.annotation.switch
import scalaz.{@@, Tag}

object PatternMatching {

  sealed trait Event
  case object E1 extends Event
  case object E2 extends Event
  case object E3 extends Event

  sealed trait Foo

  case class IntBar(i: Int, b: Bar)
  def intBar: IntBar = IntBar(1, Bar(2))

  def tuple2Boxed(): (Int, Bar) = (1, Bar(2))

  def tuple2: (Int, Double) = (1, 2.0)

  def tuple3(): (Int, Double, Int) = (1, 2.0, 3)

  case class Triple(x: Int, y: Double, z: Int)

  def triple(): Triple = Triple(1, 2.0, 3)


  def arrayExample(): Array[Bar] = {
    Array(Bar(1), Bar(2), Bar(3))
    //      .map(b => b.copy(b.value + 1))
  }

  def arrayIntExample(): Array[Int] = {
    Array(1, 2, 3).map(i => i * 2)
    //      .map(b => b.copy(b.value + 1))
  }

  def tagFoo(i: Int): Int @@ Foo = {
    Tag.apply(i)
  }

  def useFoo(i: Int @@ Foo): Option[String] = {
    Some((5 + Tag.unwrap(i)).toString)
  }

  case class Bar(value: Int) extends AnyVal
  Bar.unapply(Bar(1))

  sealed trait Side
  case object Buy extends Side
  case object Sell extends Side
  def handleOrder(s: Side): Boolean = s match {
    case Buy => true
    case Sell => false
  }

  sealed trait Order
  case class BuyOrder(price: Double) extends Order
  case class SellOrder(price: Double) extends Order
  def handleOrder(o: Order): Boolean = o match {
    case BuyOrder(price) if price > 2.0 => true
    case BuyOrder(_) => false
    case SellOrder(_) => false
  }

  def handleGuard(e: Bar): Option[String] = e match {
    case ee if ee.value > 5 => None
    case Bar(4) => Some("4")
  }

  val z = 4
  def handleInt(e: Int): Option[String] = e match {
    case 2 => Some("res")
    case 3 => None
    case `z` => None
  }

  case class ShareCount(value: Int) extends AnyVal
  def handleAnyVal(e: ShareCount): Option[String] = e match {
    case ShareCount(2) => Some("res")
    case ShareCount(3) => None
    case ShareCount(4) => None
  }

  def processShareCount(sc: ShareCount): Boolean = (sc: @switch) match {
    case ShareCount(1) => true
    case _ => false
  }

}
