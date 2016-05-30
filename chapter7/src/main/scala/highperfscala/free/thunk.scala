package highperfscala.free

import scala.language.{higherKinds, implicitConversions, postfixOps}
import scalaz.{-\/, Free, Functor, \/, \/-}

case class LimitMs(value: Long) extends AnyVal

sealed trait Thunk[A]
case class Timed[A](
  whenActive: () => A,
  whenExpired: () => A,
  limit: LimitMs) extends Thunk[A]
case class StartProcessing[A](
  whenActive: BboUpdated => A,
  whenExpired: BboUpdated => A,
  limit: LimitMs) extends Thunk[A]
case class TradingDecision[A](
  makeDecision: TradingStrategy => A) extends Thunk[A]

object Thunk {
  implicit val functor: Functor[Thunk] = new Functor[Thunk] {
    def map[A, B](t: Thunk[A])(f: (A) => B): Thunk[B] = t match {
      case Timed(whenActive, whenExpired, limit) =>
        Timed(() => f(whenActive()), () => f(whenExpired()), limit)
      case StartProcessing(whenActive, whenExpired, limit) =>
        StartProcessing(c => f(whenActive(c)), c => f(whenExpired(c)), limit)
      case TradingDecision(makeDecision) => TradingDecision(
        (makeDecision.apply _).andThen(f))
    }
  }

  def timed[L, R](
    f: () => R,
    exp: () => L,
    limit: LimitMs): Free[Thunk, L \/ R] = Free.liftF(
    Timed(() => \/-(f()), () => -\/(exp()), limit))
  def startProcessing[L, R](
    f: BboUpdated => R,
    exp: BboUpdated => L,
    limit: LimitMs): Free[Thunk, L \/ R] =
    Free.liftF(StartProcessing(f.andThen(\/-(_)), exp.andThen(-\/(_)), limit))
  def tradingDecision[L, R](f: TradingStrategy => R): Free[Thunk, L \/ R] =
    Free.liftF(TradingDecision((f.apply _).andThen(\/-(_))))
}
