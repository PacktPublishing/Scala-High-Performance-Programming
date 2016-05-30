package highperfscala.free

import highperfscala.free.BboUpdatedPipeline.BboProcessingFailure

import scala.concurrent.duration.Duration
import Thunk._

class TimedStep[R](f: => R, d: Duration) {
  def orElse[L](l: => L): EitherFree[Thunk, L, R] =
    new EitherFree(timed(() => f, () => l, LimitMs(d.toMillis)))
}

class Step[R](f: => R) {
  def within(d: Duration): TimedStep[R] = new TimedStep[R](f, d)
}
object Step {
  def apply[R](f: => R): Step[R] = new Step(f)
}

class TimedStartingStep[R](f: BboUpdated => R, d: Duration) {
  def orElse[L](b: BboUpdated => L): EitherFree[Thunk, L, R] =
    new EitherFree(startProcessing(f, b, LimitMs(d.toMillis)))
}

class StartWith[R](f: BboUpdated => R) {
  def within(d: Duration): TimedStartingStep[R] = new TimedStartingStep[R](f, d)
}
object StartWith {
  def apply[R](f: BboUpdated => R): StartWith[R] = new StartWith(f)
}

object MakeTradingDecision {
  def apply(e: BboUpdated): EitherFree[Thunk, BboProcessingFailure, Option[Either[Bid, Offer]]] =
    new EitherFree(tradingDecision(_.makeTradingDecision(e)))

}
