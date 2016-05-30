package highperfscala.concurrency.backtesting

import java.util.concurrent.TimeUnit

import org.joda.time.{DateTime, Interval}

import scala.concurrent.{Await, Future}

object Backtesting {

  sealed trait Strategy

  case class PnL(value: BigDecimal) extends AnyVal
  case class BacktestPerformanceSummary(pnl: PnL)

  case class Ticker(value: String) extends AnyVal

  def backtest(
    strategy: Strategy,
    ticker: Ticker,
    testInterval: Interval): BacktestPerformanceSummary = ???

  sealed trait VectorBasedReturnSeriesFrame

  def loadReturns(testInterval: Interval): VectorBasedReturnSeriesFrame = ???

  case object Dave1 extends Strategy
  case object Dave2 extends Strategy

  object Serial {
    def lastMonths(months: Int): Interval =
      new Interval(new DateTime().minusMonths(months), new DateTime())
    backtest(Dave1, Ticker("AAPL"), lastMonths(3))
    backtest(Dave1, Ticker("GOOG"), lastMonths(3))
    backtest(Dave2, Ticker("AAPL"), lastMonths(3))
    backtest(Dave2, Ticker("GOOG"), lastMonths(2))
  }

  object ForComprehension {
    def lastMonths(months: Int): Interval =
      new Interval(new DateTime().minusMonths(months), new DateTime())

    implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
    val summariesF = for {
      firstDaveAapl <- Future(backtest(Dave1, Ticker("AAPL"), lastMonths(3)))
      firstDaveGoog <- Future(backtest(Dave1, Ticker("GOOG"), lastMonths(3)))
      secondDaveAapl <- Future(backtest(Dave2, Ticker("AAPL"), lastMonths(3)))
      secondDaveGoog <- Future(backtest(Dave2, Ticker("GOOG"), lastMonths(2)))
    } yield (firstDaveAapl, firstDaveGoog, secondDaveAapl, secondDaveGoog)

    Await.result(summariesF, scala.concurrent.duration.Duration(1, TimeUnit.SECONDS))

    Future(1).flatMap(f1 => Future(2).flatMap(f2 => Future(3).map(f3 => (f1, f2, f3))))
  }

  object Concurrency {
    def lastMonths(months: Int): Interval =
      new Interval(new DateTime().minusMonths(months), new DateTime())

    implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
    val firstDaveAaplF = Future(backtest(Dave1, Ticker("AAPL"), lastMonths(3)))
    val firstDaveGoogF = Future(backtest(Dave1, Ticker("GOOG"), lastMonths(3)))
    val secondDaveAaplF = Future(backtest(Dave2, Ticker("AAPL"), lastMonths(3)))
    val secondDaveGoogF = Future(backtest(Dave2, Ticker("GOOG"), lastMonths(2)))
    val z = for {
      firstDaveAapl <- firstDaveAaplF
      firstDaveGoog <- firstDaveGoogF
      secondDaveAapl <- secondDaveAaplF
      secondDaveGoog <- secondDaveGoogF
    } yield (firstDaveAapl, firstDaveGoog, secondDaveAapl, secondDaveGoog)

  }

}
