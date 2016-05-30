package highperfscala.concurrency.backtesting

import org.joda.time.MonthDay

import scalaz.concurrent.Task

object Backtest {

  case class PnL(value: BigDecimal) extends AnyVal
  object PnL {
    def merge(x: PnL, y: PnL): PnL = PnL(x.value + y.value)
    val zero: PnL = PnL(0)
  }
  case class BacktestPerformanceSummary(pnl: PnL)
  case class DecisionDelayMillis(value: Long) extends AnyVal

  def originalBacktest(
    testDays: List[MonthDay],
    decisionDelay: DecisionDelayMillis): BacktestPerformanceSummary = {
    val pnls = for {
      d <- testDays
      _ = Thread.sleep(decisionDelay.value)
    } yield PnL(10)
    BacktestPerformanceSummary(pnls.reduceOption(PnL.merge).getOrElse(PnL.zero))
  }

  def backtestWithoutConcurrency(
    testDays: List[MonthDay],
    decisionDelay: DecisionDelayMillis): Task[BacktestPerformanceSummary] = {
    val ts = for (d <- testDays) yield Task.delay {
      Thread.sleep(decisionDelay.value)
      PnL(10)
    }
    Task.gatherUnordered(ts).map(pnls => BacktestPerformanceSummary(
      pnls.reduceOption(PnL.merge).getOrElse(PnL.zero)))
  }

  def backtestWithAllForked(
    testDays: List[MonthDay],
    decisionDelay: DecisionDelayMillis): Task[BacktestPerformanceSummary] = {
    val ts = for (d <- testDays) yield Task.fork {
      Thread.sleep(decisionDelay.value)
      Task.now(PnL(10))
    }
    Task.gatherUnordered(ts).map(pnls => BacktestPerformanceSummary(
      pnls.reduceOption(PnL.merge).getOrElse(PnL.zero)))
  }

  def backtestWithBatchedForking(
    testDays: List[MonthDay],
    decisionDelay: DecisionDelayMillis): Task[BacktestPerformanceSummary] = {
    val ts = for (d <- testDays) yield Task.delay {
      Thread.sleep(decisionDelay.value)
      PnL(10)
    }
    Task.gatherUnordered(ts.sliding(30, 30).toList.map(xs =>
      Task.fork(Task.gatherUnordered(xs)))).map(pnls =>
      BacktestPerformanceSummary(
        pnls.flatten.reduceOption(PnL.merge).getOrElse(PnL.zero)))
  }
}
