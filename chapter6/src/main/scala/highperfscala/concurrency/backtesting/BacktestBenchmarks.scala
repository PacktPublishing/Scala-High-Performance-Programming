package highperfscala.concurrency.backtesting

import java.util.concurrent.TimeUnit

import highperfscala.concurrency.backtesting.Backtest.{BacktestPerformanceSummary, DecisionDelayMillis}
import org.joda.time.{DateTime, Interval, MonthDay}
import org.openjdk.jmh.annotations.Mode._
import org.openjdk.jmh.annotations._

import scala.annotation.tailrec

@BenchmarkMode(Array(Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 5, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 30, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, warmups = 1, jvmArgs = Array("-Xms1G", "-Xmx1G"))
class BacktestBenchmarks {

  import BacktestBenchmarks._

  @Benchmark
  def withoutConcurrency(state: BenchmarkState): BacktestPerformanceSummary =
    Backtest.backtestWithoutConcurrency(state.backtestDays, state.decisionDelay)
      .unsafePerformSync

  @Benchmark
  def withBatchedForking(state: BenchmarkState): BacktestPerformanceSummary =
    Backtest.backtestWithBatchedForking(state.backtestDays, state.decisionDelay)
      .unsafePerformSync

  @Benchmark
  def withAllForked(state: BenchmarkState): BacktestPerformanceSummary =
    Backtest.backtestWithAllForked(state.backtestDays, state.decisionDelay)
      .unsafePerformSync
}

object BacktestBenchmarks {

  private def daysWithin(i: Interval): List[MonthDay] = {
    @tailrec
    def recurse(xs: List[MonthDay], current: DateTime): List[MonthDay] =
      current.isAfter(i.getEnd) match {
        case true => xs
        case false => recurse(
          new MonthDay(current.getMonthOfYear, current.getDayOfMonth) :: xs,
          current.plusDays(1))
      }
    recurse(Nil, i.getStart)
  }

  // Constant starting point to avoid differences due to number of days
  // per month
  private val end: DateTime = new DateTime(2016, 1, 1, 0, 0, 0, 0)
  private def trailingMonths(backtestIntervalMonths: Int): Interval =
    new Interval(
      end.minusMonths(backtestIntervalMonths), end)

  @State(Scope.Benchmark)
  class BenchmarkState {
    @Param(Array("1", "10"))
    var decisionDelayMillis: Long = 0
    @Param(Array("1", "12", "24" ))
    var backtestIntervalMonths: Int = 0

    var decisionDelay: DecisionDelayMillis = DecisionDelayMillis(-1)
    var backtestDays: List[MonthDay] = Nil

    @Setup
    def setup(): Unit = {
      decisionDelay = DecisionDelayMillis(decisionDelayMillis)
      backtestDays = daysWithin(trailingMonths(backtestIntervalMonths))
    }
  }
}
