package highperfscala.clientreports.views

import org.joda.time.{Duration, Instant, Interval}

object PerformanceReporting {

  def trend(
    now: () => Instant,
    findOrders: (Interval, Ticker) => List[Order],
    findExecutions: (Interval, Ticker) => List[Execution],
    request: GenerateTradingPerformanceTrend): List[TradingPerformanceTrend] = {
    def periodPnL(
      duration: Duration): Map[Ticker, PeriodPnL] = {
      val currentTime = now()
      val interval = new Interval(currentTime.minus(duration), currentTime)
      (for {
        ticker <- request.tickers
        orders = findOrders(interval, ticker)
        executions = findExecutions(interval, ticker)
        idToExecPrice = executions.groupBy(_.id).mapValues(es =>
          Price.average(es.map(_.price)))
        signedExecutionPrices = for {
          o <- orders
          if o.clientId == request.clientId
          price <- idToExecPrice.get(o.id).map(p => o match {
            case _: BuyOrder => Price(p.value * -1)
            case _: SellOrder => p
          }).toList
        } yield price
        trend = signedExecutionPrices.foldLeft(PnL.zero) {
          case (pnl, p) => PnL(pnl.value + p.value)
        } match {
          case p if p.value >= PnL.zero.value => PeriodPositive
          case _ => PeriodNegative
        }
      } yield ticker -> trend).toMap
    }

    val tickerToLastHour = periodPnL(Duration.standardHours(1)).mapValues {
      case PeriodPositive => LastHourPositive
      case PeriodNegative => LastHourNegative
    }
    val tickerToLastDay = periodPnL(Duration.standardDays(1)).mapValues {
      case PeriodPositive => LastDayPositive
      case PeriodNegative => LastDayNegative
    }
    val tickerToLastSevenDays = periodPnL(Duration.standardDays(7)).mapValues {
      case PeriodPositive => LastSevenDayPositive
      case PeriodNegative => LastSevenDayNegative
    }

    tickerToLastHour.zip(tickerToLastDay).zip(tickerToLastSevenDays).map({
      case (((t, lastHour), (_, lastDay)), (_, lastSevenDays)) =>
        TradingPerformanceTrend(t, lastHour, lastDay, lastSevenDays)
    }).toList
  }
}
