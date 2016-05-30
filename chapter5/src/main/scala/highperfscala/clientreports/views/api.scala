package highperfscala.clientreports.views

import org.joda.time.Instant

sealed trait LastHourPnL
case object LastHourPositive extends LastHourPnL
case object LastHourNegative extends LastHourPnL

sealed trait LastDayPnL
case object LastDayPositive extends LastDayPnL
case object LastDayNegative extends LastDayPnL

sealed trait LastSevenDayPnL
case object LastSevenDayPositive extends LastSevenDayPnL
case object LastSevenDayNegative extends LastSevenDayPnL

case class Ticker(value: String) extends AnyVal

case class TradingPerformanceTrend(
  ticker: Ticker,
  lastHour: LastHourPnL,
  lastDay: LastDayPnL,
  lastSevenDay: LastSevenDayPnL)

case class Price(value: BigDecimal) extends AnyVal
object Price {
  def average(ps: List[Price]): Price = {
    val prices = ps.map(_.value)
    Price(prices.sum / prices.length)
  }
}
case class OrderId(value: Long) extends AnyVal
case class CreatedTimestamp(value: Instant) extends AnyVal
case class ClientId(value: Long) extends AnyVal
sealed trait Order {
  def created: CreatedTimestamp
  def id: OrderId
  def ticker: Ticker
  def price: Price
  def clientId: ClientId
}
case class BuyOrder(
  created: CreatedTimestamp, id: OrderId, ticker: Ticker, price: Price,
  clientId: ClientId) extends Order
case class SellOrder(
  created: CreatedTimestamp, id: OrderId, ticker: Ticker, price: Price,
  clientId: ClientId) extends Order

case class Execution(created: CreatedTimestamp, id: OrderId, price: Price)

case class PnL(value: BigDecimal) extends AnyVal
object PnL {
  val zero: PnL = PnL(BigDecimal(0))
}

sealed trait PeriodPnL
case object PeriodPositive extends PeriodPnL
case object PeriodNegative extends PeriodPnL

case class GenerateTradingPerformanceTrend(
  tickers: List[Ticker], clientId: ClientId)
