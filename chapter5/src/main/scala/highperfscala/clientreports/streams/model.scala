package highperfscala.clientreports.streams

import org.joda.time.Instant

case class Ticker(value: String) extends AnyVal
case class Price(value: BigDecimal) extends AnyVal
case class OrderId(value: Long) extends AnyVal
case class EventInstant(value: Instant) extends AnyVal
case class ClientId(value: Long) extends AnyVal

sealed trait Order {
  def created: EventInstant
  def id: OrderId
  def ticker: Ticker
  def price: Price
  def clientId: ClientId
}
case class BuyOrder(
  created: EventInstant, id: OrderId, ticker: Ticker, price: Price,
  clientId: ClientId) extends Order
case class SellOrder(
  created: EventInstant, id: OrderId, ticker: Ticker, price: Price,
  clientId: ClientId) extends Order

case class Execution(created: EventInstant, id: OrderId, price: Price)


sealed trait OrderBookEvent
case class BuyOrderSubmitted(
  created: EventInstant, id: OrderId, ticker: Ticker, price: Price,
  clientId: ClientId) extends OrderBookEvent
case class SellOrderSubmitted(
  created: EventInstant, id: OrderId, ticker: Ticker, price: Price,
  clientId: ClientId) extends OrderBookEvent
case class OrderCanceled(created: EventInstant, id: OrderId)
  extends OrderBookEvent
case class OrderExecuted(created: EventInstant, id: OrderId, price: Price)
  extends OrderBookEvent