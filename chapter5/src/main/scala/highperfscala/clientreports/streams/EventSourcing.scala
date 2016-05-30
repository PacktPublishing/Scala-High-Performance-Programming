package highperfscala.clientreports.streams

import org.joda.time.{Duration, Instant}

object EventSourcing {

  sealed trait PnlEvent
  case class PnlIncreased(
    created: EventInstant, clientId: ClientId, ticker: Ticker, profit: Pnl
  ) extends PnlEvent
  case class PnlDecreased(
    created: EventInstant, clientId: ClientId, ticker: Ticker, loss: Pnl)
    extends PnlEvent

  case class Pnl(value: BigDecimal) extends AnyVal {
    def isProfit: Boolean = value.signum >= 0
    def +(p: Pnl): Pnl = Pnl(value + p.value)
    def -(p: Pnl): Pnl = Pnl(value - p.value)
  }
  object Pnl {
    def fromBidExecution(bid: Price, buy: Price): Pnl =
      Pnl(bid.value - buy.value)
    def fromOfferExecution(offer: Price, sell: Price): Pnl =
      Pnl(sell.value - offer.value)
    val zero: Pnl = Pnl(BigDecimal(0))
  }

  case class PendingOrder(ticker: Ticker, price: Price, clientId: ClientId)
  case class TradeState(
    pendingBuys: Map[OrderId, PendingOrder],
    pendingSells: Map[OrderId, PendingOrder]) {
    def cancelOrder(id: OrderId): TradeState = copy(
      pendingBuys = pendingBuys - id, pendingSells = pendingSells - id)
    def addPendingBuy(o: PendingOrder, id: OrderId): TradeState =
      copy(pendingBuys = pendingBuys + (id -> o))
    def addPendingSell(o: PendingOrder, id: OrderId): TradeState =
      copy(pendingSells = pendingSells + (id -> o))
  }
  object TradeState {
    val empty: TradeState = TradeState(Map.empty, Map.empty)
  }

  def processPnl(
    s: TradeState,
    e: OrderBookEvent): (TradeState, Option[PnlEvent]) = e match {
    case BuyOrderSubmitted(_, id, t, p, cId) =>
      s.addPendingBuy(PendingOrder(t, p, cId), id) -> None
    case SellOrderSubmitted(_, id, t, p, cId) =>
      s.addPendingSell(PendingOrder(t, p, cId), id) -> None
    case OrderCanceled(_, id) => s.cancelOrder(id) -> None
    case OrderExecuted(ts, id, price) =>
      val (p, o) = (s.pendingBuys.get(id), s.pendingSells.get(id)) match {
        case (Some(order), None) =>
          Pnl.fromBidExecution(order.price, price) -> order
        case (None, Some(order)) =>
          Pnl.fromOfferExecution(price, order.price) -> order
        case error => sys.error(
          s"Unsupported retrieval of ID = $id returned: $error")
      }
      s.cancelOrder(id) -> Some(
        if (p.isProfit) PnlIncreased(ts, o.clientId, o.ticker, p)
        else PnlDecreased(ts, o.clientId, o.ticker, p))
  }

  sealed trait LastHourPnL
  case object LastHourPositive extends LastHourPnL
  case object LastHourNegative extends LastHourPnL

  case class HourInstant(value: Instant) extends AnyVal {
    def isSameHour(h: HourInstant): Boolean =
      h.value.toDateTime.getHourOfDay == value.toDateTime.getHourOfDay
  }
  object HourInstant {
    def create(i: EventInstant): HourInstant =
      HourInstant(i.value.toDateTime.withMillisOfSecond(0)
        .withSecondOfMinute(0).withMinuteOfHour(0).toInstant)
  }
  case class HourlyPnlTrendCalculated(
    start: HourInstant,
    clientId: ClientId,
    ticker: Ticker,
    pnl: LastHourPnL)

  case class HourlyState(
    keyToHourlyPnl: Map[(ClientId, Ticker), (HourInstant, Pnl)])
  object HourlyState {
    val empty: HourlyState = HourlyState(Map.empty)
  }
  def processHourlyPnl(
    s: HourlyState,
    e: PnlEvent): (HourlyState, Option[HourlyPnlTrendCalculated]) = {
    def processChange(
      ts: EventInstant,
      clientId: ClientId,
      ticker: Ticker,
      pnl: Pnl): (HourlyState, Option[HourlyPnlTrendCalculated]) = {
      val (start, p) = s.keyToHourlyPnl.get((clientId, ticker)).fold(
        (HourInstant.create(ts), Pnl.zero))(identity)
      start.isSameHour(HourInstant.create(ts)) match {
        case true => (s.copy(keyToHourlyPnl = s.keyToHourlyPnl +
          ((clientId, ticker) ->(start, p + pnl))), None)
        case false => (s.copy(keyToHourlyPnl =
          s.keyToHourlyPnl + ((clientId, ticker) ->
            (HourInstant.create(ts), Pnl.zero + pnl))),
          Some(HourlyPnlTrendCalculated(start, clientId, ticker,
            p.isProfit match {
              case true => LastHourPositive
              case false => LastHourNegative
            })))
      }
    }

    e match {
      case PnlIncreased(ts, clientId, ticker, pnl) => processChange(
        ts, clientId, ticker, pnl)
      case PnlDecreased(ts, clientId, ticker, pnl) => processChange(
        ts, clientId, ticker, pnl)
    }
  }

  case class PipelineState(tradeState: TradeState, hourlyState: HourlyState)
  object PipelineState {
    val empty: PipelineState = PipelineState(TradeState.empty, HourlyState.empty)
  }

  def pipeline(
    initial: PipelineState,
    f: HourlyPnlTrendCalculated => Unit,
    xs: Stream[OrderBookEvent]): PipelineState = xs.foldLeft(initial) {
    case (PipelineState(ts, hs), e) =>
      val (tss, pnlEvent) = processPnl(ts, e)
      PipelineState(tss,
        pnlEvent.map(processHourlyPnl(hs, _)).fold(hs) {
          case (hss, Some(hourlyEvent)) =>
            f(hourlyEvent)
            hss
          case (hss, None) => hss
        })
  }

  def main(args: Array[String]): Unit = {
    val now = EventInstant(HourInstant.create(EventInstant(
      new Instant())).value)
    val Foo = Ticker("FOO")

    pipeline(PipelineState.empty, println, Stream(
      BuyOrderSubmitted(now, OrderId(1), Foo, Price(21.07), ClientId(1)),
      OrderExecuted(EventInstant(now.value.plus(Duration.standardMinutes(30))),
        OrderId(1), Price(21.00)),
      BuyOrderSubmitted(EventInstant(now.value.plus(
        Duration.standardMinutes(35))),
        OrderId(2), Foo, Price(24.02), ClientId(1)),
      OrderExecuted(EventInstant(now.value.plus(Duration.standardHours(1))),
        OrderId(2), Price(24.02))))
  }
}
