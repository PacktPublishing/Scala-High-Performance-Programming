package highperfscala.free

case class Bid(value: BigDecimal) extends AnyVal
case class Offer(value: BigDecimal) extends AnyVal
case class Ticker(value: String) extends AnyVal
case class BboUpdated(ticker: Ticker, bid: Bid, offer: Offer)