package highperfscala.dataanalysis

import org.joda.time.DateTime

case class TimestampMinutes(value: Int) extends AnyVal {

  def next: TimestampMinutes = TimestampMinutes(value + 1)
}
object TimestampMinutes {
  def fromDateTime(dt: DateTime): TimestampMinutes = TimestampMinutes(
      (dt.withSecondOfMinute(0).withMillisOfSecond(0)
      .getMillis / (1000 * 60)).toInt)
}

case class AskPrice(value: Int) extends AnyVal

case class BidPrice(value: Int) extends AnyVal

case class Execution(time: TimestampMinutes, ask: AskPrice, bid: BidPrice)

case class Midpoint(time: TimestampMinutes, value: Double)
object Midpoint {

  def fromAskAndBid(
    time: TimestampMinutes,
    askPrice: AskPrice,
    bidPrice: BidPrice): Midpoint =
    Midpoint(time, (bidPrice.value + askPrice.value) / 2D)

  def fromExecution(ex: Execution): Midpoint =
    fromAskAndBid(ex.time, ex.ask, ex.bid)

}

case class MinuteRollUp(value: Int) extends AnyVal

case class Return(value: Double) extends AnyVal

object Return {
  def fromMidpoint(start: Midpoint, end: Midpoint): Return =
    Return((end.value - start.value) / start.value * 100)

  implicit val returnOrdering = new Ordering[Return] {
    override def compare(x: Return, y: Return): Int =
      x.value.compare(y.value)
  }
}

