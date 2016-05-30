package highperfscala.features

import highperfscala.orderbook.Events.OrderExecuted
import highperfscala.orderbook.{EventInstant, Execution}

import scalaz.{@@, Tag}

object FeatureGeneration {

  case class MidpointPrice(value: BigDecimal) extends AnyVal
  object MidpointPrice {
    def create(buy: Execution, sell: Execution): MidpointPrice =
      MidpointPrice((buy.price.value + sell.price.value) / BigDecimal(2))
  }
  case class EventSeconds(value: Long) extends AnyVal
  object EventSeconds {
    def create(i: EventInstant): EventSeconds =
      EventSeconds(i.value.getMillis / 1000l)
  }

  sealed trait PerSecond
  def midpointPerSecond(p: MidpointPrice): MidpointPrice @@ PerSecond = Tag(p)

  // Illustrate example with fold to show the principle of monoid

  def foo(xs: List[OrderExecuted]) = {
    val secondlyPrices = xs.map(e => e.i -> MidpointPrice.create(e.buy, e.sell))
      .groupBy(z => EventSeconds.create(z._1))
      // What about missing indices?
      .mapValues(prices => midpointPerSecond(MidpointPrice(
      prices.map(_._2).reduceOption((x, y) =>
        MidpointPrice(x.value + y.value)).getOrElse(MidpointPrice(
        BigDecimal(0))).value / prices.size)))
      .toList.sortWith((x, y) => x._1.value <= y._1.value)
      .map(_._2)

    secondlyPrices.sliding(size = 6)
    secondlyPrices.sliding(size = 11)
    secondlyPrices.sliding(size = 21)


  }

}
