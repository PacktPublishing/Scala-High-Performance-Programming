package highperfscala
package orderbook

import org.scalacheck.Gen

case class Price(value: BigDecimal)
object Price {
  implicit val genPrice: Gen[Price] = Gen.posNum[Double].map(d =>
    Price(BigDecimal(d)))
  implicit val ordering: Ordering[Price] = new Ordering[Price] {
    def compare(x: Price, y: Price): Int =
      Ordering.BigDecimal.compare(x.value, y.value)
  }
}

case class OrderId(value: Long)
object OrderId {
  implicit val genOrderId: Gen[OrderId] = Gen.posNum[Long].map(OrderId.apply)
}

sealed trait LimitOrder {
  def id: OrderId
  def price: Price
}
object LimitOrder {
  implicit val genLimitOrder: Gen[LimitOrder] = Gen.oneOf(
    BuyLimitOrder.genBuyLimitOrder, SellLimitOrder.genSellLimitOrder)
}
case class BuyLimitOrder(id: OrderId, price: Price) extends LimitOrder
object BuyLimitOrder {
  implicit val genBuyLimitOrder: Gen[BuyLimitOrder] = Gen.zip(
    OrderId.genOrderId, Price.genPrice).map(Function.tupled(BuyLimitOrder.apply))
}
case class SellLimitOrder(id: OrderId, price: Price) extends LimitOrder
object SellLimitOrder {
  implicit val genSellLimitOrder: Gen[SellLimitOrder] = Gen.zip(
    OrderId.genOrderId, Price.genPrice).map(Function.tupled(
    SellLimitOrder.apply))
}

case class Execution(orderId: OrderId, price: Price)