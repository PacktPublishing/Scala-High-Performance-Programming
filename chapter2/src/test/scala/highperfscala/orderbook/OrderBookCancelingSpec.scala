package highperfscala
package orderbook

import highperfscala.orderbook.Commands.{AddLimitOrder, CancelOrder}
import highperfscala.orderbook.Events.{OrderCanceled, OrderCancelRejected}
import org.scalacheck.Prop
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class OrderBookCancelingSpec extends Specification with ScalaCheck {

  """Given empty book
    |When cancel order arrives
    |Then OrderCancelRejected
  """.stripMargin ! Prop.forAll(OrderId.genOrderId) { id =>
    OrderBook.handle(OrderBook.empty, CancelOrder(id))._2 ====
      OrderCancelRejected
  }

  """Given empty book
    |and buy limit order added
    |When cancel order arrives
    |Then OrderCanceled
  """.stripMargin ! Prop.forAll(BuyLimitOrder.genBuyLimitOrder) { o =>
    (OrderBook.handle(_: OrderBook, AddLimitOrder(o))).andThen {
      case (ob, _) => OrderBook.handle(ob, CancelOrder(o.id))._2
    }(OrderBook.empty) ==== OrderCanceled
  }

  """Given empty book
    |and sell limit order added
    |When cancel order arrives
    |Then OrderCanceled
  """.stripMargin ! Prop.forAll(SellLimitOrder.genSellLimitOrder) { o =>
    (OrderBook.handle(_: OrderBook, AddLimitOrder(o))).andThen {
      case (ob, _) => OrderBook.handle(ob, CancelOrder(o.id))._2
    }(OrderBook.empty) ==== OrderCanceled
  }

}
