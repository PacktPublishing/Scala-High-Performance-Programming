package highperfscala.orderbook

import highperfscala.orderbook.Commands.{AddLimitOrder, CancelOrder}
import highperfscala.orderbook.Events.{OrderCancelRejected, OrderCanceled}
import org.scalacheck.Prop
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class LazyCancelOrderBookCancelingSpec extends Specification with ScalaCheck {

  """Given empty book
    |When cancel order arrives
    |Then OrderCancelRejected
  """.stripMargin ! Prop.forAll(
    OrderId.genOrderId,
    CommandInstant.genCommandInstant,
    EventInstant.genEventInstant) { (id, ci, ei) =>
    LazyCancelOrderBook.handle(
      () => ei, LazyCancelOrderBook.empty, CancelOrder(ci, id))._2 ====
      OrderCancelRejected(ei, id)
  }

  """Given empty book
    |and buy limit order added
    |When cancel order arrives
    |Then OrderCanceled
  """.stripMargin ! Prop.forAll(
    BuyLimitOrder.genBuyLimitOrder,
    CommandInstant.genCommandInstant,
    EventInstant.genEventInstant) { (o, ci, ei) =>
    (LazyCancelOrderBook.handle(
      () => ei, _: LazyCancelOrderBook, AddLimitOrder(ci, o))).andThen {
      case (ob, _) => LazyCancelOrderBook.handle(
        () => ei, ob, CancelOrder(ci, o.id))._2
    }(LazyCancelOrderBook.empty) ==== OrderCanceled(ei, o.id)
  }

  """Given empty book
    |and sell limit order added
    |When cancel order arrives
    |Then OrderCanceled
  """.stripMargin ! Prop.forAll(
    SellLimitOrder.genSellLimitOrder,
    CommandInstant.genCommandInstant,
    EventInstant.genEventInstant) { (o, ci, ei) =>
    (LazyCancelOrderBook.handle(
      () => ei, _: LazyCancelOrderBook, AddLimitOrder(ci, o))).andThen {
      case (ob, _) => LazyCancelOrderBook.handle(
        () => ei, ob, CancelOrder(ci, o.id))._2
    }(LazyCancelOrderBook.empty) ==== OrderCanceled(ei, o.id)
  }

}
