package highperfscala.orderbook

import highperfscala.orderbook.Commands.{AddLimitOrder, CancelOrder}
import highperfscala.orderbook.Events.{LimitOrderAdded, OrderCancelRejected, OrderExecuted}
import org.scalacheck.Prop
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class QueueOrderBookTradingSpec extends Specification with ScalaCheck {

  """Given empty book
    |When limit order arrives
    |Then LimitOrderAdded
  """.stripMargin ! Prop.forAll(
    LimitOrder.genLimitOrder,
    CommandInstant.genCommandInstant,
    EventInstant.genEventInstant) { (l, ci, ei) =>
     QueueOrderBook.handle(
      () => ei,  QueueOrderBook.empty, AddLimitOrder(ci, l))._2 ====
      LimitOrderAdded(ei)
  }

  """Given empty book
    |and buy limit order added
    |When resting sell limit order arrives
    |Then LimitOrderAdded
  """.stripMargin ! Prop.forAll(
    BuyLimitOrder.genBuyLimitOrder,
    OrderId.genOrderId,
    CommandInstant.genCommandInstant,
    EventInstant.genEventInstant) { (buy, id, ci, ei) =>
    ( QueueOrderBook.handle(
      () => ei, _:  QueueOrderBook, AddLimitOrder(ci, buy))).andThen {
      case (ob, _) =>  QueueOrderBook.handle(() => ei, ob, AddLimitOrder(ci,
        SellLimitOrder(id, Price(buy.price.value + 0.01))))._2
    }( QueueOrderBook.empty) ==== LimitOrderAdded(ei)
  }

  """Given empty book
    |and sell limit order added
    |When resting buy limit order arrives
    |Then LimitOrderAdded
  """.stripMargin ! Prop.forAll(
    SellLimitOrder.genSellLimitOrder,
    OrderId.genOrderId,
    CommandInstant.genCommandInstant,
    EventInstant.genEventInstant) { (sell, id, ci, ei) =>
    ( QueueOrderBook.handle(() => ei, _:  QueueOrderBook, AddLimitOrder(ci, sell)))
      .andThen {
        case (ob, _) =>  QueueOrderBook.handle(() => ei, ob, AddLimitOrder(ci,
          BuyLimitOrder(id, Price(sell.price.value - 0.01))))._2
      }( QueueOrderBook.empty) ==== LimitOrderAdded(ei)
  }

  """Given empty book
    |and buy limit order added
    |When crossing sell limit order arrives
    |Then OrderExecuted
  """.stripMargin ! Prop.forAll(
    BuyLimitOrder.genBuyLimitOrder,
    OrderId.genOrderId,
    CommandInstant.genCommandInstant,
    EventInstant.genEventInstant) { (buy, id, ci, ei) =>
    ( QueueOrderBook.handle(() => ei, _:  QueueOrderBook, AddLimitOrder(ci, buy)))
      .andThen {
        case (ob, _) =>  QueueOrderBook.handle(() => ei, ob, AddLimitOrder(ci,
          SellLimitOrder(id, Price(buy.price.value - 0.01))))._2
      }( QueueOrderBook.empty) ==== OrderExecuted(ei,
      Execution(buy.id, buy.price), Execution(id, buy.price))
  }

  """Given empty book
    |and sell limit order added
    |When crossing buy limit order arrives
    |Then OrderExecuted
  """.stripMargin ! Prop.forAll(
    SellLimitOrder.genSellLimitOrder,
    OrderId.genOrderId,
    CommandInstant.genCommandInstant,
    EventInstant.genEventInstant) { (sell, id, ci, ei) =>
    ( QueueOrderBook.handle(() => ei, _:  QueueOrderBook, AddLimitOrder(ci, sell)))
      .andThen {
        case (ob, _) =>  QueueOrderBook.handle(() => ei, ob, AddLimitOrder(ci,
          BuyLimitOrder(id, Price(sell.price.value + 0.01))))._2
      }( QueueOrderBook.empty) ==== OrderExecuted(ei,
      Execution(id, sell.price), Execution(sell.id, sell.price))
  }

  """Given empty book
    |When cancel order command arrives
    |Then OrderCancelRejected
  """.stripMargin ! Prop.forAll(
    OrderId.genOrderId,
    CommandInstant.genCommandInstant,
    EventInstant.genEventInstant) { (id, ci, ei) =>
     QueueOrderBook.handle(() => ei,  QueueOrderBook.empty, CancelOrder(ci, id))
      ._2 ==== OrderCancelRejected(ei, id)
  }
}
