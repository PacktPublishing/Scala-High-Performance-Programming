package highperfscala.orderbook

import highperfscala.orderbook.Commands.{CancelOrder, AddLimitOrder}
import highperfscala.orderbook.Events.{OrderCancelRejected, OrderExecuted, LimitOrderAdded}
import org.scalacheck.Prop
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class OrderBookTradingSpec extends Specification with ScalaCheck {

  """Given empty book
    |When limit order arrives
    |Then LimitOrderAdded
  """.stripMargin ! Prop.forAll(LimitOrder.genLimitOrder) { l =>
    OrderBook.handle(OrderBook.empty, AddLimitOrder(l))._2 ==== LimitOrderAdded
  }

  """Given empty book
    |and buy limit order added
    |When resting sell limit order arrives
    |Then LimitOrderAdded
  """.stripMargin ! Prop.forAll(BuyLimitOrder.genBuyLimitOrder,
    OrderId.genOrderId) { (buy, id) =>
    (OrderBook.handle(_: OrderBook, AddLimitOrder(buy))).andThen {
      case (ob, _) => OrderBook.handle(ob, AddLimitOrder(
        SellLimitOrder(id, Price(buy.price.value + 0.01))))._2
    }(OrderBook.empty) ==== LimitOrderAdded
  }

  """Given empty book
    |and sell limit order added
    |When resting buy limit order arrives
    |Then LimitOrderAdded
  """.stripMargin ! Prop.forAll(SellLimitOrder.genSellLimitOrder,
    OrderId.genOrderId) { (sell, id) =>
    (OrderBook.handle(_: OrderBook, AddLimitOrder(sell))).andThen {
      case (ob, _) => OrderBook.handle(ob, AddLimitOrder(
        BuyLimitOrder(id, Price(sell.price.value - 0.01))))._2
    }(OrderBook.empty) ==== LimitOrderAdded
  }

  """Given empty book
    |and buy limit order added
    |When crossing sell limit order arrives
    |Then OrderExecuted
  """.stripMargin ! Prop.forAll(BuyLimitOrder.genBuyLimitOrder,
    OrderId.genOrderId) { (buy, id) =>
    (OrderBook.handle(_: OrderBook, AddLimitOrder(buy))).andThen {
      case (ob, _) => OrderBook.handle(ob, AddLimitOrder(
        SellLimitOrder(id, Price(buy.price.value - 0.01))))._2
    }(OrderBook.empty) ==== OrderExecuted(
      Execution(buy.id, buy.price), Execution(id, buy.price))
  }

  """Given empty book
    |and sell limit order added
    |When crossing buy limit order arrives
    |Then OrderExecuted
  """.stripMargin ! Prop.forAll(SellLimitOrder.genSellLimitOrder,
    OrderId.genOrderId) { (sell, id) =>
    (OrderBook.handle(_: OrderBook, AddLimitOrder(sell))).andThen {
      case (ob, _) => OrderBook.handle(ob, AddLimitOrder(
        BuyLimitOrder(id, Price(sell.price.value + 0.01))))._2
    }(OrderBook.empty) ==== OrderExecuted(
      Execution(id, sell.price), Execution(sell.id, sell.price))
  }

  """Given empty book
    |When cancel order command arrives
    |Then OrderCancelRejected
  """.stripMargin ! Prop.forAll(OrderId.genOrderId) { id =>
    OrderBook.handle(OrderBook.empty, CancelOrder(id))._2 ====
      OrderCancelRejected
  }
}
