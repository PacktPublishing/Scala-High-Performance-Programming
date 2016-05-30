package highperfscala
package orderbook

import orderbook.Commands._
import orderbook.Events._

import scala.collection.immutable.{Queue, TreeMap}

// http://web.archive.org/web/20110312023826/http://www.quantcup.org/home/howtohft_howtobuildafastlimitorderbook
case class OrderBook(
  bids: TreeMap[Price, Queue[BuyLimitOrder]],
  offers: TreeMap[Price, Queue[SellLimitOrder]]) {
  def bestBid: Option[BuyLimitOrder] = bids.lastOption.flatMap(_._2.headOption)
  def bestOffer: Option[SellLimitOrder] =
    offers.headOption.flatMap(_._2.headOption)
}

object OrderBook {
  val noEvent: Option[Event] = None
  val empty: OrderBook = OrderBook(
    TreeMap.empty[Price, Queue[BuyLimitOrder]],
    TreeMap.empty[Price, Queue[SellLimitOrder]])

  // Could make sense to handle with State monad
  def handle(ob: OrderBook, c: Command): (OrderBook, Event) = c match {
    case AddLimitOrder(o) => handleAddLimitOrder(ob, o)
    case CancelOrder(id) => handleCancelOrder(ob, id)
  }

  private def handleAddLimitOrder(
    ob: OrderBook, o: LimitOrder): (OrderBook, Event) = o match {
    case oo: BuyLimitOrder =>
      ob.bestOffer.exists(oo.price.value >= _.price.value) match {
        case true => crossBookBuy(ob, oo)
        case false =>
          val orders = ob.bids.getOrElse(oo.price, Queue.empty)
          ob.copy(bids = ob.bids + (oo.price -> orders.enqueue(oo))) ->
            LimitOrderAdded
      }
    case oo: SellLimitOrder =>
      ob.bestBid.exists(oo.price.value <= _.price.value) match {
        case true => crossBookSell(ob, oo)
        case false =>
          val orders = ob.offers.getOrElse(oo.price, Queue.empty)
          ob.copy(offers = ob.offers + (oo.price -> orders.enqueue(oo))) ->
            LimitOrderAdded
      }
  }

  private def handleCancelOrder(
    ob: OrderBook, id: OrderId): (OrderBook, Event) = {
    ob.bids.find { case (p, q) => q.exists(_.id == id) }.fold(
      ob.offers.find { case (p, q) => q.exists(_.id == id) }.fold(
        ob -> Event.orderCancelRejected) { case (p, q) =>
        // It's awkward to duplicate the queue remove logic, but the different
        // types for bid vs offer make it difficult to share the code
        val updatedQ = q.filter(_.id != id)
        ob.copy(offers = updatedQ.nonEmpty match {
          case true => ob.offers + (p -> updatedQ)
          case false => ob.offers - p
        }) -> OrderCanceled
      }) { case (p, q) =>
      val updatedQ = q.filter(_.id != id)
      ob.copy(bids = updatedQ.nonEmpty match {
        case true => ob.bids + (p -> updatedQ)
        case false => ob.bids - p
      }) -> OrderCanceled
    }
  }

  private def crossBookSell(
    ob: OrderBook, s: SellLimitOrder): (OrderBook, Event) =
    ob.bids.lastOption.fold(handleAddLimitOrder(ob, s)) { case (_, xs) =>
      val (o, qq) = xs.dequeue
      (ob.copy(bids = qq.isEmpty match {
        case true => ob.bids - o.price
        case false => ob.bids + (o.price -> qq)
      }), OrderExecuted(Execution(o.id, o.price), Execution(s.id, o.price)))
    }

  private def crossBookBuy(
    ob: OrderBook, b: BuyLimitOrder): (OrderBook, Event) =
    ob.offers.headOption.fold(handleAddLimitOrder(ob, b)) { case (_, xs) =>
      val (o, qq) = xs.dequeue
      (ob.copy(offers = qq.isEmpty match {
        case true => ob.offers - o.price
        case false => ob.offers + (o.price -> qq)
      }), OrderExecuted(Execution(b.id, o.price), Execution(o.id, o.price)))
    }
}

object Commands {
  sealed trait Command
  case class AddLimitOrder(o: LimitOrder) extends Command
  case class CancelOrder(id: OrderId) extends Command
}

object Events {
  sealed trait Event
  object Event {
    val orderCancelRejected: Event = OrderCancelRejected
  }
  case class OrderExecuted(buy: Execution, sell: Execution) extends Event
  case object LimitOrderAdded extends Event
  case object OrderCancelRejected extends Event
  case object OrderCanceled extends Event
}
