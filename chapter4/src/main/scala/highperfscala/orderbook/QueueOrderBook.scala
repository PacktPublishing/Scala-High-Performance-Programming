package highperfscala
package orderbook

import scala.collection.immutable.{TreeMap, Queue}
import orderbook.Commands._
import orderbook.Events._

// Taken originally from Chapter 2
// http://web.archive.org/web/20110312023826/http://www.quantcup.org/home/howtohft_howtobuildafastlimitorderbook
case class QueueOrderBook(
  bids: TreeMap[Price, Queue[BuyLimitOrder]],
  offers: TreeMap[Price, Queue[SellLimitOrder]]) {
  def bestBid: Option[BuyLimitOrder] = bids.lastOption.flatMap(_._2.headOption)
  def bestOffer: Option[SellLimitOrder] =
    offers.headOption.flatMap(_._2.headOption)
}

object QueueOrderBook {
  val noEvent: Option[Event] = None
  val empty: QueueOrderBook = QueueOrderBook(
    TreeMap.empty[Price, Queue[BuyLimitOrder]],
    TreeMap.empty[Price, Queue[SellLimitOrder]])

  // Could make sense to handle with State monad
  def handle(
    currentTime: () => EventInstant,
    ob: QueueOrderBook,
    c: Command): (QueueOrderBook, Event) = c match {
    case AddLimitOrder(_, o) => handleAddLimitOrder(currentTime, ob, o)
    case CancelOrder(_, id) => handleCancelOrder(currentTime, ob, id)
  }

  private def handleAddLimitOrder(
    currentTime: () => EventInstant,
    ob: QueueOrderBook,
    o: LimitOrder): (QueueOrderBook, Event) = o match {
    case oo: BuyLimitOrder =>
      ob.bestOffer.exists(oo.price.value >= _.price.value) match {
        case true => crossBookBuy(currentTime, ob, oo)
        case false =>
          val orders = ob.bids.getOrElse(oo.price, Queue.empty)
          ob.copy(bids = ob.bids + (oo.price -> orders.enqueue(oo))) ->
            LimitOrderAdded(currentTime())
      }
    case oo: SellLimitOrder =>
      ob.bestBid.exists(oo.price.value <= _.price.value) match {
        case true => crossBookSell(currentTime, ob, oo)
        case false =>
          val orders = ob.offers.getOrElse(oo.price, Queue.empty)
          ob.copy(offers = ob.offers + (oo.price -> orders.enqueue(oo))) ->
            LimitOrderAdded(currentTime())
      }
  }

  private def handleCancelOrder(
    currentTime: () => EventInstant,
    ob: QueueOrderBook,
    id: OrderId): (QueueOrderBook, Event) = {
    ob.bids.find { case (p, q) => q.exists(_.id == id) }.fold(
      ob.offers.find { case (p, q) => q.exists(_.id == id) }
        .fold[(QueueOrderBook, Event)](ob ->
        OrderCancelRejected(currentTime(), id)) {
        case (p, q) =>
          // It's awkward to duplicate the queue remove logic, but the different
          // types for bid vs offer make it difficult to share the code
          val updatedQ = q.filter(_.id != id)
          ob.copy(offers = updatedQ.nonEmpty match {
            case true => ob.offers + (p -> updatedQ)
            case false => ob.offers - p
          }) -> OrderCanceled(currentTime(), id)
      }) { case (p, q) =>
      val updatedQ = q.filter(_.id != id)
      ob.copy(bids = updatedQ.nonEmpty match {
        case true => ob.bids + (p -> updatedQ)
        case false => ob.bids - p
      }) -> OrderCanceled(currentTime(), id)
    }
  }

  private def crossBookSell(
    currentTime: () => EventInstant,
    ob: QueueOrderBook, s: SellLimitOrder): (QueueOrderBook, Event) =
    ob.bids.lastOption.fold(handleAddLimitOrder(currentTime, ob, s)) {
      case (_, xs) =>
        val (o, qq) = xs.dequeue
        (ob.copy(bids = qq.isEmpty match {
          case true => ob.bids - o.price
          case false => ob.bids + (o.price -> qq)
        }), OrderExecuted(currentTime(), Execution(o.id, o.price), Execution(s.id, o.price)))
    }

  private def crossBookBuy(
    currentTime: () => EventInstant,
    ob: QueueOrderBook, b: BuyLimitOrder): (QueueOrderBook, Event) =
    ob.offers.headOption.fold(handleAddLimitOrder(currentTime, ob, b)) {
      case (_, xs) =>
        val (o, qq) = xs.dequeue
        (ob.copy(offers = qq.isEmpty match {
          case true => ob.offers - o.price
          case false => ob.offers + (o.price -> qq)
        }), OrderExecuted(
          currentTime(), Execution(b.id, o.price), Execution(o.id, o.price)))
    }
}
