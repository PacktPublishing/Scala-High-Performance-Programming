package highperfscala
package orderbook

import highperfscala.orderbook.Commands._
import highperfscala.orderbook.Events._

import scala.annotation.tailrec
import scala.collection.immutable.{Queue, TreeMap}

// http://web.archive.org/web/20110312023826/http://www.quantcup.org/home/howtohft_howtobuildafastlimitorderbook
case class LazyCancelOrderBook(
  activeIds: Set[OrderId],
  pendingCancelIds: Set[OrderId],
  bids: TreeMap[Price, Queue[BuyLimitOrder]],
  offers: TreeMap[Price, Queue[SellLimitOrder]]) {
  def bestBid: Option[BuyLimitOrder] = bids.lastOption.flatMap(_._2.headOption)
  def bestOffer: Option[SellLimitOrder] =
    offers.headOption.flatMap(_._2.headOption)
}

object LazyCancelOrderBook {
  val noEvent: Option[Event] = None
  val empty: LazyCancelOrderBook = LazyCancelOrderBook(
    Set.empty,
    Set.empty,
    TreeMap.empty[Price, Queue[BuyLimitOrder]],
    TreeMap.empty[Price, Queue[SellLimitOrder]])

  // Could make sense to handle with State monad
  def handle(
    currentTime: () => EventInstant,
    ob: LazyCancelOrderBook,
    c: Command): (LazyCancelOrderBook, Event) = c match {
    case AddLimitOrder(_, o) => handleAddLimitOrder(currentTime, ob, o)
    case CancelOrder(_, id) => handleCancelOrder(currentTime, ob, id)
  }

  // Am cheating by adding order to book if current level does not have active
  // order
  private def handleAddLimitOrder(
    currentTime: () => EventInstant,
    ob: LazyCancelOrderBook,
    lo: LimitOrder): (LazyCancelOrderBook, Event) = lo match {
    case b: BuyLimitOrder =>
      @tailrec
      def findActiveOrder(
        q: Queue[SellLimitOrder],
        idsToRemove: Set[OrderId]): (Option[SellLimitOrder], Option[Queue[SellLimitOrder]], Set[OrderId]) =
        q.dequeueOption match {
          case Some((o, qq)) => ob.pendingCancelIds.contains(o.id) match {
            case true =>
              findActiveOrder(qq, idsToRemove + o.id)
            case false =>
              (Some(o), if (qq.nonEmpty) Some(qq) else None, idsToRemove + o.id)
          }
          case None => (None, None, idsToRemove)
        }

      def restLimitOrder: (LazyCancelOrderBook, Event) = {
        val orders = ob.bids.getOrElse(b.price, Queue.empty)
        ob.copy(bids = ob.bids + (b.price -> orders.enqueue(b)),
          activeIds = ob.activeIds + b.id) -> LimitOrderAdded(currentTime())
      }

      ob.bestOffer.exists(_.price.value <= b.price.value) match {
        case true => ob.offers.headOption.fold(restLimitOrder) {
          case (p, q) => findActiveOrder(q, Set.empty) match {
            case (Some(o), Some(qq), rms) => (ob.copy(
              offers = ob.offers + (o.price -> qq),
              activeIds = ob.activeIds -- rms), OrderExecuted(currentTime(),
              Execution(b.id, o.price), Execution(o.id, o.price)))
            case (Some(o), None, rms) => (ob.copy(
              offers = ob.offers - o.price, activeIds = ob.activeIds -- rms),
              OrderExecuted(currentTime(),
                Execution(b.id, o.price), Execution(o.id, o.price)))
            case (None, _, rms) =>
              val bs = ob.bids.getOrElse(b.price, Queue.empty).enqueue(b)
              (ob.copy(bids = ob.bids + (b.price -> bs),
                offers = ob.offers - p,
                activeIds = ob.activeIds -- rms + b.id),
                LimitOrderAdded(currentTime()))
          }
        }
        case false => restLimitOrder
      }

    case s: SellLimitOrder =>
      @tailrec
      def findActiveOrder(
        q: Queue[BuyLimitOrder],
        idsToRemove: Set[OrderId]): (Option[BuyLimitOrder], Option[Queue[BuyLimitOrder]], Set[OrderId]) =
        q.dequeueOption match {
          case Some((o, qq)) => ob.pendingCancelIds.contains(o.id) match {
            case true =>
              findActiveOrder(qq, idsToRemove + o.id)
            case false =>
              (Some(o), if (qq.nonEmpty) Some(qq) else None, idsToRemove + o.id)
          }
          case None => (None, None, idsToRemove)
        }

      def restLimitOrder: (LazyCancelOrderBook, Event) = {
        val orders = ob.offers.getOrElse(s.price, Queue.empty)
        ob.copy(offers = ob.offers + (s.price -> orders.enqueue(s)),
          activeIds = ob.activeIds + s.id) ->
          LimitOrderAdded(currentTime())
      }

      ob.bestBid.exists(_.price.value >= s.price.value) match {
        case true => ob.bids.headOption.fold(restLimitOrder) {
          case (p, q) => findActiveOrder(q, Set.empty) match {
            case (Some(o), Some(qq), rms) => (ob.copy(
              bids = ob.bids + (o.price -> qq), activeIds = ob.activeIds -- rms),
              OrderExecuted(currentTime(),
                Execution(o.id, o.price), Execution(s.id, o.price)))

            case (Some(o), None, rms) => (ob.copy(
              bids = ob.bids - o.price, activeIds = ob.activeIds -- rms),
              OrderExecuted(currentTime(),
                Execution(o.id, o.price), Execution(s.id, o.price)))

            // If no order found, implies that the queue is now empty
            case (None, _, rms) =>
              val os = ob.offers.getOrElse(s.price, Queue.empty).enqueue(s)
              (ob.copy(offers = ob.offers + (s.price -> os),
                bids = ob.bids - p, activeIds = ob.activeIds -- rms + s.id),
                LimitOrderAdded(currentTime()))
          }
        }
        case false => restLimitOrder
      }
  }

  private def handleCancelOrder(
    currentTime: () => EventInstant,
    ob: LazyCancelOrderBook,
    id: OrderId): (LazyCancelOrderBook, Event) =
    ob.activeIds.contains(id) match {
      case true => ob.copy(activeIds = ob.activeIds - id,
        pendingCancelIds = ob.pendingCancelIds + id) ->
        OrderCanceled(currentTime(), id)
      case false => ob -> OrderCancelRejected(currentTime(), id)
    }
}


