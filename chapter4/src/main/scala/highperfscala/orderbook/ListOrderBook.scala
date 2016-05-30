package highperfscala
package orderbook

import orderbook.Commands._
import orderbook.Events._

import scala.collection.immutable.TreeMap

// http://web.archive.org/web/20110312023826/http://www.quantcup.org/home/howtohft_howtobuildafastlimitorderbook
case class ListOrderBook(
  bids: TreeMap[Price, List[BuyLimitOrder]],
  offers: TreeMap[Price, List[SellLimitOrder]]) {
  def bestBid: Option[BuyLimitOrder] = bids.lastOption.flatMap(_._2.headOption)
  def bestOffer: Option[SellLimitOrder] =
    offers.headOption.flatMap(_._2.headOption)
}

object ListOrderBook {
  val noEvent: Option[Event] = None
  val empty: ListOrderBook = ListOrderBook(
    TreeMap.empty[Price, List[BuyLimitOrder]],
    TreeMap.empty[Price, List[SellLimitOrder]])

  // Could make sense to handle with State monad
  def handle(
    currentTime: () => EventInstant,
    ob: ListOrderBook,
    c: Command): (ListOrderBook, Event) = c match {
    case AddLimitOrder(_, o) => handleAddLimitOrder(currentTime, ob, o)
    case CancelOrder(_, id) => handleCancelOrder(currentTime, ob, id)
  }

  private def handleAddLimitOrder(
    currentTime: () => EventInstant,
    ob: ListOrderBook,
    o: LimitOrder): (ListOrderBook, Event) = o match {
    case oo: BuyLimitOrder =>
      ob.bestOffer.exists(oo.price.value >= _.price.value) match {
        case true => crossBookBuy(currentTime, ob, oo)
        case false =>
          val orders = ob.bids.getOrElse(oo.price, Nil)
          ob.copy(bids = ob.bids + (oo.price -> orders.:+(oo))) ->
            LimitOrderAdded(currentTime())
      }
    case oo: SellLimitOrder =>
      ob.bestBid.exists(oo.price.value <= _.price.value) match {
        case true => crossBookSell(currentTime, ob, oo)
        case false =>
          val orders = ob.offers.getOrElse(oo.price, Nil)
          ob.copy(offers = ob.offers + (oo.price -> orders.:+(oo))) ->
            LimitOrderAdded(currentTime())
      }
  }

  private def handleCancelOrder(
    currentTime: () => EventInstant,
    ob: ListOrderBook,
    id: OrderId): (ListOrderBook, Event) = {
    ob.bids.find { case (p, q) => q.exists(_.id == id) }.fold(
      ob.offers.find { case (p, q) => q.exists(_.id == id) }
        .fold[(ListOrderBook, Event)](ob ->
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
    ob: ListOrderBook,
    s: SellLimitOrder): (ListOrderBook, Event) =
    ob.bids.lastOption.fold(handleAddLimitOrder(currentTime, ob, s)) {
      case (_, Nil) => sys.error("Cannot execute cross with empty bids")
      case (_, (o :: Nil)) => (ob.copy(bids = ob.bids - o.price),
        OrderExecuted(currentTime(),
          Execution(o.id, o.price), Execution(s.id, o.price)))
      case (_, (o :: qq)) => (ob.copy(bids = ob.bids + (o.price -> qq)),
        OrderExecuted(currentTime(),
          Execution(o.id, o.price), Execution(s.id, o.price)))
    }

  private def crossBookBuy(
    currentTime: () => EventInstant,
    ob: ListOrderBook,
    b: BuyLimitOrder): (ListOrderBook, Event) =
    ob.offers.headOption.fold(handleAddLimitOrder(currentTime, ob, b)) {
      case (_, Nil) => sys.error("Cannot execute cross with empty offers")
      case (_, (o :: Nil)) => (ob.copy(offers = ob.offers - o.price),
        OrderExecuted(currentTime(), Execution(b.id, o.price),
          Execution(o.id, o.price)))
      case (_, (o :: qq)) => (ob.copy(offers = ob.offers + (o.price -> qq)),
        OrderExecuted(currentTime(),
          Execution(b.id, o.price), Execution(o.id, o.price)))
    }
}

