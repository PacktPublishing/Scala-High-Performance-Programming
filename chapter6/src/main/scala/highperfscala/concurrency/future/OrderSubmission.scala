package highperfscala.concurrency.future

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object OrderSubmission {

  trait RawOrder
  trait OrderSubmitted

  trait ValidatedOrder
  object ValidatedOrder {
    def fromRawOrder(o: RawOrder): Option[ValidatedOrder] = None
  }

  trait AccountPositions

  def submitOrder(
    ec: ExecutionContext,
    sendToExchange: ValidatedOrder => Future[OrderSubmitted],
    updatePositions: OrderSubmitted => Future[AccountPositions],
    o: RawOrder): Unit = {
    implicit val iec = ec

    (for {
      vo <- ValidatedOrder.fromRawOrder(o).fold(Future.failed[ValidatedOrder](
        new Exception("Order failed validation")))(Future.successful)
      os <- sendToExchange(vo)
      ap <- updatePositions(os)
    } yield (os, ap)).onComplete {
      case Success((os, ap)) => // Marshal order submission info to caller
      case Failure(e) => // Marshal appropriate error response to caller
    }
  }

  def submitOrderWithMetrics(
    ec: ExecutionContext,
    sendToExchange: ValidatedOrder => Future[OrderSubmitted],
    updatePositions: OrderSubmitted => Future[AccountPositions],
    incrementExchangeErrorCount: () => Unit,
    o: RawOrder): Unit = {
    implicit val iec = ec

    (for {
      vo <- ValidatedOrder.fromRawOrder(o).fold(Future.failed[ValidatedOrder](
        new Exception("Order failed validation")))(Future.successful)
      os <- {
        val f = sendToExchange(vo)
        f.onFailure({ case e => incrementExchangeErrorCount() })
        f
      }
      ap <- updatePositions(os)
    } yield (os, ap)).onComplete {
      case Success((os, ap)) => // Marshal order submission info to caller
      case Failure(e) => // Marshal appropriate error response to caller
    }
  }

}
