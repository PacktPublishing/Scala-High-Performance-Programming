package highperfscala.concurrency.blocking

import scala.concurrent.{ExecutionContext, Future}

object BlockingExample {

  case class Ticker(value: String) extends AnyVal

  case class ClientId(value: Long) extends AnyVal

  case class Order(ticker: Ticker, clientId: ClientId)

  object Order {

    val staticList = List(
      Order(Ticker("FOO"), ClientId(12345)),
      Order(Ticker("FOO"), ClientId(5437)),
      Order(Ticker("BAR"), ClientId(12345)),
      Order(Ticker("FOO"), ClientId(9864)),
      Order(Ticker("BAR"), ClientId(12345)),
      Order(Ticker("BAR"), ClientId(5680)),
      Order(Ticker("BAR"), ClientId(12345)),
      Order(Ticker("FOO"), ClientId(542467))
    )
  }

  object JdbcOrderRepository {

    def findBuyOrders(
      client: ClientId,
      ticker: Ticker)(implicit ec: ExecutionContext): Future[List[Order]] = Future {
      Thread.sleep(100)
      Order.staticList.filter(o => o.clientId == client && o.ticker == ticker)
    }(ec)

    import scala.concurrent.ExecutionContext.Implicits.global

    def findBuyOrdersWithBlocking(
      client: ClientId,
      ticker: Ticker): Future[List[Order]] = Future {
      scala.concurrent.blocking {
        Thread.sleep(100)
        Order.staticList.filter(o => o.clientId == client && o.ticker == ticker)
      }
    }

  }

}
