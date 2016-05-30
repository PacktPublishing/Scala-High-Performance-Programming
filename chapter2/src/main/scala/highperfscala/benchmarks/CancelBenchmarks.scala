package highperfscala
package benchmarks

import java.util.concurrent.TimeUnit

import highperfscala.benchmarks.CancelBenchmarks._
import highperfscala.orderbook.Commands._
import highperfscala.orderbook.Events.Event
import highperfscala.orderbook.{BuyLimitOrder, OrderId, Price, OrderBook}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.annotations.Mode._

// Run via
// sbt ';project performance;jmh:run .*CancelBenchmarks -wi 3 -i 10 -f 1 -wbs 100000 -bs 100000 -jvmArgs "-Xmx1G -Xms1G" -foe true -p enqueuedOrderCount=10,100,1000,5000'
@BenchmarkMode(Array(Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class CancelBenchmarks {

  @Benchmark
  def cancelLastOrderInLine(b: BookWithLargeQueue): (OrderBook, Event) =
    OrderBook.handle(b.book, b.cancelLast)

  @Benchmark
  def cancelFirstOrderInLine(b: BookWithLargeQueue): (OrderBook, Event) =
    OrderBook.handle(b.book, b.cancelFirst)

  @Benchmark
  def cancelNonexistentOrder(b: BookWithLargeQueue): (OrderBook, Event) =
    OrderBook.handle(b.book, b.cancelNonexistent)
}

object CancelBenchmarks {

  @State(Scope.Benchmark)
  class BookWithLargeQueue {
    private val p = Price(BigDecimal(1.00))
    private val firstId: Int = 1
    private val defaultCancelLast = CancelOrder(OrderId(-1))

    @Param(Array("1", "100", "1000"))
    var enqueuedOrderCount: Int = 0

    var book: OrderBook = OrderBook.empty

    @Setup(Level.Trial)
    def setup(): Unit = {
      if (enqueuedOrderCount < 0)
        sys.error(s"Invalid enqueued order count = $enqueuedOrderCount")
      assert(book == OrderBook.empty)
      assert(cancelLast == defaultCancelLast)

      cancelLast = CancelOrder(OrderId(enqueuedOrderCount))
      book = {
        (firstId to enqueuedOrderCount).foldLeft(OrderBook.empty) {
          case (ob, i) =>
            OrderBook.handle(ob, AddLimitOrder(BuyLimitOrder(OrderId(i), p)))._1
        }
      }

      assert(cancelLast != defaultCancelLast)
      if (enqueuedOrderCount > 0)
        assert(book.bids.head._2.size == enqueuedOrderCount,
          s"Book built incorrectly! Expected book to contain " +
            s"$enqueuedOrderCount bids for $p, but actual book is $book")
    }

    var cancelLast: CancelOrder = defaultCancelLast
    val cancelFirst: CancelOrder = CancelOrder(OrderId(firstId))
    val cancelNonexistent: CancelOrder = CancelOrder(OrderId(-1))
  }
}
