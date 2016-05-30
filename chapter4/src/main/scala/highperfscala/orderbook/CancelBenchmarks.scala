package highperfscala
package orderbook

import java.util.concurrent.TimeUnit

import highperfscala.orderbook.Commands._
import highperfscala.orderbook.Events.Event
import org.openjdk.jmh.annotations.Mode.Throughput
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 5, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 30, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, warmups = 1, jvmArgs = Array("-Xms1G", "-Xmx1G"))
class CancelBenchmarks {
  import CancelBenchmarks._

  @Benchmark
  def eagerCancelLastOrderInLine(b: BookWithLargeQueue): (QueueOrderBook, Event) =
    QueueOrderBook.handle(systemEventTime, b.eagerBook, b.cancelLast)

  @Benchmark
  def eagerCancelFirstOrderInLine(b: BookWithLargeQueue): (QueueOrderBook, Event) =
    QueueOrderBook.handle(systemEventTime, b.eagerBook, b.cancelFirst)

  @Benchmark
  def eagerCancelNonexistentOrder(b: BookWithLargeQueue): (QueueOrderBook, Event) =
    QueueOrderBook.handle(systemEventTime, b.eagerBook, b.cancelNonexistent)

  @Benchmark
  def lazyCancelLastOrderInLine(b: BookWithLargeQueue): (LazyCancelOrderBook, Event) =
    LazyCancelOrderBook.handle(systemEventTime, b.lazyBook, b.cancelLast)

  @Benchmark
  def lazyCancelFirstOrderInLine(b: BookWithLargeQueue): (LazyCancelOrderBook, Event) =
    LazyCancelOrderBook.handle(systemEventTime, b.lazyBook, b.cancelFirst)

  @Benchmark
  def lazyCancelNonexistentOrder(b: BookWithLargeQueue): (LazyCancelOrderBook, Event) =
    LazyCancelOrderBook.handle(systemEventTime, b.lazyBook, b.cancelNonexistent)
}

object CancelBenchmarks {

  val systemEventTime = () => EventInstant.now()

  @State(Scope.Benchmark)
  class BookWithLargeQueue {
    private val p = Price(BigDecimal(1.00))
    private val firstId: Int = 1
    private val defaultCancelLast = CancelOrder(
      CommandInstant.now(), OrderId(-1))

    @Param(Array("1", "10"))
    var enqueuedOrderCount: Int = 0

    var eagerBook: QueueOrderBook = QueueOrderBook.empty

    var lazyBook: LazyCancelOrderBook = LazyCancelOrderBook.empty

    @Setup(Level.Trial)
    def setup(): Unit = {
      if (enqueuedOrderCount < 0)
        sys.error(s"Invalid enqueued order count = $enqueuedOrderCount")
      assert(eagerBook == QueueOrderBook.empty)
      assert(lazyBook == LazyCancelOrderBook.empty)
      assert(cancelLast == defaultCancelLast)

      cancelLast = CancelOrder(
        CommandInstant.now(), OrderId(enqueuedOrderCount))
      eagerBook = {
        (firstId to enqueuedOrderCount).foldLeft(QueueOrderBook.empty) {
          case (ob, i) =>
            QueueOrderBook.handle(
              () => EventInstant.now(),
              ob, AddLimitOrder(
                CommandInstant.now(), BuyLimitOrder(OrderId(i), p)))._1
        }
      }
      lazyBook = {
        (firstId to enqueuedOrderCount).foldLeft(LazyCancelOrderBook.empty) {
          case (ob, i) =>
            LazyCancelOrderBook.handle(
              () => EventInstant.now(),
              ob, AddLimitOrder(
                CommandInstant.now(), BuyLimitOrder(OrderId(i), p)))._1
        }
      }

      assert(cancelLast != defaultCancelLast)
      if (enqueuedOrderCount > 0)
        assert(eagerBook.bids.head._2.size == enqueuedOrderCount,
          s"Book built incorrectly! Expected book to contain " +
            s"$enqueuedOrderCount bids for $p, but actual book is $eagerBook")
    }

    var cancelLast: CancelOrder = defaultCancelLast
    val cancelFirst: CancelOrder = CancelOrder(
      CommandInstant.now(), OrderId(firstId))
    val cancelNonexistent: CancelOrder = CancelOrder(
      CommandInstant.now(), OrderId(-1))
  }
}

