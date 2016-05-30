package highperfscala.orderbook

import java.util.concurrent.TimeUnit

import highperfscala.orderbook.Commands.{CancelOrder, AddLimitOrder}
import org.openjdk.jmh.annotations.Mode.Throughput
import org.openjdk.jmh.annotations._
import InterleavedOrderBenchmarks._

@BenchmarkMode(Array(Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 5, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 30, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, warmups = 1, jvmArgs = Array("-Xms1G", "-Xmx1G"))
class InterleavedOrderBenchmarks {

  @Benchmark
  def eagerOneToOneCT(state: InterleavedOrderState): QueueOrderBook = {
    val b1 = QueueOrderBook.handle(() => EventInstant.now(),
      state.eagerBook, firstCancel)._1
    QueueOrderBook.handle(() => EventInstant.now(),
      b1, firstCrossSell)._1
  }

  @Benchmark
  def lazyOneToOneCT(state: InterleavedOrderState): LazyCancelOrderBook = {
    val b1 = LazyCancelOrderBook.handle(() => EventInstant.now(),
      state.lazyBook, firstCancel)._1
    LazyCancelOrderBook.handle(() => EventInstant.now(),
      b1, firstCrossSell)._1
  }

  @Benchmark
  def eagerTwoToOneCT(state: InterleavedOrderState): QueueOrderBook = {
    val b1 = QueueOrderBook.handle(() => EventInstant.now(),
      state.eagerBook, firstCancel)._1
    val b2 = QueueOrderBook.handle(() => EventInstant.now(),
      b1, secondCancel)._1
    QueueOrderBook.handle(() => EventInstant.now(),
      b2, firstCrossSell)._1
  }

  @Benchmark
  def lazyTwoToOneCT(state: InterleavedOrderState): LazyCancelOrderBook = {
    val b1 = LazyCancelOrderBook.handle(() => EventInstant.now(),
      state.lazyBook, firstCancel)._1
    val b2 = LazyCancelOrderBook.handle(() => EventInstant.now(),
      b1, secondCancel)._1
    LazyCancelOrderBook.handle(() => EventInstant.now(),
      b2, firstCrossSell)._1
  }

  @Benchmark
  def eagerOneToTwoCT(state: InterleavedOrderState): QueueOrderBook = {
    val b1 = QueueOrderBook.handle(() => EventInstant.now(),
      state.eagerBook, firstCancel)._1
    val b2 = QueueOrderBook.handle(() => EventInstant.now(),
      b1, firstCrossSell)._1
    QueueOrderBook.handle(() => EventInstant.now(),
      b2, secondCrossSell)._1
  }

  @Benchmark
  def lazyOneToTwoCT(state: InterleavedOrderState): LazyCancelOrderBook = {
    val b1 = LazyCancelOrderBook.handle(() => EventInstant.now(),
      state.lazyBook, firstCancel)._1
    val b2 = LazyCancelOrderBook.handle(() => EventInstant.now(),
      b1, firstCrossSell)._1
    LazyCancelOrderBook.handle(() => EventInstant.now(),
      b2, secondCrossSell)._1
  }
}

object InterleavedOrderBenchmarks {
  private val bidPrice = Price(BigDecimal(5))
  private val maxOrderCount = 30

  val firstCancel: CancelOrder = CancelOrder(CommandInstant.now(), OrderId(1))
  val secondCancel: CancelOrder = CancelOrder(CommandInstant.now(), OrderId(2))
  val firstCrossSell: AddLimitOrder = AddLimitOrder(CommandInstant.now(),
    SellLimitOrder(OrderId(maxOrderCount + 1), Price(bidPrice.value - 1)))
  val secondCrossSell: AddLimitOrder = AddLimitOrder(CommandInstant.now(),
    SellLimitOrder(OrderId(maxOrderCount + 2), Price(bidPrice.value - 1)))

  @State(Scope.Benchmark)
  class InterleavedOrderState {
    var lazyBook: LazyCancelOrderBook = LazyCancelOrderBook.empty
    var eagerBook: QueueOrderBook = QueueOrderBook.empty

    @Setup
    def setup(): Unit = {
      lazyBook = (1 to maxOrderCount).foldLeft(LazyCancelOrderBook.empty) {
        case (b, i) => LazyCancelOrderBook.handle(
          () => EventInstant.now(), b, AddLimitOrder(
            CommandInstant.now(), BuyLimitOrder(OrderId(i), bidPrice)))._1
      }
      eagerBook = (1 to maxOrderCount).foldLeft(QueueOrderBook.empty) {
        case (b, i) => QueueOrderBook.handle(
          () => EventInstant.now(), b, AddLimitOrder(
            CommandInstant.now(), BuyLimitOrder(OrderId(i), bidPrice)))._1
      }
    }
  }
}
