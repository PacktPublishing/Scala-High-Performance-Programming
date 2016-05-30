package highperfscala.concurrency.blocking

import java.util.concurrent.{ExecutorService, Executors, TimeUnit}

import highperfscala.concurrency.blocking.BlockingExample.{ClientId, Order, Ticker}
import org.openjdk.jmh.annotations.Mode.Throughput
import org.openjdk.jmh.annotations._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

@BenchmarkMode(Array(Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 5, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 30, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, warmups = 1, jvmArgs = Array("-Xms1G", "-Xmx1G"))
class BlockingFutureBenchmarks {

  import BlockingFutureBenchmarks._

  @Benchmark
  def withDefaultContext(state: BlockingFutureState): List[List[Order]] = {
    val futures = (1 until state.operations).map{_ =>
      BlockingExample.JdbcOrderRepository.findBuyOrders(
        state.clientId, state.ticker
      )(state.defaultC)
    }

    implicit val ex = state.defaultC
    Await.result(
      Future.sequence(futures).map(_.toList),
      Duration("5 minutes")
    )
  }

  @Benchmark
  def withDedicatedContext(state: BlockingFutureState): List[List[Order]] = {
    val futures = (1 until state.operations).map{_ =>
      BlockingExample.JdbcOrderRepository.findBuyOrders(
        state.clientId, state.ticker
      )(state.dedicatedC)
    }

    implicit val ex = state.defaultC  // we use CPU-bound context for computations below
    Await.result(
      Future.sequence(futures).map(_.toList),
      Duration("5 minutes")
    )
  }

}

object BlockingFutureBenchmarks {

  @State(Scope.Benchmark)
  class BlockingFutureState {

    @Param(Array("10", "1000"))
    var operations: Int = 0

    val clientId = ClientId(12345)
    val ticker = Ticker("FOO")

    var defaultC: ExecutionContext = null
    var dedicatedC: ExecutionContext = null
    var es: ExecutorService = null

    @Setup(Level.Trial)
    def setup(): Unit = {
      defaultC = scala.concurrent.ExecutionContext.global
      es = {
        val i = Runtime.getRuntime.availableProcessors * 20
        Executors.newFixedThreadPool(i)
      }
      dedicatedC = ExecutionContext.fromExecutorService(es)
    }

    @TearDown(Level.Trial)
    def tearDown(): Unit = {
      es.shutdownNow()
    }

  }

}
