package highperfscala.concurrency.task

import java.util.concurrent.{ExecutorService, Executors, TimeUnit}

import org.openjdk.jmh.annotations.Mode.Throughput
import org.openjdk.jmh.annotations._

import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration.Duration
import scalaz.concurrent.Task

@BenchmarkMode(Array(Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 5, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 30, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, warmups = 1, jvmArgs = Array("-Xms1G", "-Xmx1G"))
class TaskFutureBenchmarks {

  import TaskFutureBenchmarks._

  @Benchmark
  def mapWithFuture(state: TaskFutureState): Int = {
    implicit val ec = state.context
    val init = Future(0)
    val res = (1 until state.operations).foldLeft(init)((f, _) => f.map(_ + 1))
    Await.result(res, Duration("5 minutes"))
  }

  @Benchmark
  def mapWithTask(state: TaskFutureState): Int = {
    val init = Task(0)(state.es)
    val res = (1 until state.operations).foldLeft(init)((t, _) => t.map(_ + 1))
    res.unsafePerformSync
  }

  @Benchmark
  def flatMapWithFuture(state: TaskFutureState): Int = {
    implicit val ec = state.context
    val init = Future(0)
    val res = (1 until state.operations).foldLeft(init)((f, _) =>
      f.flatMap(i => Future(i + 1)))
    Await.result(res, Duration("5 minutes"))
  }

  @Benchmark
  def flatMapWithTask(state: TaskFutureState): Int = {
    val init = Task(0)(state.es)
    val res = (1 until state.operations).foldLeft(init)((t, _) =>
      t.flatMap(i => Task(i + 1)(state.es)))
    res.unsafePerformSync
  }

}

object TaskFutureBenchmarks {

  @State(Scope.Benchmark)
  class TaskFutureState {

    @Param(Array("5", "10", "100"))
    var operations: Int = 0

    var es: ExecutorService = null
    var context: ExecutionContext = null

    @Setup(Level.Trial)
    def setup(): Unit = {
      es = Executors.newFixedThreadPool(20)
      context = ExecutionContext.fromExecutor(es)
    }

    @TearDown(Level.Trial)
    def tearDown(): Unit = {
      es.shutdownNow()
    }
  }

}
