package highperfscala.concurrency.future

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.Mode.Throughput
import org.openjdk.jmh.annotations._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

@BenchmarkMode(Array(Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 5, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 30, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, warmups = 1, jvmArgs = Array("-Xms1G", "-Xmx1G"))
class TransformFutureBenchmarks {

  import TransformFutureBenchmarks._

  @Benchmark
  def manyTransforms(state: TransformFutureState): Int = {
    import scala.concurrent.ExecutionContext.Implicits._
    val init = Future(0)
    val res = (1 until state.operations).foldLeft(init)((f, _) => f.map(_ + 1))
    Await.result(res, Duration("5 minutes"))
  }

  @Benchmark
  def oneTransform(state: TransformFutureState): Int = {
    import scala.concurrent.ExecutionContext.Implicits._
    val res = Future {
      (1 until state.operations).foldLeft(0)((acc, _) => acc + 1)
    }
    Await.result(res, Duration("5 minutes"))
  }

}

object TransformFutureBenchmarks {

  @State(Scope.Benchmark)
  class TransformFutureState {

    @Param(Array("5", "10"))
    var operations: Int = 0

  }

}
