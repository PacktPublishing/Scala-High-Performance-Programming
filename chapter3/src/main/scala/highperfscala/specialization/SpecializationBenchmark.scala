package highperfscala.specialization

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.Mode._
import org.openjdk.jmh.annotations._

import SpecializationBenchmark._

@BenchmarkMode(Array(Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 5, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 30, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, warmups = 1, jvmArgs = Array("-Xms1G", "-Xmx1G"))
class SpecializationBenchmark {

  @Benchmark
  def specialized(): Double =
    specializedExecution.shareCount.toDouble * specializedExecution.price

  @Benchmark
  def boxed(): Double =
    boxedExecution.shareCount.toDouble * boxedExecution.price
}

object SpecializationBenchmark {
  class SpecializedExecution[@specialized(Int) T1, @specialized(Double) T2](
    val shareCount: Long, val price: Double)
  class BoxingExecution[T1, T2](val shareCount: T1, val price: T2)

  val specializedExecution: SpecializedExecution[Int, Double] =
    new SpecializedExecution(10l, 2d)
  val boxedExecution: BoxingExecution[Long, Double] = new BoxingExecution(10l, 2d)
}
