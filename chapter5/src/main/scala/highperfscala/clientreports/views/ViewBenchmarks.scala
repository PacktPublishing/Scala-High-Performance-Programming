package highperfscala.clientreports.views

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.Mode.Throughput
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 5, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 30, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, warmups = 1, jvmArgs = Array("-Xms1G", "-Xmx1G"))
class ViewBenchmarks {

  import ViewBenchmarks._

  @Benchmark
  def singleTransformList(state: ViewState): List[Int] =
    state.numbers.map(_ * 2)

  @Benchmark
  def singleTransformView(state: ViewState): Vector[Int] =
    state.numbers.view.map(_ * 2).toVector

  @Benchmark
  def twoTransformsList(state: ViewState): List[Int] =
    state.numbers.map(_ * 2).filter(_ % 3 == 0)

  @Benchmark
  def twoTransformsView(state: ViewState): Vector[Int] =
    state.numbers.view.map(_ * 2).filter(_ % 3 == 0).toVector

  @Benchmark
  def threeTransformsList(state: ViewState): List[Int] =
    state.numbers.map(_ * 2).map(_ + 7).filter(_ % 3 == 0)

  @Benchmark
  def threeTransformsView(state: ViewState): Vector[Int] =
    state.numbers.view.map(_ * 2).map(_ + 7).filter(_ % 3 == 0).toVector
}

object ViewBenchmarks {

  @State(Scope.Benchmark)
  class ViewState {

    @Param(Array("10", "1000", "1000000"))
    var collectionSize: Int = 0

    var numbers: List[Int] = Nil

    @Setup
    def setup(): Unit = {
      numbers = (for (i <- 1 to collectionSize) yield i).toList
    }
  }
}
