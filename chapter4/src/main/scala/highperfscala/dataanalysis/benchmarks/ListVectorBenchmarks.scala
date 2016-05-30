package highperfscala.dataanalysis.benchmarks

import java.util.concurrent.TimeUnit

import highperfscala.dataanalysis.benchmarks.ListVectorBenchmarks.ExecutionList
import highperfscala.dataanalysis.util.DataCodec
import highperfscala.dataanalysis.{ListVectorExperiment, Midpoint, MinuteRollUp, Return}
import org.openjdk.jmh.annotations.Mode._
import org.openjdk.jmh.annotations.{BenchmarkMode, _}

@BenchmarkMode(Array(Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 5, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 30, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, warmups = 1, jvmArgs = Array("-Xms1G", "-Xmx1G"))
class ListVectorBenchmarks {

  @Benchmark
  def computeReturnsWithList(s: ExecutionList): List[Return] = {
    ListVectorExperiment.computeReturnsWithList(
      MinuteRollUp(s.rollUp),
      s.list
    )
  }

  @Benchmark
  def computeReturnsWithVector(s: ExecutionList): Vector[Return] = {
    ListVectorExperiment.computeReturnsWithVector(
      MinuteRollUp(s.rollUp),
      s.vector
    )
  }

}

object ListVectorBenchmarks {

  @State(Scope.Benchmark)
  class ExecutionList {

    @Param(Array("10", "60", "120"))
    var rollUp: Int = 0

    var list: List[Midpoint] = Nil
    var vector: Vector[Midpoint] = Vector.empty

    @Setup(Level.Trial)
    def setup(): Unit = {
      list = DataCodec.read(
        getClass.getResourceAsStream("/dataanalysis/executions"))
        .map(Midpoint.fromExecution)
      vector = list.toVector
    }
  }

}
