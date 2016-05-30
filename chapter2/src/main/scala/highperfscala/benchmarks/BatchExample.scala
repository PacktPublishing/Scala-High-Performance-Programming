package highperfscala
package benchmarks

import highperfscala.benchmarks.BatchExample._
import org.openjdk.jmh.annotations._

// Scala transcription of
// http://hg.openjdk.java.net/code-tools/jmh/file/bcec9a03787f/jmh-samples/src/main/java/org/openjdk/jmh/samples/JMHSample_26_BatchSize.java
//
// Run via
// sbt 'project performance;jmh:run .*BatchExample -f 1 -jvmArgs "-Xmx1G -Xms1G"'
@State(Scope.Thread)
class BatchExample {
  @Benchmark
  @Warmup(iterations = 5, time = 1)
  @Measurement(iterations = 5, time = 1)
  @BenchmarkMode(Array(Mode.Throughput))
  def measureWrong_1(): java.util.List[String] = {
    xs.add(xs.size() / 2, "something")
    xs
  }

  @Benchmark
  @Warmup(iterations = 5, time = 5)
  @Measurement(iterations = 5, time = 5)
  @BenchmarkMode(Array(Mode.Throughput))
  def measureWrong_5(): java.util.List[String] = {
    xs.add(xs.size() / 2, "something")
    xs
  }

  @Benchmark
  @Warmup(iterations = 5, batchSize = 5000)
  @Measurement(iterations = 5, batchSize = 5000)
  @BenchmarkMode(Array(Mode.SingleShotTime))
  def measureRight(): java.util.List[String] = {
    xs.add(xs.size() / 2, "something")
    xs
  }

  @Setup(Level.Iteration)
  def setup(): Unit = xs.clear()
}

object BatchExample {
  val xs = new java.util.LinkedList[String]()
}
