package highperfscala.option

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.Mode._
import org.openjdk.jmh.annotations.{BenchmarkMode, _}

import scalaz.@@
import OptionBenchmarks._

@BenchmarkMode(Array(Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 5, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 30, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, warmups = 1, jvmArgs = Array("-Xms1G", "-Xmx1G"))
class OptionBenchmarks {

  @Benchmark
  def scalaOption(s: OptionState): Option[ShareCount] = {
    val c = s.counter
    s.counter = s.counter + 1
    c % s.someFrequency match {
      case 0 => Some(ShareCount(s.counter))
      case _ => None
    }
  }

  @Benchmark
  def nullOption(s: OptionState): ShareCount @@ Opt = {
    OptOps.some(ShareCount(s.counter))
    val c = s.counter
    s.counter = s.counter + 1
    c % s.someFrequency match {
      case 0 => OptOps.some(ShareCount(s.counter))
      case _ => OptOps.none
    }
  }

  @Benchmark
  def nullOptionNoneReused(s: OptionState): ShareCount @@ Opt = {
    val c = s.counter
    s.counter = s.counter + 1
    c % s.someFrequency match {
      case 0 => OptOps.some(ShareCount(s.counter))
      case _ => OptionBenchmarks.noShareCount
    }
  }
}

object OptionBenchmarks {

  val noShareCount = OptOps.none[ShareCount]

  case class ShareCount(value: Long) extends AnyVal

  @State(Scope.Benchmark)
  class OptionState {

    @Param(Array("1", "2", "3", "5"))
    var someFrequency: Int = 0

    var counter: Long = 0
  }
}
