package highperfscala.option

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.Mode._
import org.openjdk.jmh.annotations.{BenchmarkMode, _}
import OptionCreationBenchmarks._

import scalaz.@@

@BenchmarkMode(Array(Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 5, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 30, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, warmups = 1, jvmArgs = Array("-Xms1G", "-Xmx1G"))
class OptionCreationBenchmarks {

  @Benchmark
  def scalaSome(): Option[ShareCount] = Some(ShareCount(1))

  @Benchmark
  def scalaNone(): Option[ShareCount] = None

  @Benchmark
  def optSome(): ShareCount @@ Opt = OptOps.some(ShareCount(1))

  @Benchmark
  def optSomeWithNullChecking(): ShareCount @@ Opt =
    OptOps.nullCheckingSome(ShareCount(1))

  @Benchmark
  def optNone(): ShareCount @@ Opt = OptOps.none

  @Benchmark
  def optNoneReuse(): ShareCount @@ Opt = noShares
}

object OptionCreationBenchmarks {
  case class ShareCount(value: Long) extends AnyVal
  val noShares: ShareCount @@ Opt = OptOps.none
}
