package highperfscala.option

import java.util.concurrent.TimeUnit

import highperfscala.option.OptionBenchmarks.ShareCount
import org.openjdk.jmh.annotations.{Fork, _}
import org.openjdk.jmh.annotations.Mode._
import OptionFoldingBenchmarks._

import scalaz.@@

@BenchmarkMode(Array(Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 5, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 30, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, warmups = 1, jvmArgs = Array("-Xms1G", "-Xmx1G"))
class OptionFoldingBenchmarks {

  @Benchmark
  def scalaOption(): ShareCount =
    scalaSome.fold(ShareCount(0))(c => ShareCount(c.value * 2))


  @Benchmark
  def optOption(): ShareCount =
    OptOps.fold(optSome)(ShareCount(0))(c => ShareCount(c.value * 2))

}

object OptionFoldingBenchmarks {

  case class ShareCount(value: Long) extends AnyVal

  val scalaSome: Option[ShareCount] = Some(ShareCount(7))
  val optSome: ShareCount @@ Opt = OptOps.some(ShareCount(7))
}
