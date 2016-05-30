package highperfscala.dataanalysis.benchmarks

import java.util.concurrent.TimeUnit

import highperfscala.dataanalysis._
import highperfscala.dataanalysis.benchmarks.ReturnSeriesFrameBenchmarks.SeriesState
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.annotations.Mode._

import scala.util.Random

@BenchmarkMode(Array(Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 5, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 30, time = 10, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, warmups = 1, jvmArgs = Array("-Xms1G", "-Xmx1G"))
class ReturnSeriesFrameBenchmarks {

  @Benchmark
  def normalizeVectors(s: SeriesState): VectorBasedReturnSeriesFrame = {
    ReturnSeriesFrame.scaleVector(s.vectorBased)
  }

  @Benchmark
  def normalizeArray(s: SeriesState): ArrayBasedReturnSeriesFrame = {
    ReturnSeriesFrame.scaleWithMap(s.arrayBased)
  }

  @Benchmark
  def normalizeSpireLoop(s: SeriesState): ArrayBasedReturnSeriesFrame = {
    ReturnSeriesFrame.scaleWithSpire(s.arrayBased)
  }

  @Benchmark
  def normalizeSaddleMatrix(s: SeriesState): MatrixBasedReturnSeriesFrame = {
    ReturnSeriesFrame.scaleWithSaddle(s.matrixBased)
  }

}

object ReturnSeriesFrameBenchmarks {

  @State(Scope.Benchmark)
  class SeriesState {

    @Param(Array("10"))
    var seriesCount: Int = 0

    @Param(Array(/*"60", "1440",*/ "300000"))
    var seriesSize: Int = 0

    var vectorBased: VectorBasedReturnSeriesFrame = null
    var arrayBased: ArrayBasedReturnSeriesFrame = null
    var matrixBased: MatrixBasedReturnSeriesFrame = null

    @Setup(Level.Trial)
    def setup(): Unit = {
      val series = new Array[Array[Return]](seriesCount)
      for(i <- 0 until seriesCount){
        val localSeries = new Array[Return](seriesSize)
        for(j <- 0 until seriesSize){
          localSeries(j) = Return(Random.nextInt(100))
        }
        series(i) = localSeries
      }

      val vectors = series.map(_.toVector).toVector

      vectorBased = new VectorBasedReturnSeriesFrame(vectors)
      arrayBased = ReturnSeriesFrame.newArrayBasedFrame(series)
      matrixBased = ReturnSeriesFrame.newMatrixBasedFrame(series)
      vectorBased.scalingVector
      arrayBased.scalingVector
    }
  }

}
