package highperfscala.dataanalysis

import org.saddle._

trait ReturnSeriesFrame

class VectorBasedReturnSeriesFrame(
  val series: Vector[Vector[Return]]) extends ReturnSeriesFrame {

  lazy val scalingVector: Vector[Double] =
    for (i <- series.indices.toVector) yield series(i).max.value

}

class ArrayBasedReturnSeriesFrame(
  val series: Array[Array[Return]]) extends ReturnSeriesFrame {

  lazy val scalingVector: Array[Double] = {
    val v = new Array[Double](series.length)
    for (i <- series.indices) {
      v(i) = series(i).max.value
    }
    v
  }

}

class MatrixBasedReturnSeriesFrame(
  val series: Mat[Return]) extends ReturnSeriesFrame {

  val scalingVector: Array[Double] = {
    val v = new Array[Double](series.cols().size)
    for (i <- v.indices) {
      v(i) = series.col(i).toSeq.max.value
    }
    v
  }

  val scalingVec: Vec[Double] = Vec(scalingVector)

}

object ReturnSeriesFrame {

  def newArrayBasedFrame(series: Array[Array[Return]]): ArrayBasedReturnSeriesFrame = {
    new ArrayBasedReturnSeriesFrame(series)
  }

  def newMatrixBasedFrame(series: Array[Array[Return]]): MatrixBasedReturnSeriesFrame = {
    new MatrixBasedReturnSeriesFrame(Mat(series))
  }

  def scaleVector(frame: VectorBasedReturnSeriesFrame): VectorBasedReturnSeriesFrame = {
    new VectorBasedReturnSeriesFrame(
      frame.series.zip(frame.scalingVector).map { case (series, scaling) =>
        series.map(point => Return(point.value / scaling))
      }
    )
  }

  def scaleWithMap(frame: ArrayBasedReturnSeriesFrame): ArrayBasedReturnSeriesFrame = {
    new ArrayBasedReturnSeriesFrame(
      frame.series.zip(frame.scalingVector).map { case (series, scaling) =>
        series.map(point => Return(point.value / scaling))
      })
  }

  def scaleWithSpire(frame: ArrayBasedReturnSeriesFrame): ArrayBasedReturnSeriesFrame = {
    import spire.syntax.cfor._

    val result = new Array[Array[Return]](frame.series.length)

    cfor(0)(_ < frame.series.length, _ + 1) { i =>
      val s = frame.series(i)
      val scaled = new Array[Return](s.length)
      cfor(0)(_ < s.length, _ + 1) { j =>
        val point = s(j)
        scaled(j) = Return(point.value / frame.scalingVector(i))
      }
      result(i) = scaled
    }

    new ArrayBasedReturnSeriesFrame(result)
  }

  def scaleWithSaddle(frame: MatrixBasedReturnSeriesFrame): MatrixBasedReturnSeriesFrame =
    new MatrixBasedReturnSeriesFrame(
      (frame.series.map(_.value) dot frame.scalingVec).map(Return.apply)
    )

}
