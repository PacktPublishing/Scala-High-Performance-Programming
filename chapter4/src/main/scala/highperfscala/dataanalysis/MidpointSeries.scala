package highperfscala.dataanalysis

import scala.annotation.tailrec

class MidpointSeries private(val points: Vector[Midpoint]) extends AnyVal {

  def returns(rollUp: MinuteRollUp): Vector[Return] = {
    for {
      i <- (rollUp.value until points.size).toVector
    } yield Return.fromMidpoint(points(i - rollUp.value), points(i))
  }

  def midpointAt(time: TimestampMinutes): Option[Midpoint] = {
    if (points.isEmpty || time.value < points.head.time.value ||
      time.value > points.last.time.value) {
      None
    } else {
      val index = time.value - points.head.time.value
      Some(points(index))
    }
  }

  private[dataanalysis] def size = points.size // provided for testing

}

object MidpointSeries {

  private def removeDuplicates(v: Vector[Midpoint]): Vector[Midpoint] = {

    @tailrec
    def loop(
      current: Midpoint,
      rest: Vector[Midpoint],
      result: Vector[Midpoint]): Vector[Midpoint] = {
      val sameTime = current +: rest.takeWhile(_.time == current.time)
      val average = sameTime.map(_.value).sum / sameTime.size

      val newResult = result :+ Midpoint(current.time, average)
      rest.drop(sameTime.size - 1) match {
        case h +: r => loop(h, r, newResult)
        case _ => newResult
      }
    }

    v match {
      case h +: rest => loop(h, rest, Vector.empty)
      case _ => Vector.empty
    }
  }

  private def extrapolate(
    a: Midpoint,
    b: Midpoint,
    time: TimestampMinutes): Midpoint = {
    val price = a.value +
      ((time.value - a.time.value) / (b.time.value - a.time.value)) *
        (b.value - a.value)
    Midpoint(time, price)
  }

  private def addMissingDataPoints(v: Vector[Midpoint]): Vector[Midpoint] = {
    @tailrec
    def loop(
      previous: Midpoint,
      rest: Vector[Midpoint],
      result: Vector[Midpoint]): Vector[Midpoint] = rest match {
      case current +: mPoints if previous.time.value == current.time.value - 1 =>
        // Nothing to extrapolate, the data points are consecutive
        loop(current, mPoints, result :+ previous)

      case current +: mPoints if previous.time.value < current.time.value - 1 =>
        //Need to generate a data point
        val newPoint = extrapolate(previous, current, previous.time.next)
        loop(newPoint, rest, result :+ previous)

      case _ => result :+ previous
    }

    v match {
      case h +: rest => loop(h, rest, Vector.empty)
      case _ => Vector.empty
    }
  }

  def fromExecution(executions: Vector[Execution]): MidpointSeries = {
    new MidpointSeries(
      addMissingDataPoints(
        removeDuplicates(
          executions.map(Midpoint.fromExecution))))
  }

}
