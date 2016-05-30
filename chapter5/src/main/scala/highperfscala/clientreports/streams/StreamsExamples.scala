package highperfscala.clientreports.streams

import scala.annotation.tailrec

object StreamsExamples {

  def powerOf2: Stream[Int] = {
    def next(n: Int): Stream[Int] = {
      println(s"Adding $n")
      n #:: next(2 * n)
    }
    1 #:: next(1)
  }

  def recursivePowerOf2(n: Int): Stream[Int] =
    math.pow(2, n).toInt #:: recursivePowerOf2(n+1)

  @tailrec
  def drop[A](s: Stream[A], count: Int): Stream[A] = count match {
    case 0 => s
    case n if n > 0 => drop(s.tail, count - 1)
    case n if n < 0 => throw new Exception("cannot drop negative count")
  }

  def max(s: => Stream[Int]): Option[Int] = {
    @tailrec
    def loop(ss: Stream[Int], current: Option[Int]): Option[Int] = ss match {
      case Stream.Empty => current
      case h #:: rest if current.exists(_ >= h) => loop(rest, current)
      case h #:: rest => loop(rest, Some(h))
    }
    loop(s, None)
  }

  def memoizingMax(s: => Stream[Int]): Option[Int] = {
    @tailrec
    def loop(ss: Stream[Int], current: Int): Int = ss match {
      case Stream.Empty => current
      case h #:: rest if h > current => loop(rest, h)
      case h #:: rest if h <= current => loop(rest, current)
    }

    // This pattern matching creates a reference to the tail of s and
    // prevents eager garbage collection of the intermediate elements.
    s match {
      case Stream.Empty => None
      case h #:: rest => Some(loop(rest, h))
    }
  }

  def range(s: Seq[Int]): Int = s.max - s.min

}
