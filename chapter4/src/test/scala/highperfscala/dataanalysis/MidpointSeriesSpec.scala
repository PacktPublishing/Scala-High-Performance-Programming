package highperfscala.dataanalysis

import org.joda.time.DateTime
import org.specs2.mutable.Specification

class MidpointSeriesSpec extends Specification {

  "A MidpointSeries" should {

    "be properly created from execution data points" in {
      val t0 = TimestampMinutes.fromDateTime(DateTime.now)
      val t1 = t0.next
      val t2 = t1.next
      val t3 = t2.next
      val t4 = t3.next
      val t5 = t4.next
      val t6 = t5.next
      val executions = Vector(
        Execution(t0, AskPrice(40), BidPrice(20)),
        Execution(t1, AskPrice(30), BidPrice(25)),
        Execution(t1, AskPrice(50), BidPrice(22)),
        Execution(t4, AskPrice(24), BidPrice(16)),
        Execution(t4, AskPrice(84), BidPrice(78)),
        Execution(t4, AskPrice(64), BidPrice(37)),
        Execution(t6, AskPrice(41), BidPrice(23))
      )

      val series = MidpointSeries.fromExecution(executions)
      series.size ==== 7
      series.midpointAt(t0).get ==== Midpoint(t0, 30)
      series.midpointAt(t1).get ==== Midpoint(t1, 31.75)
      series.midpointAt(t2).get ==== Midpoint(t2, 31.75)
      series.midpointAt(t3).get ==== Midpoint(t3, 31.75)
      series.midpointAt(t4).get ==== Midpoint(t4, 50.5)
      series.midpointAt(t5).get ==== Midpoint(t5, 50.5)
      series.midpointAt(t6).get ==== Midpoint(t6, 32)
      series.midpointAt(t6.next) ==== None
    }

  }

}
