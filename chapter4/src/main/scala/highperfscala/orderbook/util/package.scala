package highperfscala
package orderbook

import Commands._
import com.codahale.metrics.Snapshot
import highperfscala.orderbook.LatencyBenchmark.{LazyImplementation, QueueImplementation, BookImplementation}
import org.slf4s.Logging

package object util extends Logging {

  def infiniteCommands(sample: List[Command]): Stream[Command] =
    Stream.continually(sample.toStream).flatten

  def generateCount(sample: List[Command], count: Int): List[Command] =
    infiniteCommands(sample).take(count).toList

  def jvmWarmUp(sample: List[Command], impl: BookImplementation): Unit = {
    log.debug("Begin warmUp")
    val commands = util.generateCount(sample, 100000)
    val systemEventTime = () => EventInstant.now()
    impl match {
      case QueueImplementation => commands.foldLeft(QueueOrderBook.empty) {
        case (book, command) => QueueOrderBook.handle(
          systemEventTime, book, command)._1
      }
      case LazyImplementation => commands.foldLeft(LazyCancelOrderBook.empty) {
        case (book, command) => LazyCancelOrderBook.handle(
          systemEventTime, book, command)._1
      }
    }
    log.debug("End warmUp")
  }

  def printSnapshot(s: Snapshot): Unit = println {
    s"""
       |Processed ${s.size} commands
       |99p latency: ${s.get99thPercentile()} ms
       |99.9p latency: ${s.get999thPercentile()} ms
       |Maximum latency: ${s.getMax} ms
        """.stripMargin
  }


}
