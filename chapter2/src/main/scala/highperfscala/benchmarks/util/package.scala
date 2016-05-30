package highperfscala
package benchmarks

import com.codahale.metrics.Snapshot
import orderbook.Commands.Command
import orderbook.OrderBook
import org.slf4s.Logging

package object util extends Logging {

  def infiniteCommands(sample: List[Command]): Stream[Command] =
    Stream.continually(sample.toStream).flatten

  def generateCount(sample: List[Command], count: Int): List[Command] =
    infiniteCommands(sample).take(count).toList

  def jvmWarmUp(sample: List[Command]): Unit = {
    log.debug("Begin warm up")
    val commands = util.generateCount(sample, 100000)
    commands.foldLeft(OrderBook.empty) {
      case (book, command) => OrderBook.handle(book, command)._1
    }
    log.debug("End warm up")
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
