package highperfscala
package benchmarks

import java.io.File
import highperfscala.orderbook.Commands.Command
import highperfscala.orderbook.OrderBook
import org.mpierce.metrics.reservoir.hdrhistogram.HdrHistogramReservoir
import util._

import scala.annotation.tailrec

object SecondLatencyBenchmark {

  def main(args: Array[String]): Unit = {

    val commandSample = DataCodec.read(new File(args(0)))
    val (commandsPerSecond, iterations) = (args(1).toInt, args(2).toInt)
    val totalCommandCount = commandsPerSecond * iterations

    jvmWarmUp(commandSample)

    @tailrec
    def sendCommands(
      xs: List[(List[Command], Int)],
      ob: OrderBook,
      testStart: Long,
      histogram: HdrHistogramReservoir): (OrderBook, HdrHistogramReservoir) =
      xs match {
        case head :: tail =>
          val (batch, offsetInSeconds) = head
          val shouldStart = testStart + (1000 * offsetInSeconds)

          while (shouldStart > System.currentTimeMillis()) {
            // keep the thread busy while waiting for the next batch to be sent
          }

          val updatedBook = batch.foldLeft(ob) {
            case (accBook, c) =>
              val newBook = OrderBook.handle(accBook, c)._1
              val operationEnd = System.currentTimeMillis()
              // record latency
              histogram.update(operationEnd - shouldStart)
              newBook
          }

          sendCommands(tail, updatedBook, testStart, histogram)
        case Nil => (ob, histogram)
      }

    val (_, histogram) = sendCommands(
      // Organizes commands per 1 second batches
      generateCount(commandSample, totalCommandCount)
        .grouped(commandsPerSecond).zipWithIndex
        .toList,
      OrderBook.empty,
      System.currentTimeMillis(),
      new HdrHistogramReservoir())

    printSnapshot(histogram.getSnapshot)
  }

}
