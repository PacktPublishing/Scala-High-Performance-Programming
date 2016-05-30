package highperfscala
package benchmarks

import java.io.File
import highperfscala.orderbook.Commands.Command
import highperfscala.orderbook.OrderBook
import org.mpierce.metrics.reservoir.hdrhistogram.HdrHistogramReservoir
import util._

import scala.annotation.tailrec

object FinalLatencyBenchmark {

  case class CommandsPerSecond(value: Int) extends AnyVal
  case class BenchmarkIterationCount(value: Int) extends AnyVal
  case class CommandSentTimestamp(value: Long) extends AnyVal

  def runBenchmark(
    sampleCommands: List[Command],
    cps: CommandsPerSecond,
    count: BenchmarkIterationCount): Unit = {
    val totalCommandCount = cps.value * count.value

    jvmWarmUp(sampleCommands)

    @tailrec
    def sendCommands(
      xs: List[(Command, Int)],
      ob: OrderBook,
      testStart: Long,
      histogram: HdrHistogramReservoir): (OrderBook, HdrHistogramReservoir) =
      xs match {
        case head :: tail =>
          val (command, offsetInMs) = head
          val shouldStart = testStart + offsetInMs

          while (shouldStart > System.currentTimeMillis()) {
            // keep the thread busy while waiting for the next batch to be sent
          }

          val newBook = OrderBook.handle(ob, command)._1
          val operationEnd = System.currentTimeMillis()
          histogram.update(operationEnd - shouldStart)

          sendCommands(tail, newBook, testStart, histogram)
        case Nil => (ob, histogram)
      }

    val (_, histogram) = sendCommands(
      generateCount(sampleCommands, totalCommandCount)
        .grouped(cps.value)
        .toList.zipWithIndex
        .flatMap {
          case (secondBatch, sBatchIndex) =>
            val batchOffsetInMs = sBatchIndex * 1000
            val commandIntervalInMs = 1000.0 / cps.value
            secondBatch.zipWithIndex.map {
              case (command, commandIndex) =>
                val commandOffsetInMs =
                  Math.floor(commandIntervalInMs * commandIndex).toInt
                (command, batchOffsetInMs + commandOffsetInMs)
            }
        },
      OrderBook.empty,
      System.currentTimeMillis(),
      new HdrHistogramReservoir())

    printSnapshot(histogram.getSnapshot)
  }

  def main(args: Array[String]): Unit = {
    runBenchmark(DataCodec.read(new File(args(0))),
      CommandsPerSecond(args(1).toInt),
      BenchmarkIterationCount(args(2).toInt))
  }
}
