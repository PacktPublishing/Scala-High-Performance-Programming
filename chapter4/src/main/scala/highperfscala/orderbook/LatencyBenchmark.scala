package highperfscala.orderbook

import java.io.File

import highperfscala.orderbook.util.DataCodec
import org.mpierce.metrics.reservoir.hdrhistogram.HdrHistogramReservoir
import util._

object LatencyBenchmark {

  sealed trait BookImplementation
  case object QueueImplementation extends BookImplementation
  case object LazyImplementation extends BookImplementation
  object BookImplementation {
    def fromString(s: String): Option[BookImplementation] =
      s.toLowerCase() match {
        case "lazy" => Some(LazyImplementation)
        case "queue" => Some(QueueImplementation)
        case _ => None
      }
  }

  def main(args: Array[String]): Unit = {

    val commandSample = DataCodec.read(new File(args(0)))
    val (commandsPerSecond, iterations) = (args(1).toInt, args(2).toInt)
    val bookImplementation = BookImplementation.fromString(args(3)).fold(
      sys.error(s"Unsupported book implementation = ${args(3)}"))(identity)

    val totalCommandCount = commandsPerSecond * iterations

    // Circular dependency with BookImplementation
    jvmWarmUp(commandSample, bookImplementation)

    val histogram = new HdrHistogramReservoir()

    var commandsWithOffset =
      generateCount(commandSample, totalCommandCount)
        .grouped(commandsPerSecond)
        .toList.zipWithIndex
        .flatMap {
          case (secondBatch, sBatchIndex) =>
            val batchOffsetInMs = sBatchIndex * 1000
            val commandIntervalInMs = 1000.0 / commandsPerSecond
            secondBatch.zipWithIndex.map {
              case (command, commandIndex) =>
                val commandOffsetInMs =
                  Math.floor(commandIntervalInMs * commandIndex).toInt
                (command, batchOffsetInMs + commandOffsetInMs)
            }
        }

    val systemEventTime = () => EventInstant.now()
    val testStart = System.currentTimeMillis()

    // This is terrible duplication
    bookImplementation match {
      case LazyImplementation =>
        var book = LazyCancelOrderBook.empty

        while (commandsWithOffset.nonEmpty) {
          val (command, offsetInMs) = commandsWithOffset.head
          val shouldStart = testStart + offsetInMs

          while (shouldStart > System.currentTimeMillis()) {
            // keep the thread busy while waiting for the next batch to be sent
          }

          book = LazyCancelOrderBook.handle(systemEventTime, book, command)._1
          val end = System.currentTimeMillis()
          // record latency
          histogram.update(end - shouldStart)

          commandsWithOffset = commandsWithOffset.tail
        }

        printSnapshot(histogram.getSnapshot)
      case QueueImplementation =>
        var book = QueueOrderBook.empty

        while (commandsWithOffset.nonEmpty) {
          val (command, offsetInMs) = commandsWithOffset.head
          val shouldStart = testStart + offsetInMs

          while (shouldStart > System.currentTimeMillis()) {
            // keep the thread busy while waiting for the next batch to be sent
          }

          book = QueueOrderBook.handle(systemEventTime, book, command)._1
          val end = System.currentTimeMillis()
          // record latency
          histogram.update(end - shouldStart)

          commandsWithOffset = commandsWithOffset.tail
        }

        printSnapshot(histogram.getSnapshot)
    }
  }

}
