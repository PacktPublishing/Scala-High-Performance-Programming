package highperfscala.free

import com.codahale.metrics.Snapshot
import org.slf4s.Logging

object GenericBenchmark extends Logging {
  private def infiniteCommands[A](sample: List[A]): Stream[A] =
    Stream.continually(sample.toStream).flatten

  private def generateCount[A](sample: List[A], count: Int): List[A] =
    infiniteCommands(sample).take(count).toList

  private def jvmWarmUp[A, B](
    sample: List[A],
    init: B,
    f: (MessageSentTimestamp, B, A) => B): Unit = {
    log.debug("Begin warm up")
    val commands = generateCount(sample, 100000)
    commands.foldLeft(init) {
      case (acc, a) =>
        f(MessageSentTimestamp(System.currentTimeMillis()), acc, a)
    }
    log.debug(s"End warm up")
  }

  def printSnapshot(s: Snapshot): Unit = println {
    s"""
       |Processed ${s.size} commands
       |mean latency: ${s.getMean} ms
       |median latency: ${s.getMedian} ms
       |75p latency: ${s.get75thPercentile()} ms
       |99p latency: ${s.get99thPercentile()} ms
       |99.9p latency: ${s.get999thPercentile()} ms
       |Maximum latency: ${s.getMax} ms
        """.stripMargin
  }

  case class MessagesPerSecond(value: Int) extends AnyVal
  case class BenchmarkIterationCount(value: Int) extends AnyVal
  case class MessageSentTimestamp(value: Long) extends AnyVal

  def runBenchmark[A, B](
    sampleMessages: List[A],
    mps: MessagesPerSecond,
    count: BenchmarkIterationCount,
    warmup: (B, (MessageSentTimestamp, B, A) => B),
    afterWarmUp: () => Unit,
    handleEvent: (MessageSentTimestamp, A) => Unit): Unit = {
    val totalMessageCount = mps.value * count.value

    Function.tupled(jvmWarmUp(
      sampleMessages, _: B, _: (MessageSentTimestamp, B, A) => B))(warmup)

    afterWarmUp()

    var messagesWithOffset =
      generateCount(sampleMessages, totalMessageCount)
        .grouped(mps.value)
        .toList.zipWithIndex
        .flatMap {
          case (secondBatch, sBatchIndex) =>
            val batchOffsetInMs = sBatchIndex * 1000
            val messageIntervalInMs = 1000.0 / mps.value
            secondBatch.zipWithIndex.map {
              case (command, commandIndex) =>
                val commandOffsetInMs =
                  Math.floor(messageIntervalInMs * commandIndex).toInt
                (command, batchOffsetInMs + commandOffsetInMs)
            }
        }

    val testStart = System.currentTimeMillis()

    while (messagesWithOffset.nonEmpty) {
      val (message, offsetInMs) = messagesWithOffset.head
      val shouldStart = testStart + offsetInMs

      while (shouldStart > System.currentTimeMillis()) {
        // keep the thread busy while waiting for the next batch to be sent
      }

      handleEvent(MessageSentTimestamp(shouldStart), message)
      messagesWithOffset = messagesWithOffset.tail
    }
  }
}
