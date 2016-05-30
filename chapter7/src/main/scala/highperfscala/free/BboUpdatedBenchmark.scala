package highperfscala.free

import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

import highperfscala.free.GenericBenchmark.{BenchmarkIterationCount, MessageSentTimestamp, MessagesPerSecond}
import org.mpierce.metrics.reservoir.hdrhistogram.HdrHistogramReservoir

import scala.language.postfixOps

object BboUpdatedBenchmark {

  private def generateDeterministicSample(): List[BboUpdated] = {
    val t = Ticker("XYZ")
    (1 to 5000).foldLeft(List.empty[BboUpdated]) {
      case (acc, i) =>
        val event = acc.headOption match {
          case Some(h) => i % 2 == 0 match {
            case true => BboUpdated(
              t, Bid(h.bid.value + 0.41), Offer(h.bid.value + 0.29))
            case false => BboUpdated(
              t, Bid(h.bid.value - 0.35), Offer(h.bid.value - 0.25))
          }
          case None => BboUpdated(t, Bid(22.45), Offer(23.51))
        }
        (event.bid.value >= event.offer.value match {
          case true => event.copy(offer = Offer(event.bid.value + 0.47))
          case false => event
        }) :: acc
    }
  }


  sealed trait ExecutionStrategy
  case object WithoutFree extends ExecutionStrategy
  case object WithThunk extends ExecutionStrategy
  case object WithTask extends ExecutionStrategy

  def main(args: Array[String]): Unit = {
    val histogram = new HdrHistogramReservoir()
    val queue = new LinkedBlockingQueue[(MessageSentTimestamp, BboUpdated)](100)
    val isRecording = new AtomicBoolean(false)
    val isDoneProcessing = new AtomicBoolean(false)

    if (args.length != 3) sys.error("Missing required args")
    val mps = MessagesPerSecond(args(0).toInt)
    val bic = BenchmarkIterationCount(args(1).toInt)
    val executionStrategy = args(2) match {
      case "without-free" => WithoutFree
      case "with-thunk" => WithThunk
      case "with-task" => WithTask
      case unsupported => sys.error(
        s"Unsupported execution strategy: $unsupported")
    }

    {
      val eventThread = new Thread(new Runnable {
        val count = new AtomicLong(0)
        val strategy = new ProductionStrategy(new AtomicLong(0))
        def run(): Unit = while (true) {
          def updateHistogram(l: ProcessingLatencyMs): Unit =
            if (isRecording.get()) histogram.update(l.value)

          Option(queue.poll(5, TimeUnit.SECONDS)) match {
            case Some((ts, e)) => executionStrategy match {
              case WithoutFree => BboUpdatedPipeline.strategyPipeline(
                updateHistogram, strategy, ts, e)
              case WithThunk => BboUpdatedPipeline.runWithFoldInterpreter(
                updateHistogram, strategy, ts, e)
              case WithTask => BboUpdatedPipeline.runWithTaskInterpreter(
                updateHistogram, strategy, ts, e)
            }
            case None => isDoneProcessing.set(true)
          }
        }
      })
      eventThread.setDaemon(true)
      eventThread.start()
    }

    def afterWarmUp(): Unit = {
      while (!isDoneProcessing.get()) Thread.sleep(1000)

      isRecording.set(true)
      isDoneProcessing.set(false)
    }

    GenericBenchmark.runBenchmark[BboUpdated, Option[Either[Bid, Offer]]](
      generateDeterministicSample(), mps, bic,
      (None, (ts, acc, e) => {
        queue.put((ts, e))
        None
      }),
      afterWarmUp,
      (ts, e) => queue.offer((ts, e)))

    while (!isDoneProcessing.get()) Thread.sleep(1000)

    GenericBenchmark.printSnapshot(histogram.getSnapshot)
  }

}
