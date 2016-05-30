package highperfscala.free

import highperfscala.free.GenericBenchmark.MessageSentTimestamp

import scala.concurrent.duration._
import scala.language.{higherKinds, postfixOps}
import scalaz.concurrent.Task
import scalaz.{-\/, \/-, ~>}

object BboUpdatedPipeline {

  sealed trait BboProcessingFailure
  object BboProcessingFailure {
    def enrichmentFailure(t: Ticker): BboProcessingFailure =
      EnrichmentFailure(t)
    def journalingFailure: BboProcessingFailure = JournalingFailure
    def tradeAuthorizationFailure: BboProcessingFailure = TradeAuthorizationFailure
  }

  case class EnrichmentFailure(t: Ticker) extends BboProcessingFailure
  case object JournalingFailure extends BboProcessingFailure
  case object TradeAuthorizationFailure extends BboProcessingFailure

  import BboProcessingFailure._

  def enrichEvent(e: BboUpdated): BboUpdated = e
  def journalEvent(e: BboUpdated): Unit = ()
  def performPreTradeBalanceChecks(e: BboUpdated): Unit = ()
  def sendTradingDecision(d: Either[Bid, Offer]): Unit = ()

  def strategyPipeline(
    recordProcessingLatency: ProcessingLatencyMs => Unit,
    s: TradingStrategy,
    ts: MessageSentTimestamp,
    e: BboUpdated): Unit = {
    val enriched = enrichEvent(e)
    journalEvent(enriched)
    performPreTradeBalanceChecks(enriched)
    val d = s.makeTradingDecision(enriched)
    d.foreach(sendTradingDecision)
    recordProcessingLatency(ProcessingLatencyMs(
      System.currentTimeMillis() - ts.value))
  }

  private val pipeline = for {
    enriched <- StartWith(enrichEvent) within (8 millis) orElse (e =>
      enrichmentFailure(e.ticker))
    _ <- Step(journalEvent(enriched)) within (9 millis) orElse
      journalingFailure
    _ <- Step(performPreTradeBalanceChecks(enriched)) within (10 millis) orElse
      tradeAuthorizationFailure
    decision <- MakeTradingDecision(enriched)
  } yield decision

  case class PipelineState(
    ts: MessageSentTimestamp,
    strategy: TradingStrategy,
    event: BboUpdated)

  private def logFailure(f: BboProcessingFailure): Unit = ()
  private def logException(e: Throwable): Unit = ()

  private def hasProcessingTimeExpired(
    ts: MessageSentTimestamp, l: LimitMs): Boolean =
    System.currentTimeMillis() - ts.value >= l.value

  def runWithFoldInterpreter(
    recordProcessingLatency: ProcessingLatencyMs => Unit,
    strategy: TradingStrategy,
    ts: MessageSentTimestamp,
    e: BboUpdated): Unit = {
    val (_, decision) = pipeline.free.foldRun(
      PipelineState(ts, strategy, e)) {
      case (state, StartProcessing(whenActive, whenExpired, limitMs)) =>
        state -> (hasProcessingTimeExpired(state.ts, limitMs) match {
          case true => whenExpired(e)
          case false => whenActive(e)
        })
      case (state, Timed(whenActive, whenExpired, limitMs)) =>
        state -> (hasProcessingTimeExpired(state.ts, limitMs) match {
          case true => whenExpired()
          case false => whenActive()
        })
      case (state, TradingDecision(runStrategy)) =>
        state -> runStrategy(state.strategy)
    }

    decision.fold(logFailure, {
      case Some(order) =>
        sendTradingDecision(order)
        recordProcessingLatency(ProcessingLatencyMs(
          System.currentTimeMillis() - ts.value))
      case None =>
        recordProcessingLatency(ProcessingLatencyMs(
          System.currentTimeMillis() - ts.value))
    })
  }

  private def thunkToTask(ps: PipelineState): Thunk ~> Task = new (Thunk ~> Task) {
    def apply[B](t: Thunk[B]): Task[B] = t match {
      case StartProcessing(whenActive, whenExpired, limitMs) => Task.suspend(
        hasProcessingTimeExpired(ps.ts, limitMs) match {
          case true => Task.now(whenExpired(ps.event))
          case false => Task.now(whenActive(ps.event))
        })
      case Timed(whenActive, whenExpired, limitMs) => Task.suspend(
        hasProcessingTimeExpired(ps.ts, limitMs) match {
          case true => Task.now(whenExpired())
          case false => Task.now(whenActive())
        })
      case TradingDecision(runStrategy) =>
        Task.fork(Task.now(runStrategy(ps.strategy)))
    }
  }

  def runWithTaskInterpreter(
    recordProcessingLatency: ProcessingLatencyMs => Unit,
    s: TradingStrategy,
    ts: MessageSentTimestamp,
    e: BboUpdated) = {
    pipeline.free.foldMap(thunkToTask(PipelineState(ts, s, e)))
      .unsafePerformAsync {
        case -\/(ex) => logException(ex)
        case \/-(\/-(decision)) =>
          decision.foreach(sendTradingDecision)
          recordProcessingLatency(ProcessingLatencyMs(
            System.currentTimeMillis() - ts.value))
        case \/-(-\/(failure)) => logFailure(failure)
      }
  }
}
