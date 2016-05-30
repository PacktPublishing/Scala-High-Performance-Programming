package highperfscala.clientreports.streams

import org.joda.time.Instant

import scala.annotation.tailrec
import scala.util.Random

object MarkovChainEventGenerator {

  sealed trait Step
  case object GenerateBuy extends Step
  case object GenerateSell extends Step
  case object GenerateCancel extends Step
  case object GenerateExecution extends Step

  case class Weight(value: Int) extends AnyVal

  case class GeneratedWeight(value: Int) extends AnyVal

  case class StepTransitionWeights(
    buy: Weight,
    sell: Weight,
    cancel: Weight,
    execution: Weight) {

    def weightSum: Weight =
      Weight(buy.value + sell.value + cancel.value + execution.value)

    private val buyRange = 1 to buy.value

    private val sellRange = {
      val start = buy.value + 1
      start to start + sell.value - 1
    }
    private val cancelRange = {
      val start = buy.value + sell.value + 1
      start to start + cancel.value - 1
    }

    private val executionRange = {
      val start = buy.value + sell.value + cancel.value + 1
      start to start + execution.value - 1
    }

    private val allRanges =
      List(buyRange, sellRange, cancelRange, executionRange)

    def stepFrom(gw: GeneratedWeight): Step =
      allRanges.map(_.contains(gw.value)) match {
        case true :: false :: false :: false :: Nil => GenerateBuy
        case false :: true :: false :: false :: Nil => GenerateSell
        case false :: false :: true :: false :: Nil => GenerateCancel
        case false :: false :: false :: true :: Nil => GenerateExecution
        case unsupported => sys.error(
          s"Unsupported result for weight = $gw:  $unsupported")
      }
  }

  case class State(
    pendingOrders: Set[OrderId],
    step: Step)
  object State {
    private val clientIds: Vector[ClientId] =
      (1 to 100).map(i => ClientId(i)).toVector

    private val tickers: Vector[Ticker] = Vector(
      "FOO", "BAR", "XYZ", "RTY", "PLM").map(Ticker.apply)

    val initialBuy: (State, OrderBookEvent) = {
      val e = randomBuySubmitted()
      State(Set(e.id), GenerateBuy) -> e
    }

    val initialSell: (State, OrderBookEvent) = {
      val e = randomSellSubmitted()
      State(Set(e.id), GenerateSell) -> e
    }

    private def randomBuySubmitted() = BuyOrderSubmitted(
      EventInstant(Instant.now()), OrderId(Math.abs(Random.nextLong())),
      tickers(Random.nextInt(tickers.size)), Price(Random.nextInt(100) + 1),
      clientIds(Random.nextInt(clientIds.size)))

    private def randomSellSubmitted() = SellOrderSubmitted(
      EventInstant(Instant.now()), OrderId(Math.abs(Random.nextLong())),
      tickers(Random.nextInt(tickers.size)), Price(Random.nextInt(100) + 1),
      clientIds(Random.nextInt(clientIds.size)))

    private def randomCanceled(id: OrderId) = OrderCanceled(
      EventInstant(Instant.now()), id)

    private def randomExecuted(id: OrderId) = OrderExecuted(
      EventInstant(Instant.now()), id, Price(Random.nextInt(100) + 1))

    def nextState(
      weight: StepTransitionWeights => GeneratedWeight,
      stepToWeights: Map[Step, StepTransitionWeights],
      s: State): (State, OrderBookEvent) = {
      @tailrec
      def loop(): (State, OrderBookEvent) = {
        val transition = stepToWeights(s.step)
        val nextStep = transition.stepFrom(weight(transition))
        nextStep match {
          case GenerateBuy =>
            val e = randomBuySubmitted()
            s.copy(step = nextStep, pendingOrders = s.pendingOrders + e.id) -> e
          case GenerateSell =>
            val e = randomSellSubmitted()
            s.copy(step = nextStep, pendingOrders = s.pendingOrders + e.id) -> e
          case GenerateCancel if s.pendingOrders.nonEmpty =>
            val e = randomCanceled(s.pendingOrders.head)
            s.copy(step = nextStep, pendingOrders = s.pendingOrders - e.id) -> e
          case GenerateCancel if s.pendingOrders.isEmpty => loop()
          case GenerateExecution if s.pendingOrders.nonEmpty =>
            val e = randomExecuted(s.pendingOrders.head)
            s.copy(step = nextStep, pendingOrders = s.pendingOrders - e.id) -> e
          case GenerateExecution if s.pendingOrders.isEmpty => loop()
        }
      }
      loop()
    }
  }

  def main(args: Array[String]): Unit = {
    val stepToWeights = Map[Step, StepTransitionWeights](
      GenerateBuy -> StepTransitionWeights(
        Weight(10), Weight(25), Weight(40), Weight(40)),
      GenerateSell -> StepTransitionWeights(
        Weight(25), Weight(10), Weight(40), Weight(25)),
      GenerateCancel -> StepTransitionWeights(
        Weight(60), Weight(50), Weight(40), Weight(10)),
      GenerateExecution -> StepTransitionWeights(
        Weight(30), Weight(30), Weight(60), Weight(25)))

    val next = State.nextState(
      t => GeneratedWeight(Random.nextInt(t.weightSum.value) + 1),
      stepToWeights, _: State)

    Stream.iterate(State.initialBuy) { case (s, e) => next(s) }
      .take(5)
      .foreach { case (s, e) => println(s"State = $s\nEvent = $e")  }
  }
}
