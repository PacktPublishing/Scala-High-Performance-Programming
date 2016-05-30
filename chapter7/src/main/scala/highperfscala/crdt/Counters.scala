package highperfscala.crdt

case class CounterUpdate(i: Int)

case class GCounterState(uid: Int, counter: Int)

class StateBasedGCounter(
  val uid: Int,
  count: Int,
  otherCounters: Map[Int, Int]) {

  def value: Int = count + otherCounters.values.sum

  def update(
    change: CounterUpdate): (StateBasedGCounter, GCounterState) = {
    (new StateBasedGCounter(uid, count + change.i, otherCounters),
      GCounterState(uid, count))
  }

  def merge(other: GCounterState): StateBasedGCounter = {
    val newValue = other.counter max otherCounters.getOrElse(other.uid, 0)
    new StateBasedGCounter(uid, count, otherCounters.+(other.uid -> newValue))
  }
}

class OperationBasedCounter(count: Int) {

  def value: Int = count

  def update(change: CounterUpdate): (OperationBasedCounter, CounterUpdate) =
    new OperationBasedCounter(count + change.i) -> change

  def merge(operation: CounterUpdate): OperationBasedCounter =
    update(operation)._1

}

case class PNCounterState(
  incState: GCounterState,
  decState: GCounterState)

object StateBasedPNCounter {
  def newCounter(uid: Int): StateBasedPNCounter =
    new StateBasedPNCounter(
      new StateBasedGCounter(uid, 0, Map.empty),
      new StateBasedGCounter(uid, 0, Map.empty)
    )
}

class StateBasedPNCounter private(
  incCounter: StateBasedGCounter,
  decCounter: StateBasedGCounter) {

  def value = incCounter.value - decCounter.value

  def update(change: CounterUpdate): (StateBasedPNCounter, PNCounterState) = {
    val (newIncCounter, newDecCounter, stateUpdate) =
      change match {
        case CounterUpdate(c) if c >= 0 =>
          val (iC, iState) = incCounter.update(change)
          val dState = GCounterState(decCounter.uid, decCounter.value)
          (iC, decCounter, PNCounterState(iState, dState))
        case CounterUpdate(c) if c < 0 =>
          val (dC, dState) = decCounter.update(change)
          val iState = GCounterState(incCounter.uid, incCounter.value)
          (incCounter, dC, PNCounterState(iState, dState))
      }

    (new StateBasedPNCounter(newIncCounter, newDecCounter), stateUpdate)
  }

  def merge(other: PNCounterState): StateBasedPNCounter =
    new StateBasedPNCounter(
      incCounter.merge(other.incState),
      decCounter.merge(other.decState)
    )
}