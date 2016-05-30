package highperfscala.orderbook

// API externalized from chapter 2

object Commands {
  sealed trait Command
  case class AddLimitOrder(i: CommandInstant, o: LimitOrder) extends Command
  case class CancelOrder(i: CommandInstant, id: OrderId) extends Command
}

object Events {
  sealed trait Event
  case class OrderExecuted(
    i: EventInstant, buy: Execution, sell: Execution) extends Event
  case class LimitOrderAdded(i: EventInstant) extends Event
  case class OrderCancelRejected(i: EventInstant, id: OrderId) extends Event
  case class OrderCanceled(i: EventInstant, id: OrderId) extends Event
}
