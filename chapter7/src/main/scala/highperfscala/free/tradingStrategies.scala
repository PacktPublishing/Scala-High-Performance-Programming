package highperfscala.free

import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.locks.LockSupport

trait TradingStrategy {
  def makeTradingDecision(e: BboUpdated): Option[Either[Bid, Offer]]
}

class ProductionStrategy(counter: AtomicLong) extends TradingStrategy {
  def makeTradingDecision(e: BboUpdated): Option[Either[Bid, Offer]] = {
    val c = counter.getAndIncrement()
    c % 1000 == 0 match {
      case true =>
        Thread.sleep(10)
        Some(Right(e.offer))
      case false =>
        LockSupport.parkNanos(20000) // 0.02ms
        c % 3 == 0 match {
          case true => Some(Left(e.bid))
          case false => None
        }
    }
  }
}

