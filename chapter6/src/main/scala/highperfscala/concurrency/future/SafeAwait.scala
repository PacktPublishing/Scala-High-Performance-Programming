package highperfscala.concurrency.future

import java.util.concurrent.TimeoutException

import scala.concurrent.duration.Duration
import scala.concurrent.{Awaitable, Await}
import scala.util.{Failure, Success, Try}

  object SafeAwait {
    def result[T](
      awaitable: Awaitable[T],
      atMost: Duration): Option[T] = Try(Await.result(awaitable, atMost)) match {
      case Success(t) => Some(t)
      case Failure(_: TimeoutException) => None
      case Failure(e) => throw e
    }
  }
