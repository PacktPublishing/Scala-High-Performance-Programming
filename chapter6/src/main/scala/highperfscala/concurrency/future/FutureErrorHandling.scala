package highperfscala.concurrency.future

import scala.concurrent.Future
import scala.util.{Failure, Success}
import scalaz.{\/-, \/}

object FutureErrorHandling {
  def main(args: Array[String]): Unit = {
    implicit val context = scala.concurrent.ExecutionContext.global
    def failedTransform(): Unit = {
      Future("not-an-integer").map(_.toInt).map(i => {
        println("Multiplying")
        i * 2
      })
      Thread.sleep(1000)
    }

    def recoverWith(): Unit = {
      Future("not-an-integer").map(_.toInt).recover {
        case _: NumberFormatException => -2
      }.map(i => {
        println("Multiplying")
        i * 2
      }).onComplete {
        case Success(i) => println(s"Multiplication result = $i")
        case Failure(e) => println(s"Failed due to ${e.getMessage}")
      }
      Thread.sleep(1000)
    }

    def disjunction(): Unit = {
      scalaz.\/.right[Throwable, Int](1).map(_ * 2)

    }

    recoverWith()
  }
}
