package highperfscala.concurrency.task

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}
import scalaz.concurrent.Task
import scalaz.{-\/, \/, \/-}

object TaskExample {

  def createAndRunTask(): Unit = {
    val t = Task {
      println("Computing the answer...")
      Thread.sleep(2000)
      40 + 2
    }

    t.unsafePerformAsync {
      case \/-(answer) => println("The answer is " + answer)
      case -\/(ex) => println("Failed to compute the answer: " + ex)
    }

    println("Waiting for the answer")
  }

  object CallbackAPI {

    import scala.concurrent.ExecutionContext.Implicits.global

    def doCoolThings[A](a: => A, f: (Throwable \/ A) => Unit): Unit =
      Future(a).onComplete {
        case Failure(ex) => f(-\/(ex))
        case Success(res) => f(\/-(res))
      }
  }

  def doCoolThingsToTask[A](a: => A): Task[A] =
    Task.async { f =>
      CallbackAPI.doCoolThings[A](a, res => f(res))
    }

  def futureToTask[A](future: Future[A])(implicit ec: ExecutionContext): Task[A] =
    Task.async { f =>
      future.onComplete {
        case Success(res) => f(\/-(res))
        case Failure(ex) => f(-\/(ex))
      }
    }

  def taskToFuture[A](t: Task[A]): Future[A] = {
    val p = Promise[A]()
    t.unsafePerformAsync {
      case \/-(a) => p.success(a)
      case -\/(ex) => p.failure(ex)
    }
    p.future
  }

  def mapFuture() = {
    import scala.concurrent.ExecutionContext.Implicits.global
    println("Main thread: " + Thread.currentThread.getName)
    val f = Future {
      println("First execution: " + Thread.currentThread.getName)
      40
    }
    f.map { i =>
      println("Second execution: " + Thread.currentThread.getName)
      i + 2
    }
    Await.result(f, Duration("1 second"))
  }

  def flatMapFuture() = {
    import scala.concurrent.ExecutionContext.Implicits.global
    println("Main thread: " + Thread.currentThread.getName)
    val f = Future {
      println("First execution: " + Thread.currentThread.getName)
      40
    }
    f.flatMap { i => Future {
      println("Second execution: " + Thread.currentThread.getName)
      i + 2
    }
    }
    Await.result(f, Duration("1 second"))
  }

  def mapTask() = {
    println("Main thread: " + Thread.currentThread.getName)
    val f = Task {
      println("First execution: " + Thread.currentThread.getName)
      40
    }
    f.map { i =>
      println("Second execution: " + Thread.currentThread.getName)
      i + 2
    }.unsafePerformSync
  }

  def flatMapTask() = {
    println("Main thread: " + Thread.currentThread.getName)
    val f = Task {
      println("First execution: " + Thread.currentThread.getName)
      40
    }
    f.flatMap { i => Task {
      println("Second execution: " + Thread.currentThread.getName)
      i + 2
    }
    }.unsafePerformSync
  }

}
