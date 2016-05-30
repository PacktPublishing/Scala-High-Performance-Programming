package highperfscala.concurrency.future

object FutureExample {

  def main(args: Array[String]): Unit = {
    import scala.concurrent.Future
    implicit val context = scala.concurrent.ExecutionContext.global
    Future(1).map(_ + 1).filter(_ % 2 == 0).foreach(println)

    Future(1).map(i => {
      println(Thread.currentThread().getName)
      i + 1
    }).filter(i => {
      println(Thread.currentThread().getName)
      i % 2 == 0
    }).foreach(println)
  }
}
