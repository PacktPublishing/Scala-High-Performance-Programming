package highperfscala.clientreports.views

object ViewDemo {

  def main(args: Array[String]): Unit = {
    println("List evaluation:")
    val evens = List(0, 1, 2, 3, 4, 5).map(i => {
      println(s"Adding one to $i")
      i + 1
    }).filter(i => {
      println(s"Filtering $i")
      i % 2 == 0
    })

    println("--- Printing first two even elements ---")
    println(evens.take(2))

    println("View evaluation:")
    val evensView = List(0, 1, 2, 3, 4, 5).view.map(i => {
      println(s"Adding one to $i")
      i + 1
    }).filter(i => {
      println(s"Filtering $i")
      i % 2 == 0
    })

    println("--- Printing first two even elements ---")
    println(evensView.take(2).toList)
  }
}
