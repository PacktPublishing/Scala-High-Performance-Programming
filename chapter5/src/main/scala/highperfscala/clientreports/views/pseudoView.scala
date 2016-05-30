package highperfscala.clientreports.views

sealed trait PseudoView[A] {
  def map[B](f: A => B): PseudoView[B]
  def toList: List[A]
}

final class InitialView[A](xs: List[A]) extends PseudoView[A] {
  def map[B](f: A => B): PseudoView[B] = new ComposedView[A, B](xs, f)
  def toList: List[A] = xs
}

final class ComposedView[A, B](xs: List[A], fa: A => B) extends PseudoView[B] {
  def map[C](f: B => C): PseudoView[C] = new ComposedView(xs, f.compose(fa))
  def toList: List[B] = xs.map(fa)
}

object PseudoView {
  def view[A, B](xs: List[A]): PseudoView[A] = new InitialView(xs)
}

object PseudoViewExample {

  def main(args: Array[String]): Unit = {
    println("PseudoView evaluation:")
    val listPseudoView = PseudoView.view(List(0, 1, 2)).map(i => {
      println(s"Adding one to $i")
      i + 1
    }).map(i => {
      println(s"Multiplying $i")
      i * 2
    })

    println("--- Converting PseudoView to List ---")
    println(listPseudoView.toList)
  }
}
