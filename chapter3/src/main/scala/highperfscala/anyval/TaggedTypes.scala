package highperfscala.anyval

import scala.util.Random
import scalaz.{@@, Tag}

object TaggedTypes {

  sealed trait PriceTag

  type Price = BigDecimal @@ PriceTag

  object Price {
    def newPrice(p: BigDecimal): Price =
      Tag[BigDecimal, PriceTag](p)

    def lowerThan(a: Price, b: Price): Boolean =
      Tag.unwrap(a) < Tag.unwrap(b)
  }

  def newPriceArray(count: Int): Array[Price] = {
    val a = new Array[Price](count)
    for (i <- 0 until count) {
      a(i) = Price.newPrice(BigDecimal(Random.nextInt()))
    }
    a
  }

}
