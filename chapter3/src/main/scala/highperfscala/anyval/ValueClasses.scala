package highperfscala.anyval

import scala.util.Random

object ValueClasses {

  case class Price(value: BigDecimal) extends AnyVal {

    def lowerThan(p: Price): Boolean = this.value < p.value

  }

  case class OrderId(value: Long) extends AnyVal

  def printInfo(p: Price, oId: OrderId): Unit ={
    println(s"Price: ${p.value}, ID: ${oId.value}")
  }

  def newPriceArray(count: Int): Array[Price] = {
    val a = new Array[Price](count)
    for(i <- 0 until count){
      a(i) = Price(BigDecimal(Random.nextInt()))
    }
    a
  }

}
