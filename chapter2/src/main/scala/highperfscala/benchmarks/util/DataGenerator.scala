package highperfscala
package benchmarks
package util

object DataGenerator {

  import java.io.File

  import orderbook.Commands.{AddLimitOrder, CancelOrder, Command}
  import orderbook._
  import org.scalacheck.Gen

  import scala.util.Random

  /**
    * Generates fake historical data consisting of a list of commands to
    * be applied to an order book.
    */
  def main(args: Array[String]): Unit = {

    val outputFile = new File(args(0))
    val commandCount = args(1).toInt

    val orderedCommands = Random.shuffle(
      Gen.listOfN(commandCount / 2, LimitOrder.genLimitOrder).sample.get
        .map { order =>
          val nextCommand =
            if (cancelOrder()) CancelOrder(order.id)
            else AddLimitOrder(oppositeOrder(order))

          List(AddLimitOrder(order), nextCommand)
        })

    val commands = merge(orderedCommands, Nil)

    DataCodec.write(commands, outputFile)
  }

  /**
    * Randomly decide if an order should be canceled
    */
  private def cancelOrder(): Boolean = Random.nextInt(3) == 0

  private def oppositeOrder(o: LimitOrder): LimitOrder = o match {
    case BuyLimitOrder(_, price) =>
      SellLimitOrder(OrderId.genOrderId.sample.get, price)
    case SellLimitOrder(_, price) =>
      BuyLimitOrder(OrderId.genOrderId.sample.get, price)
  }

  /**
    * Merge a list of pairs, introducing randomness while making sure that
    * an order's complement comes later in the stream.
    */
  private def merge(ls: List[List[Command]], acc: List[Command]): List[Command] =
    ls match {
      case Nil => acc.reverse
      case List(c) :: rest => merge(rest, c :: acc)
      case List(c1, c2) :: rest =>
        merge(Random.shuffle(List(c2) :: rest), c1 :: acc)
    }

}
