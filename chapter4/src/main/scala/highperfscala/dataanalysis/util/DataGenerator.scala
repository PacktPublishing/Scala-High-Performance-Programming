package highperfscala.dataanalysis.util

import highperfscala.dataanalysis._
import org.joda.time.DateTime

object DataGenerator {

  import java.io.File

  import org.scalacheck.Gen

  import scala.util.Random

  private def genExecution(time: DateTime): Gen[Execution] = for {
    bid <- Gen.chooseNum(1, 100)
    ask <- Gen.chooseNum(bid, bid+20)
    et = TimestampMinutes.fromDateTime(time)
  } yield Execution(et, AskPrice(ask), BidPrice(bid))

  def main(args: Array[String]): Unit = {

    val outputFile = new File(args(0))
    val minutes = args(1).toInt

    val startDt = DateTime.now().minusMinutes(minutes)

    val xs = for {
      i <- 0 to minutes
      time = startDt.plusMinutes(i)
      execCount = Random.nextInt(5) + 1
    } yield Gen.listOfN(execCount, genExecution(time)).sample.get

    DataCodec.write(xs.flatten.toList, outputFile)
  }

}
