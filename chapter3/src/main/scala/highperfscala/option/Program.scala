package highperfscala.option


class Program[Sig <: OptionSig : OptionOps] extends App {

  val ops = OptionOps[Sig]
  import ops._

  // a little dance to derive our Show instance
  import scalaz.std.anyVal.intInstance
  val showOptOptInt = {
    implicit val showOptInt = OptionShow[Sig].optionShow[Int]
    OptionShow[Sig].optionShow[Sig#Option[Int]]
  }

  // scalaz's syntax tricks are awesome
  import showOptOptInt.showSyntax._

  val optOpt = some(some(42))

  println("optOpt: " + optOpt.shows)

  val optNone = some(none)

  println("optNone: " + optNone.shows)

}