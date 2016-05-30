package highperfscala.free

import scalaz.{Free, \/}
import language.higherKinds

class EitherFree[A[_], L, R](val free: Free[A, L \/ R]) {
  def map[RR](f: R => RR): EitherFree[A, L, RR] =
    new EitherFree(free.map(_.map(f)))
  def flatMap[RR](f: R => EitherFree[A, L, RR]): EitherFree[A, L, RR] =
    new EitherFree(free.flatMap(_.fold(l =>
      Free.point[A, L \/ RR](\/.left[L, RR](l)), z => f(z).free)))
}
