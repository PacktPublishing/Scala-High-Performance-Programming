package highperfscala.specialization

object UnexpectedAllocations {

  case class ShareCount(value: Int) extends AnyVal
  case class ExecutionCount(value: Int)

  class Container2[@specialized X, @specialized Y](x: X, y: Y)

  def shareCount = new Container2(ShareCount(1), 1)

  def executionCount = new Container2(ExecutionCount(1), 1)

  def ints = new Container2(1, 1)
}
