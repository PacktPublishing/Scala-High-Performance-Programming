package highperfscala.dataanalysis

object ListVectorExperiment {

  def computeReturnsWithList(
    rollUp: MinuteRollUp,
    data: List[Midpoint]): List[Return] = {
    for {
      i <- (rollUp.value until data.size).toList
    } yield Return.fromMidpoint(data(i - rollUp.value), data(i))
  }

  def computeReturnsWithVector(
    rollUp: MinuteRollUp,
    data: Vector[Midpoint]): Vector[Return] = {
    for {
      i <- (rollUp.value until data.size).toVector
    } yield Return.fromMidpoint(data(i - rollUp.value), data(i))
  }

}
