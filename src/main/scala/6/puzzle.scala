package daySix

import dayFive.puzzle.time

final case class Race(timeAllowed: Long, recordDistance: Long) {
  def findNumberOfPassingTimes: Long = {
    //formula for solving quadratic equation
    val a = -1
    val b = timeAllowed
    val c = recordDistance * -1
    val max =
      Math.floor(((b * -1) - Math.sqrt((b * b) - (4 * a * c))) / (2 * a))
    val min =
      Math.ceil(((b * -1) + Math.sqrt((b * b) - (4 * a * c))) / (2 * a))
    max.toLong - min.toLong + 1
  }
}
object puzzle extends App {
  private val races =
    List(Race(52, 426), Race(94, 1374), Race(75, 1279), Race(94, 1216))

  private val partOneSolution = races
    .map(_.findNumberOfPassingTimes)
    .foldLeft(1)((acc, attempt) => acc * attempt.toInt)

  println(partOneSolution)

  private val partTwoSolution = {
    Race(52947594, 426137412791216L).findNumberOfPassingTimes
  }

  println(partTwoSolution)

  time { Race(52947594, 426137412791216L).findNumberOfPassingTimes }

}
