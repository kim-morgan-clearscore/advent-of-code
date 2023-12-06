package daySix

import scala.annotation.tailrec

final case class Race(timeAllowed: Long, recordDistance: Long) {
  def toAttempt(accelerationTime: Long): Attempt = {
    Attempt(
      speed = accelerationTime,
      remainingTime = timeAllowed - accelerationTime
    )
  }
}
final case class Attempt(speed: Long, remainingTime: Long) {
  def beatsRace(race: Race): Boolean =
    (speed * remainingTime) > race.recordDistance
}
object puzzle extends App {
  private val races =
    List(Race(52, 426), Race(94, 1374), Race(75, 1279), Race(94, 1216))

  private def raceToPassingAttempts(
      race: Race,
      accelerationTime: Long
  ): Long = {

    @tailrec
    def raceToPassingAttemptsHelper(
        accelerationTime: Long,
        acc: Long
    ): Long = {
      if (accelerationTime == race.timeAllowed) acc
      else {
        val attempt = race.toAttempt(accelerationTime)
        if (attempt.beatsRace(race))
          raceToPassingAttemptsHelper(accelerationTime + 1, acc + 1)
        else raceToPassingAttemptsHelper(accelerationTime + 1, acc)
      }
    }

    raceToPassingAttemptsHelper(accelerationTime, 0)
  }

  private val partOneSolution = races
    .map(raceToPassingAttempts(_, 1))
    .foldLeft(1)((acc, attempts) => acc * attempts.toInt)
  println(partOneSolution)

  private val partTwoSolution = {
    val race = Race(52947594, 426137412791216L)
    raceToPassingAttempts(race, 1)
  }

  println(partTwoSolution)

}
