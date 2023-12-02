package dayOne

import utils.inputReader.lines

import scala.io.Source

object puzzleOne extends App {
  private val linesOfText = lines("puzzleOneCalibrationDocument.txt")

  def stripToListOfDigits(string: String): List[Char] =
    string.toList.filter(_.isDigit)

  def calibrationValue(listOfDigits: List[Char]): Int = {
    (listOfDigits.head.asDigit * 10) + listOfDigits.last.asDigit
  }

  private val digits = linesOfText.map(stripToListOfDigits)
  private val calibrationValues = digits.map(calibrationValue)
  private val solution = calibrationValues.sum
  println(solution)
}
