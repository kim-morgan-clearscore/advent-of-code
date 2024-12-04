package dayOne

import utils.inputReader.lines

object puzzleTwo extends App {
  private val linesOfText = lines("1.txt")

  private val oneToNine =
    List(One, Two, Three, Four, Five, Six, Seven, Eight, Nine)

  def findFirstDigit(string: String): Option[Digit] = {
    oneToNine.find(_.representations.exists(string.startsWith(_)))
  }

  def getDigits(string: String): List[Digit] = {
    if (string == "") List.empty
    else
      findFirstDigit(string) match {
        case Some(digit) =>
          digit :: getDigits(string.takeRight(string.length - 1))
        case None => getDigits(string.takeRight(string.length - 1))
      }
  }

  def calibrationValue(listOfDigits: List[Digit]): Int = {
    (listOfDigits.head.value * 10) + listOfDigits.last.value
  }

  private val digits = linesOfText.map(getDigits)
  private val calibrationValues = digits.map(calibrationValue)

  private val solution = calibrationValues.sum
  println(solution)
}
