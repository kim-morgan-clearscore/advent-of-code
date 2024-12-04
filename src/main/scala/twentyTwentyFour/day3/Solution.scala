package twentyTwentyFour.day3

import utils.inputReader.lines

case class Multiplication(x: Int, y: Int) {
  val eval = this.x * this.y
}

object Solution extends App {
  val linesOfText = lines("2024_3.txt")
  val test =
    "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

  def parseMultiplication(expression: String): List[Multiplication] = {
    val multiplicationRegex = """mul\((\d{1,3}),\s*(\d{1,3})\)""".r
    multiplicationRegex
      .findAllMatchIn(expression)
      .map { m =>
        Multiplication(m.group(1).toInt, m.group(2).toInt)
      }
      .toList
  }

  def processInstructions(expression: String): String = {
    val dontIndex = expression.indexOf("don't()")
    if (dontIndex < 0) expression
    else {
      val keep = expression.substring(0, dontIndex)
      val process = expression.substring(dontIndex + "don't()".length)
      val doIndex = process.indexOf("do()")
      val continue =
        if (doIndex >= 0) process.substring(doIndex + "do()".length)
        else process
      keep + processInstructions(continue)
    }
  }

  val partOneSolution = linesOfText
    .map(parseMultiplication)
    .map(mults => mults.foldLeft(0)((acc, mult) => acc + mult.eval))
    .sum

  println(partOneSolution)

  val partTwoSolution = linesOfText
    .map(processInstructions)
    .map(parseMultiplication)
    .map(mults => mults.foldLeft(0)((acc, mult) => acc + mult.eval))
    .sum

  println(partTwoSolution)
// not correct

}
