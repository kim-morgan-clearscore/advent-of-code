package twentyTwentyFour.day3

import utils.inputReader.lines

sealed trait Instruction
case class Multiplication(x: Int, y: Int) extends Instruction {
  val eval = this.x * this.y
}
case object Do extends Instruction
case object Dont extends Instruction

object Solution extends App {
  val linesOfText = lines("2024_3.txt")

  // It was completely unclear from the problem description and also annoying as hell that
  // the input had to be considered as one long block rather than separate, freestanding lines
  val input = linesOfText.foldLeft("")((acc, line) => acc + line)

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

  def parseInstruction(expression: String): List[Instruction] = {
    val combinedRegex = """mul\((\d{1,3}),\s*(\d{1,3})\)|do\(\)|don't\(\)""".r

    combinedRegex
      .findAllMatchIn(expression)
      .map { m =>
        if (m.matched.startsWith("mul")) {
          val a = m.group(1).toInt
          val b = m.group(2).toInt
          Multiplication(a, b)
        } else if (m.matched == "do()") {
          Do
        } else {
          Dont
        }
      }
      .toList
  }

  def removeBetweenDontAndDo(
      expressions: List[Instruction]
  ): List[Instruction] = {
    expressions
      .foldLeft((List.empty[Instruction], false)) {
        case ((result, insideBlock), current) =>
          current match {
            case Dont => (result :+ current, true)
            case Do if insideBlock =>
              (
                result :+ current,
                false
              )
            case _ if insideBlock =>
              (result, insideBlock)
            case _ =>
              (
                result :+ current,
                insideBlock
              )
          }
      }
      ._1
  }

  val partOneSolution =
    parseMultiplication(input).foldLeft(0)((acc, mult) => acc + mult.eval)
  println(partOneSolution)

  val partTwoSolution = removeBetweenDontAndDo(parseInstruction(input))
    .collect { case multiplication: Multiplication =>
      multiplication
    }
    .foldLeft(0)((acc, mult) => acc + mult.eval)
  println(partTwoSolution)

}
