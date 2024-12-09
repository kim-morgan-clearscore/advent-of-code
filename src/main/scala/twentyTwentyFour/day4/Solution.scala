package twentyTwentyFour.day4

import utils.inputReader.lines

object Solution extends App {
  val linesOfText = lines("2024_4.txt")
  val xmas = "XMAS"
  val smax = xmas.reverse

  val test = List(
    "MMMSXXMASM",
    "MSAMXMSMSA",
    "AMXSXMAAMM",
    "MSAMASMSMX",
    "XMASAMXAMM",
    "XXAMMXXAMA",
    "SMSMSASXSS",
    "SAXAMASAAA",
    "MAMMMXMMMM",
    "MXMXAXMASX"
  )

  val grid = test.map(_.zipWithIndex).zipWithIndex
  println(grid)

  val smallTest = List("..X...", ".SAMX.", ".A..A.", "XMAS.S", ".X....")

//  def findMatchesInWindow(window: List[String]) = {
//    println(window)
//    val horizontal = window.count(_ == xmas)
//    println("horizontal: " + horizontal)
//    val backwards = window.count(_ == smax)
//    println("backwards: " + backwards)
//    val verticallyflipped = window.transpose.map(_.mkString)
//    val verticalforward = verticallyflipped.count(_ == xmas)
//    println("verticalforward: " + verticalforward)
//    val verticalBackward = verticallyflipped.count(_ == smax)
//    println("verticalBackward: " + verticalBackward)
//    // Right Diagonal Forward (top-left to bottom-right)
//    val rightDiagonalForward =
//      if (Range(0, 4).forall(i => window(i)(i) == xmas(i))) 1
//      else 0
//
//    println("rightDiagonalForward: " + rightDiagonalForward)
//    // Right Diagonal Backward (bottom-left to top-right)
//    val rightDiagonalBackward =
//      if (Range(0, 4).forall(i => window(i)(i) == smax(i))) 1
//      else 0
//
//    println("rightDiagonalBackward: " + rightDiagonalBackward)
//    // Left Diagonal Forward (top-right to bottom-left)
//    val leftDiagonalForward =
//      if (Range(0, 4).forall(i => window(i)(3 - i) == xmas(i))) 1
//      else 0
//
//    println("leftDiagonalForward: " + leftDiagonalForward)
//    // Left Diagonal Backward (bottom-right to top-left)
//    val leftDiagonalBackward =
//      if (Range(0, 4).forall(i => window(i)(3 - i) == smax(i))) 1
//      else 0
//
//    println("leftDiagonalBackward: " + leftDiagonalBackward)
//    horizontal + backwards + verticalforward + verticalBackward + rightDiagonalForward + rightDiagonalBackward + leftDiagonalForward + leftDiagonalBackward
//  }

  val window = smallTest
    .map(_.sliding(4).map(_.mkString).toList)
    .transpose
    .map(_.sliding(4).toList)
    .flatten

//  println(window.map(findMatchesInWindow).sum)
//  println("XMASXMAS")

//  """XMAS
//    |XMAS
//    |XMAS
//    |XMAS
//    |""".stripMargin

//  val x = List("XXAM", "SSMM", "XAAM", "SSMM").transpose.map(_.mkString)
//
//  println(x)
//  println(findMatchesInWindow(List("SLLX", "LAML", "LAML", "SLLX")))
//  println(findMatchesInWindow(List("XMAS", "XMAS", "XMAS", "XMAS")))
//
//  """
//    |XMAS
//    |XMAS
//    |XMAS
//    |XMAS
//    |""".stripMargin
}
