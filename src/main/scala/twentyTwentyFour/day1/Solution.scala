package twentyTwentyFour.day1

import utils.inputReader.lines

object Solution extends App {
  val linesOfText = lines("2024_1.txt")

  val pairs = linesOfText.map(_.split("\\s{3}").map(_.toInt).toList)

  val listOne = pairs.map(_.head).sorted
  val listTwo = pairs.map(_.last).sorted

  val distances = listOne
    .zip(listTwo)
    .map { case (numOne, numTwo) =>
      Math.abs(numOne - numTwo)
    }
    .sum

  println(distances)

  val similarityScore = listOne.map(num => listTwo.count(_ == num) * num).sum

  println(similarityScore)
}
