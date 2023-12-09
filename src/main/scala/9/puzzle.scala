package dayNine

import utils.inputReader.lines

object puzzle extends App {
  val linesOfSequences = lines("9.txt").map(_.split(" ").map(_.toInt).toList)
  def findDifferences(list: List[Int], index: Int): List[Int] = {
    if (index == list.length - 1) List.empty
    else (list(index + 1) - list(index)) +: findDifferences(list, index + 1)
  }
  def findDeepDifferences(
      list: List[Int]
  ): List[List[Int]] = {
    val differences = findDifferences(list, 0)
    if (differences == List.fill(differences.length)(0)) List(differences)
    else differences +: findDeepDifferences(differences)
  }

  val originalSequenceAndDeepDifferences =
    linesOfSequences.map(seq => seq +: findDeepDifferences(seq))

  val partOneSolution =
    originalSequenceAndDeepDifferences
      .map(_.foldRight(0)((diff, acc) => acc + diff.last))
      .sum

  println(partOneSolution)

  val partTwoSolution =
    originalSequenceAndDeepDifferences
      .map(_.foldRight(0)((diff, acc) => diff.head - acc))
      .sum

  println(partTwoSolution)

}
