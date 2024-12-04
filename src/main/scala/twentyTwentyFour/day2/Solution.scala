package twentyTwentyFour.day2

import utils.inputReader.lines

// NB: Part two solution does not work... don't look at it :')

sealed trait Direction
final case object Ascending extends Direction
final case object Descending extends Direction
final case object Same extends Direction

object Solution extends App {
  val linesOfText = lines("2024_2.txt")
  val linesOfInt = linesOfText.map(_.split(" ").map(_.toInt).toList)

  val test = List(
    List(7, 6, 4, 2, 1),
    List(1, 2, 7, 8, 9),
    List(9, 7, 6, 2, 1),
    List(1, 3, 2, 4, 5),
    List(8, 6, 4, 4, 1),
    List(1, 3, 6, 7, 9)
  )

  val edgeCases = List(
    List(48, 46, 47, 49, 51, 54, 56),
    List(1, 1, 2, 3, 4, 5),
    List(1, 2, 3, 4, 5, 5),
    List(5, 1, 2, 3, 4, 5),
    List(1, 4, 3, 2, 1),
    List(1, 6, 7, 8, 9),
    List(1, 2, 3, 4, 3),
    List(9, 8, 7, 6, 7),
    List(7, 10, 8, 10, 11),
    List(29, 28, 27, 25, 26, 25, 22, 20)
  )

  def isStrictlyAscending(nums: List[Int]): Boolean =
    nums
      .zip(nums.tail)
      .map { case (num1, num2) => num1 < num2 }
      .foldLeft(true)((acc, asc) => acc && asc)

  def isStrictlyDescending(nums: List[Int]): Boolean =
    nums
      .zip(nums.tail)
      .map { case (num1, num2) => num1 > num2 }
      .foldLeft(true)((acc, desc) => acc && desc)

  def safeDistances(nums: List[Int]): Boolean = {
    nums match {
      case ::(head, next) => {
        val distance = next.headOption.fold(1)(n => Math.abs(n - head))
        if (distance > 0 && distance < 4) true && safeDistances(next)
        else false
      }
      case Nil => true
    }
  }

  def isSafe(nums: List[Int]): Boolean = {
    if (isStrictlyAscending(nums) || isStrictlyDescending(nums))
      safeDistances(nums)
    else false
  }

  def checkWithIndexIgnored(nums: List[Int], index: Int): Boolean = {
    val (firstPart, secondPart) = nums.splitAt(index)
    val newNums = firstPart ++ secondPart.tail
    isSafe(newNums)
  }

  def checkForSafeRemovable(nums: List[Int]): Boolean = {

    val directions = nums
      .zip(nums.tail)
      .map { case (num1, num2) =>
        if (num1 > num2) Descending else if (num2 > num1) Ascending else Same
      }
      .zipWithIndex

    val diffs = nums
      .zip(nums.tail)
      .map { case (num1, num2) => Math.abs(num1 - num2) }
      .zipWithIndex

    val hasRemovableAscending =
      if (directions.map(_._1).count(_ == Ascending) == 1) {
        val indexToIgnore =
          directions.find { case (d, _) => d == Ascending }.get._2
        checkWithIndexIgnored(nums, indexToIgnore) || checkWithIndexIgnored(
          nums,
          indexToIgnore + 1
        )
      } else false

    val hasRemovableDescending =
      if (directions.map(_._1).count(_ == Descending) == 1) {
        val indexToIgnore =
          directions.find { case (d, _) => d == Descending }.get._2
        checkWithIndexIgnored(nums, indexToIgnore) || checkWithIndexIgnored(
          nums,
          indexToIgnore + 1
        )
      } else false

    val hasRemovableSame = if (directions.map(_._1).count(_ == Same) == 1) {
      val indexToIgnore = directions.find { case (d, _) => d == Same }.get._2
      checkWithIndexIgnored(nums, indexToIgnore)
    } else false

    val hasRemovableDiff =
      if (diffs.count { case (diff, i) => diff == 0 || diff > 4 } == 1) {
        val indexToIgnore = diffs.find { case (d, _) => d == 0 || d > 4 }.get._2
        val (firstPart, secondPart) = nums.splitAt(indexToIgnore)
        val newNums = firstPart ++ secondPart.tail
        isSafe(newNums)
      } else false

    hasRemovableSame || hasRemovableDescending || hasRemovableAscending || hasRemovableDiff
  }

  def isSafeWithRemovable(nums: List[Int]): Boolean =
    isSafe(nums) || checkForSafeRemovable(nums)

  val numberOfSafe = linesOfInt.map(isSafe).filter(identity).length
  println(numberOfSafe)

  val numberOfSafeWithRemovable =
    linesOfInt.map(isSafeWithRemovable).filter(identity).length
  println(numberOfSafeWithRemovable)

  //for debugging
//  println(
//    linesOfInt
//      .map(isSafe)
//      .zipWithIndex
//      .zip(linesOfInt.map(isSafeWithRemovable).zipWithIndex)
//      .filter { case ((b1, _), (b2, _)) =>
//        b1 != b2
//      }
//      .map { case ((_, intVal), _) =>
//        intVal
//      }
//      .map(int => linesOfInt(int))
//  )

}
