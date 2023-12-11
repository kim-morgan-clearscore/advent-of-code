package dayEleven

import utils.inputReader.lines
import utils.timeComputation.time

case class Galaxy(x: Int, y: Int)

object puzzle extends App {
  val linesOfText = lines("11.txt")

  def rowsToCosmicallyExpand(rows: List[String], index: Int = 0): List[Int] = {
    if (index == rows.length - 1) List.empty
    else if (rows(index) == "." * rows(index).length)
      index +: rowsToCosmicallyExpand(rows, index + 1)
    else rowsToCosmicallyExpand(rows, index + 1)
  }
  def columnsToCosmicallyExpand(
      rows: List[String],
      index: Int = 0
  ): List[Int] = {
    if (index == rows.head.length - 1) List.empty
    else if (rows.filter(_(index) == '.').length == rows.length)
      index +: columnsToCosmicallyExpand(rows, index + 1)
    else columnsToCosmicallyExpand(rows, index + 1)
  }

  def galaxyWithCosmicExpansion(
      x: Int,
      y: Int,
      rowsToExpand: List[Int],
      columnsToExpand: List[Int],
      expansionFactor: Int
  ): Galaxy = {
    val newXCoord =
      x + (columnsToExpand.filter(x > _).length * (expansionFactor - 1))
    val newYCoord =
      y + (rowsToExpand.filter(y > _).length * (expansionFactor - 1))
    Galaxy(x = newXCoord, y = newYCoord)
  }

  def parseGalaxies(
      rows: List[String],
      index: Int = 0,
      expansionFactor: Int
  ): List[Galaxy] = {
    val rowsToExpand = rowsToCosmicallyExpand(rows)
    val columnsToExpand = columnsToCosmicallyExpand(rows)
    val galaxiesAtIndex = rows(index).zipWithIndex
      .filter(_._1 == '#')
      .map(element =>
        galaxyWithCosmicExpansion(
          x = element._2,
          y = index,
          rowsToExpand = rowsToExpand,
          columnsToExpand = columnsToExpand,
          expansionFactor = expansionFactor
        )
      )
      .toList
    if (index == rows.length - 1) galaxiesAtIndex
    else
      galaxiesAtIndex ++ parseGalaxies(
        rows,
        index + 1,
        expansionFactor = expansionFactor
      )
  }

  def distanceOfTwoGalaxies(
      galaxyOne: Galaxy,
      galaxyTwo: Galaxy
  ): Long = {
    Math.abs(galaxyOne.x - galaxyTwo.x) + Math.abs(galaxyOne.y - galaxyTwo.y)
  }

  def totalDistanceOfAllGalaxies(galaxies: List[Galaxy]): Long = {
    galaxies match {
      case ::(head, next) =>
        galaxies.foldLeft(0L)((acc, galaxy) =>
          distanceOfTwoGalaxies(head, galaxy) + acc
        ) + totalDistanceOfAllGalaxies(next)
      case Nil => 0
    }
  }

  val partOneSolution = totalDistanceOfAllGalaxies(
    parseGalaxies(linesOfText, expansionFactor = 2)
  )
  println(partOneSolution)

  val partTwoSolution = totalDistanceOfAllGalaxies(
    parseGalaxies(linesOfText, expansionFactor = 1000000)
  )
  println(partTwoSolution)

}
