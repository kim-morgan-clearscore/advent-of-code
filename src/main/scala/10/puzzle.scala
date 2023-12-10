package dayTen

import utils.inputReader.lines

import scala.annotation.tailrec

sealed trait Direction
case object North extends Direction
case object South extends Direction
case object East extends Direction
case object West extends Direction
case object None extends Direction

case class Position(x: Int, y: Int, direction: Direction) {
  def translate(direction: Direction): Position =
    direction match {
      case North => Position(x, y - 1, North)
      case South => Position(x, y + 1, South)
      case East  => Position(x + 1, y, East)
      case West  => Position(x - 1, y, West)
      case None  => this
    }
}

sealed trait Pipe {
  def next(current: Direction): Direction = {
    this match {
      case TopLeftCorner     => if (current == North) East else South
      case TopRightCorner    => if (current == North) West else South
      case BottomLeftCorner  => if (current == South) East else North
      case BottomRightCorner => if (current == South) West else North
      case Vertical          => if (current == North) North else South
      case Horizontal        => if (current == East) East else West
      case Start             => if (current == North) East else North
      case Empty             => None
    }
  }
}
case object TopLeftCorner extends Pipe
case object TopRightCorner extends Pipe
case object BottomLeftCorner extends Pipe
case object BottomRightCorner extends Pipe
case object Vertical extends Pipe
case object Horizontal extends Pipe
case object Start extends Pipe
case object Empty extends Pipe

object puzzle extends App {
  private val linesOfGridText = lines("10.txt")
  private def parsePipes(string: String): List[Pipe] = {
    val pipeRegex = """[FJLS\-7|.]""".r
    pipeRegex
      .findAllIn(string)
      .matchData
      .map { matchResult =>
        matchResult.toString match {
          case "F" => TopLeftCorner
          case "J" => BottomRightCorner
          case "L" => BottomLeftCorner
          case "S" => Start
          case "-" => Horizontal
          case "7" => TopRightCorner
          case "|" => Vertical
          case "." => Empty
        }
      }
      .toList
  }

  private val parsedGrid = linesOfGridText.map(parsePipes)

  private def findStart(grid: List[List[Pipe]]): Position = {
    val startLine = grid.zipWithIndex
      .filter(listWithIndex => listWithIndex._1.contains(Start))
      .head
    Position(startLine._1.indexOf(Start), startLine._2, East)
  }

  private def loop(
      current: Position,
      grid: List[List[Pipe]]
  ): List[Position] = {

    @tailrec
    def loopHelper(
        current: Position,
        grid: List[List[Pipe]],
        positions: List[Position]
    ): List[Position] = {
      val currentPipe = grid(current.y)(current.x)
      if (currentPipe == Start) positions
      else {
        val directionToMove = currentPipe.next(current.direction)
        val next = current.translate(directionToMove)
        loopHelper(next, grid, next +: positions)
      }
    }

    loopHelper(current, grid, List(findStart(grid)))
  }

  private val flipPatterns = List(
    List(Vertical),
    List(TopLeftCorner, BottomRightCorner),
    List(BottomLeftCorner, TopRightCorner)
  )

  private val noFlipPatterns = List(
    List(TopLeftCorner, TopRightCorner),
    List(BottomLeftCorner, BottomRightCorner)
  )

  private def isPipeInLoop(
      xCoordinate: Int,
      yCoordinate: Int,
      loop: List[Position]
  ): Boolean = {
    loop.find(pos => pos.x == xCoordinate && pos.y == yCoordinate).isDefined
  }

  private def findElementsInLoop(
      row: List[Pipe],
      loop: List[Position],
      insideLoop: Boolean = false,
      pattern: List[Pipe] = List.empty,
      xCoordinate: Int = 0,
      yCoordinate: Int
  ): Int =
    if (xCoordinate == row.length - 1) 0
    else {
      val currentPipe = row(xCoordinate)
      val partOfLoop = isPipeInLoop(xCoordinate, yCoordinate, loop)
      if (partOfLoop) {
        val newPattern = currentPipe match {
          case Horizontal => pattern
          case Start      => pattern :+ TopLeftCorner
          case _          => pattern :+ currentPipe
        }
        if (flipPatterns.contains(newPattern))
          findElementsInLoop(
            row,
            loop,
            !insideLoop,
            List.empty,
            xCoordinate + 1,
            yCoordinate
          )
        else if (noFlipPatterns.contains(newPattern))
          findElementsInLoop(
            row,
            loop,
            insideLoop,
            List.empty,
            xCoordinate + 1,
            yCoordinate
          )
        else
          findElementsInLoop(
            row,
            loop,
            insideLoop,
            newPattern,
            xCoordinate + 1,
            yCoordinate
          )
      } else {
        if (insideLoop)
          1 + findElementsInLoop(
            row,
            loop,
            insideLoop,
            pattern,
            xCoordinate + 1,
            yCoordinate
          )
        else
          findElementsInLoop(
            row,
            loop,
            insideLoop,
            pattern,
            xCoordinate + 1,
            yCoordinate
          )
      }
    }

  private def findElementsInGrid(
      grid: List[List[Pipe]],
      loop: List[Position],
      yCoordinate: Int = 0
  ): Int = {
    if (yCoordinate == grid.length - 1) 0
    else
      findElementsInLoop(
        row = grid(yCoordinate),
        loop = loop,
        yCoordinate = yCoordinate
      ) + findElementsInGrid(grid, loop, yCoordinate + 1)
  }

  private val puzzleLoop =
    loop(findStart(parsedGrid).translate(East), parsedGrid)

  private val partOneSolution = puzzleLoop.length / 2

  println(partOneSolution)

  private val partTwoSolution =
    findElementsInGrid(grid = parsedGrid, loop = puzzleLoop)

  println(partTwoSolution)

}
