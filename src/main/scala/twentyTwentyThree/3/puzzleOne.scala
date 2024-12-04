package dayThree

import utils.inputReader.lines
final case class Number(
    startIndex: Int,
    endIndex: Int,
    value: Int
)

final case class Symbol(
    index: Int,
    gearOne: Option[Number] = None,
    gearTwo: Option[Number] = None
) {
  val gearRatio =
    gearOne
      .flatMap(gearOne => gearTwo.map(gearTwo => gearOne.value * gearTwo.value))
      .getOrElse(0)
}

object puzzleOne extends App {
  val linesOfText = lines("3.txt")

  def parseNumbers(
      input: String
  ): List[Number] = {
    val numberRegex = """\d+""".r
    (numberRegex findAllIn input).matchData.map { matchResult =>
      val startIdx = matchResult.start
      val endIdx = matchResult.end
      Number(
        startIndex = startIdx,
        endIndex = endIdx - 1,
        matchResult.toString.toInt
      )
    }.toList
  }

  def parseSymbols(input: String): List[Symbol] = {
    val symbolRegex = """[=&#+/*%\-\@$]""".r
    (symbolRegex findAllIn input).matchData.map { matchResult =>
      val index = matchResult.start
      Symbol(index)
    }.toList
  }

  def numbersInVicinityOfIndex(
      numbers: List[Number],
      index: Int,
      maxIndex: Int
  ): List[Number] =
    numbers.filter(number =>
      index >= Math.max(
        number.startIndex - 1,
        0
      ) && index <= Math.min(
        number.endIndex + 1,
        maxIndex
      )
    )

  def findGearNumbers(
      symbol: Symbol,
      index: Int,
      board: List[List[Number]]
  ): Symbol = {
    val adjacentNumbersInRowAbove = {
      if (index == 0) List.empty
      else
        numbersInVicinityOfIndex(
          numbers = board(index - 1),
          index = symbol.index,
          maxIndex = board.length - 1
        )
    }
    val adjacentNumbersInSameRow =
      numbersInVicinityOfIndex(
        numbers = board(index),
        index = symbol.index,
        maxIndex = board.length - 1
      )
    val adjacentNumbersInRowBelow = {
      if (index == board.length - 1) List.empty
      else
        numbersInVicinityOfIndex(
          numbers = board(index + 1),
          index = symbol.index,
          maxIndex = board.length - 1
        )
    }

    val possibleGearNumbers =
      adjacentNumbersInRowAbove ++ adjacentNumbersInSameRow ++ adjacentNumbersInRowBelow

    if (possibleGearNumbers.length == 2)
      symbol.copy(
        gearOne = possibleGearNumbers.headOption,
        gearTwo = Some(possibleGearNumbers.last)
      )
    else symbol
  }

  def isSymbolInVicinityOfIndex(
      symbols: List[Symbol],
      startIndex: Int,
      endIndex: Int
  ): Boolean =
    symbols
      .find(symbol =>
        symbol.index >= (startIndex - 1) && symbol.index <= endIndex + 1
      )
      .isDefined
  def symbolInVicinity(
      number: Number,
      symbols: List[List[Symbol]],
      index: Int
  ): Boolean = {
    val symbolInRowAbove = {
      if (index == 0) false
      else
        isSymbolInVicinityOfIndex(
          symbols(index - 1),
          number.startIndex,
          number.endIndex
        )
    }
    val symbolOnSameRow =
      isSymbolInVicinityOfIndex(
        symbols(index),
        number.startIndex,
        number.endIndex
      )
    val symbolInRowBelow = {
      if (index == symbols.length - 1) false
      else
        isSymbolInVicinityOfIndex(
          symbols(index + 1),
          number.startIndex,
          number.endIndex
        )
    }
    symbolInRowAbove || symbolOnSameRow || symbolInRowBelow
  }

  val numbers = linesOfText.map(parseNumbers)
  val symbols = linesOfText.map(parseSymbols)

  val partOneSolution = {
    val partialNumbers = numbers.zipWithIndex.flatMap { case (numbers, index) =>
      numbers.filter(number => symbolInVicinity(number, symbols, index))
    }
    partialNumbers.foldLeft(0)((acc, num) => acc + num.value)
  }

  println(partOneSolution)

  val partTwoSolution = {
    val symbolsWithGearNumbers = symbols.zipWithIndex.flatMap {
      case (symbols, index) =>
        symbols.map(symbol => findGearNumbers(symbol, index, numbers))
    }
    symbolsWithGearNumbers.foldLeft(0)((acc, symbol) => acc + symbol.gearRatio)
  }

  println(partTwoSolution)

}
