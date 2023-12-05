package dayFour

import utils.inputReader.lines

final case class Card(
    id: Int,
    winningNumbers: List[Int],
    cardNumbers: List[Int]
) {
  val numberOfWinningNumbersOnCard: Int =
    cardNumbers.count(winningNumbers.contains(_))

  val points: Int =
    if (numberOfWinningNumbersOnCard == 0) 0
    else Math.pow(2, numberOfWinningNumbersOnCard - 1).toInt
}

object puzzle extends App {
  private val cards = lines("4.txt")

  private def parseCard(card: String): Card = {
    val cardRegex = """Card\s+(\d+): (.*) \| (.*)""".r
    card match {
      case cardRegex(id, winningNumbers, cardNumbers) =>
        Card(id.toInt, parseNumbers(winningNumbers), parseNumbers(cardNumbers))
    }
  }

  private def parseNumbers(numbers: String): List[Int] = {
    val numberRegex = """\d+""".r
    numberRegex
      .findAllIn(numbers)
      .matchData
      .map { _.toString.toInt }
      .toList
  }

  private def findIdsOfCopyCardsWon(card: Card): List[Int] = {
    Range
      .inclusive(card.id + 1, card.id + card.numberOfWinningNumbersOnCard)
      .toList
  }

  private def countCardsAndCopies(
      cards: List[Card],
      counts: Map[Int, Int],
      index: Int
  ): Int =
    if (index < 0)
      counts.foldLeft(0)((acc, pair) => acc + pair._2) + cards.length
    else {
      val currentCard = cards(index)
      val copyCardIds = findIdsOfCopyCardsWon(currentCard)
      val totalOccurencesOfCopyCards =
        copyCardIds.foldLeft(0)((acc, cardId) =>
          acc + counts.get(cardId).getOrElse(0)
        )
      val sumForCurrentCardId =
        totalOccurencesOfCopyCards + copyCardIds.length
      countCardsAndCopies(
        cards,
        counts ++ Map(currentCard.id -> sumForCurrentCardId),
        index - 1
      )
    }

  private val parsedCards = cards.map(parseCard)

  private val partOneSolution =
    parsedCards.foldLeft(0)((acc, card) => acc + card.points)

  println(partOneSolution)

  private def partTwoSolution: Int = {
    countCardsAndCopies(
      parsedCards,
      Map.empty,
      parsedCards.length - 1
    )
  }

  println(partTwoSolution)

}
