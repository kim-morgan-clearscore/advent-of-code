package dayFour

import utils.inputReader.lines

final case class Card(
    id: Int,
    winningNumbers: List[Int],
    cardNumbers: List[Int],
    extraCards: List[Card] = List.empty
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

  private def findExtraCards(
      originalCards: List[Card],
      cards: List[Card]
  ): List[Card] =
    cards match {
      case ::(head, next) => {
        val cardsToAdd = originalCards.slice(
          head.id,
          head.id + head.numberOfWinningNumbersOnCard
        )
        head.copy(extraCards = cardsToAdd) :: findExtraCards(
          originalCards,
          next
        )
      }
      case Nil => List.empty
    }

  private def countEachCard(
      cards: List[Card],
      counts: Map[Int, Int]
  ): Map[Int, Int] =
    cards match {
      case ::(head, next) => {
        val extraCardCount = head.extraCards.foldLeft(0)((acc, card) =>
          acc + counts.get(card.id).getOrElse(0)
        ) + head.extraCards.length
        countEachCard(next, counts ++ Map(head.id -> extraCardCount))
      }
      case Nil => counts
    }

  private val parsedCards = cards.map(parseCard)

  private val partOneSolution =
    parsedCards.foldLeft(0)((acc, card) => acc + card.points)

  println(partOneSolution)

  private val partTwoSolution = {
    val cardsWithExtras =
      findExtraCards(parsedCards, parsedCards)
    val countExtraCards =
      countEachCard(cardsWithExtras.reverse, Map.empty).foldLeft(0)(
        (acc, pair) => acc + pair._2
      )
    countExtraCards + parsedCards.length
  }

  println(partTwoSolution)

}
