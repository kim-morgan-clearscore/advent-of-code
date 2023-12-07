package daySeven

import utils.inputReader.lines
final case class Hand(cards: String, bid: Int, jokerRules: Boolean)
    extends Ordered[Hand] {

  private val cardTypes =
    if (jokerRules)
      List('J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A')
    else List('2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A')

  private val counts = {
    val cardCounts =
      cards
        .groupBy(identity)
        .view
        .mapValues(_.length)
        .toMap

    if (jokerRules) {
      if (this.cards == "JJJJJ") cardCounts
      else {
        val maxCardCount = cardCounts.removed('J').maxBy(_._2)
        val jokerCount = cardCounts.get('J').getOrElse(0)
        (cardCounts ++ Map(
          maxCardCount._1 -> (maxCardCount._2 + jokerCount)
        )).removed('J')
      }
    } else cardCounts
  }

  private val result: Result = {
    if (counts.size == 1) FiveKind
    else if (counts.size == 2)
      counts.find(_._2 == 4) match {
        case Some(_) => FourKind
        case None    => FullHouse
      }
    else if (counts.size == 3) counts.find(_._2 == 3) match {
      case Some(_) => ThreeKind
      case None    => TwoPair
    }
    else if (counts.size == 4) OnePair
    else HighCard
  }

  override def compare(hand: Hand): Int = {
    val comparison = if (this.result == hand.result) {
      val commonCards = (this.cards zip hand.cards)
        .takeWhile(pair => pair._1 == pair._2)
        .map(_._1)
        .mkString
      val firstUniqueCardInHand = this.cards.drop(commonCards.length)(0)
      val firstUniqueCardInOther = hand.cards.drop(commonCards.length)(0)
      cardTypes.indexOf(firstUniqueCardInHand) > cardTypes.indexOf(
        firstUniqueCardInOther
      )
    } else this.result.score > hand.result.score
    if (this.cards == hand.cards) 0
    else if (comparison) 1
    else -1
  }
}

sealed trait Result { val score: Int }
case object FiveKind extends Result { val score = 7 }
case object FourKind extends Result { val score = 6 }
case object FullHouse extends Result { val score = 5 }
case object ThreeKind extends Result { val score = 4 }
case object TwoPair extends Result { val score = 3 }
case object OnePair extends Result { val score = 2 }
case object HighCard extends Result { val score = 1 }

object puzzle extends App {
  private val handsText = lines("7.txt")

  private def parseHands(string: String, jokerRules: Boolean): Hand = {
    val hand = string.split(" ")
    Hand(cards = hand(0), bid = hand(1).toInt, jokerRules)
  }

  private val parsedHandsPartOne =
    handsText.map(parseHands(_, false))

  private val partOneSolution =
    parsedHandsPartOne.sorted.zipWithIndex.map { case (hand, index) =>
      hand.bid * (index + 1)
    }.sum

  println(partOneSolution)

  private val parsedHandsPartTwo =
    handsText.map(parseHands(_, true))

  private val partTwoSolution =
    parsedHandsPartTwo.sorted.zipWithIndex.map { case (hand, index) =>
      hand.bid * (index + 1)
    }.sum

  println(partTwoSolution)

}
