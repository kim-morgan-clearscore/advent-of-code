package dayTwo

import dayTwo.Game.parseGame
import utils.inputReader.lines

object puzzleTwo extends App {
  private val gamesText = lines("2.txt")
  private val games = gamesText.map(parseGame)

  def minimumCubesInBag(game: Game): Bag =
    Bag(
      red = game.rounds.maxBy(_.red).red,
      green = game.rounds.maxBy(_.green).green,
      blue = game.rounds.maxBy(_.blue).blue
    )

  val solution =
    games.map(minimumCubesInBag).foldLeft(0)((acc, bag) => acc + bag.power)
  println(solution)
}
