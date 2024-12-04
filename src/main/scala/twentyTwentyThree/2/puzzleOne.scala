package dayTwo

import dayTwo.Game.parseGame
import utils.inputReader.lines

object puzzleOne extends App {
  private val games = lines("2.txt")

  def possibleGames(games: List[Game]): List[Game] = {
    games.filterNot(game => game.rounds.exists(_.isPossible == false))
  }

  val solution = possibleGames(games.map(parseGame)).foldLeft(0)((acc, game) =>
    acc + game.id
  )
  println(solution)
}
