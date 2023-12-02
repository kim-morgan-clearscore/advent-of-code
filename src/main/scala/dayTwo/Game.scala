package dayTwo

final case class Game(id: Int, rounds: Array[Round])

object Game {
  def parseGame(game: String): Game = {
    val gameRegex = """Game (\d+): (.*)""".r
    game match {
      case gameRegex(id, rounds) => Game(id.toInt, Round.parseRounds(rounds))
    }
  }
}

final case class Round(red: Int, green: Int, blue: Int) {
  val isPossible: Boolean = {
    red <= 12 && green <= 13 && blue <= 14
  }
}

object Round {
  def getNumberOfColor(color: String, colors: Array[String]): Int = {
    val numberRegex = """(\d+)""".r
    colors.find(_.contains(color)) match {
      case Some(string) => numberRegex.findFirstIn(string).fold(0)(_.toInt)
      case None         => 0
    }
  }

  def parseRound(round: String): Round = {
    val colors = round.split(", ")
    Round(
      getNumberOfColor("red", colors),
      getNumberOfColor("green", colors),
      getNumberOfColor("blue", colors)
    )
  }

  def parseRounds(rounds: String): Array[Round] = {
    rounds.split(";").map(parseRound)
  }
}

final case class Bag(red: Int, green: Int, blue: Int) {
  val power = red * green * blue
}
