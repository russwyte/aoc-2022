class Day02 extends AdventOfCodeSuite {
  val testInput = """
  |A Y
  |B X
  |C Z""".stripMargin.trim

  case class Round(opponent: Game, player: Game)

  case class Result(opponent: Outcome, player: Outcome)
  object Result:
    def apply(round: Round): Result = Result(
      round.opponent.play(round.player),
      round.player.play(round.opponent)
    )

  case class Score(opponent: Int, player: Int):
    override def toString(): String = s"opponent: $opponent, player: $player"
  object Score:
    def apply(round: Round): Score =
      val result = Result(round)
      Score(
        round.opponent.value + result.opponent.value,
        round.player.value + result.player.value
      )

  enum Outcome(val value: Int):
    case Win extends Outcome(6)
    case Lose extends Outcome(0)
    case Draw extends Outcome(3)

  object Outcome:
    def apply(s: String) = s match
      case "Z" => Win
      case "X" => Lose
      case "Y" => Draw

  extension (o: Outcome)
    def cheat(game: Game): Game =
      import Outcome.*
      import Game.*
      o match
        case Draw => game
        case Win =>
          game match
            case Rock     => Paper
            case Paper    => Scissors
            case Scissors => Rock
        case Lose =>
          game match
            case Rock     => Scissors
            case Paper    => Rock
            case Scissors => Paper

  enum Game(val value: Int):
    case Rock extends Game(value = 1)
    case Paper extends Game(value = 2)
    case Scissors extends Game(value = 3)

  object Game:
    // we map ABC and XYZ for part1 - but this is not relevant for part2
    def apply(s: String) = s match
      case "A" | "X" => Rock
      case "B" | "Y" => Paper
      case "C" | "Z" => Scissors

  extension (g: Game)
    def play(opponent: Game): Outcome =
      if g == opponent then Outcome.Draw
      else if g == Game.Rock && opponent == Game.Scissors then Outcome.Win
      else if g == Game.Paper && opponent == Game.Rock then Outcome.Win
      else if g == Game.Scissors && opponent == Game.Paper then Outcome.Win
      else Outcome.Lose

  def makeTournamentPart1(input: String): Array[Round] =
    input.split("\n").collect { case s"$opponent $player" =>
      Round(Game(opponent), Game(player))
    }

  def makeTournamentPart2(input: String): Array[Round] =
    input.split("\n").collect { case s"$opponent $player" =>
      val opponentGame = Game(opponent)
      Round(opponentGame, Outcome(player).cheat(opponentGame))
    }

  def scoreTournament(tournament: Array[Round]): Array[Score] =
    tournament.map(Score.apply)

  def tallyScores(scores: Array[Score]): Score = scores.reduce { (a, b) =>
    Score(a.opponent + b.opponent, a.player + b.player)
  }

  test("score works") {
    assert(Score(Round(Game.Rock, Game.Rock)) == Score(4, 4))
    assert(Score(Round(Game.Rock, Game.Paper)) == Score(1, 8))
    assert(Score(Round(Game.Rock, Game.Scissors)) == Score(7, 3))
    assert(Score(Round(Game.Paper, Game.Rock)) == Score(8, 1))
    assert(Score(Round(Game.Paper, Game.Paper)) == Score(5, 5))
    assert(Score(Round(Game.Paper, Game.Scissors)) == Score(2, 9))
    assert(Score(Round(Game.Scissors, Game.Rock)) == Score(3, 7))
    assert(Score(Round(Game.Scissors, Game.Paper)) == Score(9, 2))
    assert(Score(Round(Game.Scissors, Game.Scissors)) == Score(6, 6))
  }

  test("part 1") {
    assertEquals(
      tallyScores(scoreTournament(makeTournamentPart1(testInput))).player,
      15
    )
    println(tallyScores(scoreTournament(makeTournamentPart1(input))))
  }

  test("part 2") {
    assertEquals(
      tallyScores(scoreTournament(makeTournamentPart2(testInput))).player,
      12
    )
    println(tallyScores(scoreTournament(makeTournamentPart2(input))))
  }

}
