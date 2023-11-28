package day21

import cats.data.State
import monocle.syntax.all.*
import spire.math.Natural
import spire.syntax.additiveMonoid.*
import spire.implicits.*
import cats.syntax.traverse._

object Day21 {

  case class DeterministicDie(value: Int, size: Int)

  object DeterministicDie {

    def next(deterministicDie: DeterministicDie): DeterministicDie =
      deterministicDie.focus(_.value).modify(x => 1 + x % deterministicDie.size)

  }

  case class PlayerScore(
      position: Int,
      score: BigInt
  )

  case class PlayerScores(
      player1: PlayerScore,
      player2: PlayerScore
  )

  def nextPos(max: Int, pos: Int, value: Int): Int =
    (pos + value - 1) % max + 1

  def newScoreWith(
      modifier: Int => Int,
      first: Boolean,
      playerScores: PlayerScores
  ): PlayerScores =
    if first
    then
      val updated = playerScores.focus(_.player1.position).modify(modifier)
      updated.focus(_.player1.score).modify(s => s + updated.player1.position)
    else
      val updated = playerScores.focus(_.player2.position).modify(modifier)
      updated.focus(_.player2.score).modify(s => s + updated.player2.position)

  def move(
      first: Boolean,
      playerScores: PlayerScores,
      deterministicDie: DeterministicDie
  ): (Boolean, PlayerScores, DeterministicDie) =
    val nextDice = List.iterate(deterministicDie, 4)(DeterministicDie.next).tail
    val values = nextDice.map(_.value).sum
    val modifier: Int => Int = v => nextPos(10, v, values)

    val newScore = newScoreWith(modifier, first, playerScores)

    (!first, newScore, nextDice.last)

  case class Result(
      iterations: Int,
      losingScore: BigInt
  )

  def iterateUntil(targetScore: Int, playerScores: PlayerScores, deterministicDie: DeterministicDie): Result =
    val xs = LazyList
      .iterate((true, playerScores, deterministicDie))(move.tupled)
      .takeWhile { case (f, ps, dd) =>
        ps.player1.score < targetScore && ps.player2.score < targetScore
      }

    val iterations = xs.length
    val last = xs.last
    val losing = last._2.player1.score.min(last._2.player2.score)
    Result(3 * iterations, losing)

  case class Wins(
      player1: BigInt,
      player2: BigInt
  )

  type StateMap = Map[PlayerScoresWithTurn, Wins]

  case class PlayerScoresWithTurn(
      playerScores: PlayerScores,
      first: Boolean
  )

  object PlayerScoresWithTurn {

    def scoreReached(score: Int, playerScoresWithTurn: PlayerScoresWithTurn): Boolean =
      playerScoresWithTurn.playerScores.player1.score >= score ||
        playerScoresWithTurn.playerScores.player2.score >= score

  }

  // Largely based upon https://todd.ginsberg.com/post/advent-of-code/2021/day21/
  def quantum: (StateMap, Wins) = {
    val frequencyMap =
      List(
        3 -> BigInt(1),
        4 -> BigInt(3),
        5 -> BigInt(6),
        6 -> BigInt(7),
        7 -> BigInt(6),
        8 -> BigInt(3),
        9 -> BigInt(1)
      )

    def iterate(scores: PlayerScoresWithTurn): State[StateMap, Wins] =
      if (PlayerScoresWithTurn.scoreReached(21, scores))
        if (scores.playerScores.player1.score > scores.playerScores.player2.score) State.pure(Wins(1, 0))
        else State.pure(Wins(0, 1))
      else {
        for {
          stateMap <- State.get[StateMap]
          result <- stateMap
            .get(scores)
            .fold {
              for {
                newWins <- frequencyMap
                  .traverse { (diceValue, frequency) =>
                    val newScores = newScoreWith(v => nextPos(10, v, diceValue), scores.first, scores.playerScores)
                    iterate(PlayerScoresWithTurn(newScores, !scores.first)).map { wins =>
                      Wins(
                        wins.player1 * frequency,
                        wins.player2 * frequency
                      )
                    }
                  }
                  .map {
                    _.reduce { (wins1, wins2) =>
                      Wins(
                        wins1.player1 + wins2.player1,
                        wins1.player2 + wins2.player2
                      )
                    }
                  }
                _ <- State.modify[StateMap](_.updated(scores, newWins))
              } yield newWins
            }(State.pure)
        } yield result

      }

    iterate(
      PlayerScoresWithTurn(
        PlayerScores(
          PlayerScore(
            score = 0,
            position = 10
          ),
          PlayerScore(
            score = 0,
            position = 6
          )
        ),
        true
      )
    ).run(Map.empty).value
  }

  @main
  def solution1: Result =
    val startScores = PlayerScores(
      PlayerScore(
        score = 0,
        position = 10
      ),
      PlayerScore(
        score = 0,
        position = 6
      )
    )
    val die = DeterministicDie(value = 100, size = 100)
    val result = iterateUntil(1000, startScores, die)
    pprint.log(result)
    pprint.log(result.iterations * result.losingScore)
    result

  @main
  def solution2(): Unit =
    val wins = quantum
    pprint.log(wins._2)

}
