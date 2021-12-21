package day21

import monocle.syntax.all._
import spire.math.Natural
import spire.syntax.additiveMonoid._
import spire.implicits._

object Day21 {

  case class DeterministicDie(value: Int, size: Int)

  object DeterministicDie {

    def next(deterministicDie: DeterministicDie): DeterministicDie =
      deterministicDie.focus(_.value).modify(x => 1 + x % deterministicDie.size)

  }

  case class PlayerScores(
    p1: BigInt,
    pos1: Int,
    p2: BigInt,
    pos2: Int
  )

  def nextPos(max: Int, pos: Int, value: Int): Int =
    def step(pos: Int): Int =
      if pos < max then 1 + pos else 1

    List.iterate(pos, 1 + value)(step).last

  def move(
      first: Boolean,
      playerScores: PlayerScores,
      deterministicDie: DeterministicDie
  ): (Boolean, PlayerScores, DeterministicDie) =
    val nextDice = List.iterate(deterministicDie, 4)(DeterministicDie.next).tail
    val values = nextDice.map(_.value).sum
    val modifier: Int => Int = v => nextPos(10, v, values)

    val newScore =
      if first
      then
        val updated = playerScores.focus(_.pos1).modify(modifier)
        updated.focus(_.p1).modify(s => s + updated.pos1)
      else
        val updated = playerScores.focus(_.pos2).modify(modifier)
        updated.focus(_.p2).modify(s => s + updated.pos2)

    (!first, newScore, nextDice.last)

  case class Result(
      iterations: Int,
      losingScore: BigInt
  )

  def iterateUntil(targetScore: Int, playerScores: PlayerScores, deterministicDie: DeterministicDie): Result =
    val xs = LazyList.iterate((true, playerScores, deterministicDie))(move.tupled)
      .takeWhile{
        case (f, ps, dd) => ps.p1 < targetScore && ps.p2 < targetScore
      }

    val iterations = xs.length
    val last = xs.last
    val losing = last._2.p1.min(last._2.p2)
    Result(3 * iterations, losing)

  @main
  def solution1: Result =
    val startScores = PlayerScores(p1 = 0, pos1 = 10, p2 = 0, pos2 = 6)
    val die = DeterministicDie(value = 100, size = 100)
    val result = iterateUntil(1000, startScores, die)
    pprint.log(result)
    pprint.log(result.iterations * result.losingScore)
    result

}
