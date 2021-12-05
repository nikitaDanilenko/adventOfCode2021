package day04

import spire.math.Natural
import monocle.syntax.all.*
import spire.compat.*
import spire.implicits.*

import scala.io.Source

object Day04 {

  private val input = parseInput(Source.fromResource("Day04.txt").mkString)

  def parseInput(text: String): (Seq[Natural], Seq[Board]) =
    val lines = text.linesIterator.toList
    val numbers = lines.head.split(",").map(Natural.apply)
    val boards =
      lines
        .tail
        .filter(_.nonEmpty)
        .grouped(5)
        .map { fiveLines =>
          Board(
            fiveLines.map(
              _.split(" ")
                .filter(_.nonEmpty)
                .map(n => Field(Natural(n), false))
                .toList
            )
          )
        }
        .toVector
    (numbers, boards)

  case class Field(
      number: Natural,
      marked: Boolean
  )

  case class Board(
      rows: Seq[Seq[Field]]
  )

  def markField(number: Natural)(field: Field): Field =
    if field.number == number then field.focus(_.marked).set(true) else field

  def markBoard(number: Natural)(board: Board): Board =
    board.focus(_.rows).set(board.rows.map(_.map(markField(number))))

  def winning(board: Board): Boolean =
    val transposed = Board(board.rows.transpose)
    def existsMarkedRow(board: Board): Boolean =
      board.rows.exists(_.forall(_.marked))
    existsMarkedRow(board) || existsMarkedRow(transposed)

  def score(last: Natural, board: Board): Natural =
    board.rows.flatten.filter(!_.marked).map(_.number).foldLeft(Natural.zero)(_ + _) * last

  def scoreOfWinning(boards: Seq[Board], numbers: Seq[Natural]): Natural =
    if (numbers.isEmpty)
      Natural.zero
    else
      val n = numbers.head
      val markedBoards = boards.map(markBoard(n))
      markedBoards
        .collectFirst {
          case board if winning(board) => score(n, board)
        }
        .getOrElse(scoreOfWinning(markedBoards, numbers.tail))

  def lastWinning(boards: Seq[Board], numbers: Seq[Natural]): Natural =
    def go(boards: Seq[Board], numbers: Seq[Natural], nextWinningPosition: Int): Seq[(Board, Natural, Int)] =
      if numbers.isEmpty then Seq.empty
      else
        val n = numbers.head
        val markedBoards = boards.map(markBoard(n))
        val firstWinners = markedBoards
          .collect {
            case board if winning(board) => (board, n, nextWinningPosition)
          }
        firstWinners ++ go(
          markedBoards.toSet.diff(firstWinners.map(_._1).toSet).toList,
          numbers.tail,
          nextWinningPosition + (if firstWinners.isEmpty then 0 else 1)
        )
    val lastWinning = go(boards, numbers, 0).sortBy(_._3).reverse.head
    score(lastWinning._2, lastWinning._1)

  @main
  def solution1: Unit =
    val score = scoreOfWinning(input._2, input._1)
    pprint.log(score)

  @main
  def solution2: Unit =
    val score = lastWinning(boards = input._2, numbers = input._1)
    pprint.log(score)
}
