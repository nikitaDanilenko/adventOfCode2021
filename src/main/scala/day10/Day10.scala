package day10

import cats.parse.Parser

import scala.io.Source

object Day10 {

  type Chunk = Vector[Brace]

  val input: Vector[Chunk] =
    Source
      .fromResource("Day10.txt")
      .getLines()
      .toVector
      .flatMap(Brace.parser.rep.parse(_).toOption.map(_._2.toList.toVector))

  sealed trait Result

  object Result {
    case object Ok extends Result
    case class Incomplete(stack: List[Brace]) extends Result
    case class Error(brace: Brace) extends Result
  }

  def descend(chunk: Chunk): Result =
    def go(stack: List[Brace], chunk: Chunk): Result =
      if stack.nonEmpty && chunk.isEmpty
        then Result.Incomplete(stack)
      else if stack.isEmpty && chunk.isEmpty
        then Result.Ok
      else
        val brace = chunk.head
        if brace.direction == Direction.Open
          then go(brace +: stack, chunk.tail)
        else
          val maybeFirst = stack.headOption
          maybeFirst.fold(Result.Error(brace)) { firstBrace =>
            if firstBrace.symbol == brace.symbol
              then go(stack.tail, chunk.tail)
            else
              Result.Error(brace)
          }

    go(List.empty, chunk)

  val points: Map[Brace, Int] = Map(
    Brace(Symbol.Parenthesis, Direction.Close) -> 3,
    Brace(Symbol.Square, Direction.Close) -> 57,
    Brace(Symbol.Curly, Direction.Close) -> 1197,
    Brace(Symbol.Angle, Direction.Close) -> 25137,
  )

  @main
  def solution1: Unit =
    val result =
      input.map(descend).collect {
        case Result.Error(brace) =>
          points.getOrElse(brace, 0)
      }.sum
    pprint.log(result)

  def complete(stack: List[Brace]): List[Brace] =
    stack.map(_.copy(direction = Direction.Close))

  val completionPoints: Map[Brace, BigInt] = Map(
    Brace(Symbol.Parenthesis, Direction.Close) -> BigInt(1),
    Brace(Symbol.Square, Direction.Close) -> BigInt(2),
    Brace(Symbol.Curly, Direction.Close) -> BigInt(3),
    Brace(Symbol.Angle, Direction.Close) -> BigInt(4),
  )

  @main
  def solution2: Unit =
    val resultList =
      input.map(descend).collect {
        case Result.Incomplete(stack) =>
          // The complete step is technically redundant, because it only changes the directions
          complete(stack).foldLeft(BigInt(0)) { (c, brace) =>
            BigInt(5) * c + completionPoints.getOrElse(brace, BigInt(0))
          }
      }.sorted
    pprint.log(resultList(resultList.length / 2))
}
