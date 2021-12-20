package day05

import cats.parse.Parser
import cats.parse.Rfc5234.digit
import spire.math.Natural
import monocle.syntax.all.*
import spire.algebra.AdditiveMonoid

import scala.io.Source

case class Position(x: Int, y: Int)

object Position {
  implicit val positionAdditiveMonoid: AdditiveMonoid[Position] = new AdditiveMonoid[Position] {
    override val zero: Position = Position(0, 0)
    override def plus(p1: Position, p2: Position): Position =
      Position(p1.x + p2.x, p1.y + p2.y)
  }
}

case class Line(from: Position, to: Position)

val nonNegativeIntParser: Parser[Int] = digit.rep.map(_.toList.mkString.toInt)

val positionParser: Parser[Position] =
  for {
    x <- nonNegativeIntParser
    _ <- Parser.char(',')
    y <- nonNegativeIntParser
  } yield Position(x, y)

val lineParser: Parser[Line] =
  for {
    from <- positionParser
    _ <- Parser.string(" -> ")
    to <- positionParser
  } yield Line(from, to)

def parseLine(text: String): Option[Line] =
  lineParser.parse(text).toOption.map(_._2)

private val input: List[Line] = Source
  .fromResource("Day05.txt")
  .getLines()
  .flatMap(parseLine)
  .toList

def horizontalPositions(line: Line): Seq[Position] =
  if line.from.y == line.to.y
  then
    spire.math.min(line.from.x, line.to.x)
      .to(spire.math.max(line.from.x, line.to.x))
      .map(Position(_, line.from.y))
  else Seq.empty

def verticalPositions(line: Line): Seq[Position] =
  if line.from.x == line.to.x
  then
    spire.math.min(line.from.y, line.to.y)
      .to(spire.math.max(line.from.y, line.to.y))
      .map(Position(line.from.x, _))
  else Seq.empty

type Area = Map[Position, Natural]

def markPositions(area: Area, positions: Seq[Position]): Area =
  positions.foldLeft(area) { (a, pos) =>
    a.updatedWith(pos)(n => Some(n.getOrElse(Natural.zero) + Natural.one))
  }

def diagonalUpPositions(line: Line): Seq[Position] =
  if line.from.x + line.from.y == line.to.x + line.to.y
    then
      val (first, second) = if line.from.x <= line.to.x then (line.from, line.to) else (line.to, line.from)
      0.to(second.x - first.x).map(i => first.focus(_.x).modify(_ + i).focus(_.y).modify(_ - i))
  else Seq.empty

def diagonalDownPositions(line: Line): Seq[Position] =
  if line.from.y - line.from.x == line.to.y - line.to.x
    then
      val (first, second) = if line.from.x <= line.to.x then (line.from, line.to) else (line.to, line.from)
      0.to(second.x - first.x).map(i => first.focus(_.x).modify(_ + i).focus(_.y).modify(_ + i))
  else
    Seq.empty

@main
def solution1: Unit =
  val allPositions = input.flatMap(l => horizontalPositions(l) ++ verticalPositions(l))
  val markedArea = markPositions(Map.empty, allPositions)
  pprint.log(markedArea.count(_._2 > Natural.one))

@main
def solution2: Unit =
  val allPositions = input.flatMap(l => horizontalPositions(l) ++ verticalPositions(l) ++ diagonalUpPositions(l) ++ diagonalDownPositions(l))
  val markedArea = markPositions(Map.empty, allPositions)
  pprint.log(markedArea.count(_._2 > Natural.one))