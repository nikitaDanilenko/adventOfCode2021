package day08

import cats.data.NonEmptyList
import cats.parse.Rfc5234.*
import cats.parse.Rfc5234
import cats.parse.Parser

import scala.io.Source

object Day08 {

  case class Line(
      words: Vector[String],
      code: Vector[String]
  )

  private val wordsParser = alpha.rep.repSep(Parser.char(' ').rep)

  val lineParser: Parser[Line] = {
    def recombine(list: NonEmptyList[NonEmptyList[Char]]): Vector[String] =
      list.iterator
        .map(_.iterator.mkString.sorted)
        .toVector
        .sorted

    for {
      words <- wordsParser
      _ <- Parser.char(' ').rep
      _ <- Parser.char('|')
      _ <- Parser.char(' ').rep
      code <- wordsParser
    } yield Line(
      words = recombine(words),
      code = recombine(code)
    )
  }

  val input: Vector[Line] = Source
    .fromResource("Day08.txt")
    .getLines()
    .flatMap(
      lineParser.parse(_).map(_._2).toOption
    )
    .toVector

  def translateSimple(code: Vector[String]): Int = {
    code.count( s => List(2, 3, 4, 7).contains(s.length))
  }

  @main
  def solution1: Unit =
    val allCounts = input.map(l => translateSimple(l.code)).sum
    pprint.log(allCounts)

}
