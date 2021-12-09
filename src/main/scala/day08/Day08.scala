package day08

import cats.data.NonEmptyList
import cats.parse.Rfc5234.*
import cats.parse.Rfc5234
import cats.parse.Parser
import monocle.syntax.all._

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
    code.count(s => List(2, 3, 4, 7).contains(s.length))
  }

  case class Digit(
      topCenter: Set[Char],
      topLeft: Set[Char],
      topRight: Set[Char],
      center: Set[Char],
      bottomLeft: Set[Char],
      bottomRight: Set[Char],
      bottomCenter: Set[Char]
  )

  val allDigit: Digit = {
    val all = "abcdefg".toSet
    Digit(
      topCenter = all,
      topLeft = all,
      topRight = all,
      center = all,
      bottomLeft = all,
      bottomRight = all,
      bottomCenter = all
    )
  }

  def reduceWith(words: Vector[String]): Digit =
    val digit = allDigit

    def findWithLength(length: Int): Set[Char] =
      words.find(_.length == length).get.toSet

    val one = findWithLength(2)
    val four = findWithLength(4)
    val seven = findWithLength(3)
    val digit147 =
      digit.focus(_.topCenter).modify(s => (s -- four) & seven)
        .focus(_.topLeft).modify(s => (s  -- seven) & four)
        .focus(_.center).modify(s => (s -- seven) & four)
        .focus(_.bottomLeft).modify(s => (s  -- seven) -- four)
        .focus(_.bottomCenter).modify(s => (s -- seven) -- four)
        .focus(_.topRight).modify(_ & one)
        .focus(_.bottomRight).modify(_ & one)
    val intersection5 = words.filter(_.length == 5).map(_.toSet).foldLeft(allDigit.topLeft)(_ & _)
    val digit235 =
      digit147.focus(_.topLeft).modify(_ -- intersection5)
      .focus(_.center).modify(_ & intersection5)
      .focus(_.bottomLeft).modify(_ -- intersection5)
      .focus(_.bottomCenter).modify(_ & intersection5)
    val singleRight = words.filter(_.length == 6).map(_.toSet).collectFirst {
      case s if (s & one).size == 1 => s & one
    }.get
    digit235.focus(_.topRight).modify(_ -- singleRight)
      .focus(_.bottomRight).modify(_ & singleRight)

  case class Lit(
    topCenter: Boolean,
    topLeft: Boolean,
    topRight: Boolean,
    center: Boolean,
    bottomLeft: Boolean,
    bottomRight: Boolean,
    bottomCenter: Boolean
  )

  val unlit: Lit = Lit(
    topCenter = false,
    topLeft = false,
    topRight = false,
    center = false,
    bottomLeft = false,
    bottomRight = false,
    bottomCenter = false
  )

  val litDigits: Vector[Lit] = Vector(
    Lit(
      topCenter = true,
      topLeft = true,
      topRight = true,
      center = false,
      bottomLeft = true,
      bottomRight = true,
      bottomCenter = true
    ),
    Lit(
      topCenter = false,
      topLeft = false,
      topRight = true,
      center = false,
      bottomLeft = false,
      bottomRight = true,
      bottomCenter = false
    ),
    Lit(
      topCenter = true,
      topLeft = false,
      topRight = true,
      center = true,
      bottomLeft = true,
      bottomRight = false,
      bottomCenter = true
    ),
    Lit(
      topCenter = true,
      topLeft = false,
      topRight = true,
      center = true,
      bottomLeft = false,
      bottomRight = true,
      bottomCenter = true
    ),
    Lit(
      topCenter = false,
      topLeft = true,
      topRight = true,
      center = true,
      bottomLeft = false,
      bottomRight = true,
      bottomCenter = false
    ),
    Lit(
      topCenter = true,
      topLeft = true,
      topRight = false,
      center = true,
      bottomLeft = false,
      bottomRight = true,
      bottomCenter = true
    ),
    Lit(
      topCenter = true,
      topLeft = true,
      topRight = false,
      center = true,
      bottomLeft = true,
      bottomRight = true,
      bottomCenter = true
    ),
    Lit(
      topCenter = true,
      topLeft = false,
      topRight = true,
      center = false,
      bottomLeft = false,
      bottomRight = true,
      bottomCenter = false
    ),
    Lit(
      topCenter = true,
      topLeft = true,
      topRight = true,
      center = true,
      bottomLeft = true,
      bottomRight = true,
      bottomCenter = true
    ),
    Lit(
      topCenter = true,
      topLeft = true,
      topRight = true,
      center = true,
      bottomLeft = false,
      bottomRight = true,
      bottomCenter = true
    )
  )

  def light(digit: Digit, word: String): Lit =
    word.foldLeft(unlit)((l, c) =>
      l.focus(_.topCenter).modify(_ || digit.topCenter.contains(c))
        .focus(_.topLeft).modify(_ || digit.topLeft.contains(c))
        .focus(_.center).modify(_ || digit.center.contains(c))
        .focus(_.bottomLeft).modify(_ || digit.bottomLeft.contains(c))
        .focus(_.bottomCenter).modify(_ || digit.bottomCenter.contains(c))
        .focus(_.topRight).modify(_ || digit.topRight.contains(c))
        .focus(_.bottomRight).modify(_ || digit.bottomRight.contains(c))
    )

  def litToChar(lit: Lit): Option[Char] =
    val index = litDigits.indexOf(lit)
    if index >= 0
      then index.toString.headOption
    else
      None

  @main
  def solution1: Unit =
    val allCounts = input.map(l => translateSimple(l.code)).sum
    pprint.log(allCounts)

  @main
  def solution2: Unit =
    val result = input.map { line =>
      val digit = reduceWith(line.words)
      line.code.flatMap { c => litToChar(light(digit, c)) }.mkString.toInt
    }.sum
    pprint.log(result)


}
