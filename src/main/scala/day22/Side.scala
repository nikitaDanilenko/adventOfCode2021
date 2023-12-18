package day22

import cats.parse.{ Parser, Parser0 }
import cats.parse.Rfc5234.*
import cats.instances.list.*
import cats.syntax.traverse.*

import scala.util.Try

case class Side(
    from: Int,
    to: Int
)

object Side {

  val signParser: Parser0[Int] =
    Parser
      .char('-')
      .rep0(0, 1)
      .flatMap {
        _.headOption.fold(Parser.pure(1))(_ => Parser.pure(-1))
      }

  val nonNegativeIntParser: Parser[Int] =
    digit.rep.flatMap(ds => Try(ds.toList.mkString.toInt).fold(_ => Parser.fail, Parser.pure))

  val parser: Parser0[Side] = for {
    xSign <- signParser
    x <- nonNegativeIntParser
    _ <- List.fill(2)(Parser.char('.')).sequence
    ySign <- signParser
    y <- nonNegativeIntParser
  } yield Side(
    from = xSign * x,
    to = ySign * y
  )

  def intersection(side1: Side, side2: Side): Option[Side] =
    val candidate = Side(
      from = side1.from.max(side2.from),
      to = side1.to.min(side2.to)
    )
    Some(candidate).filter(s => s.from <= s.to)

  def length(side: Side): BigInt =
    if (side.from <= side.to)
      BigInt(side.to - side.from + 1)
    else
      BigInt(0)

}
