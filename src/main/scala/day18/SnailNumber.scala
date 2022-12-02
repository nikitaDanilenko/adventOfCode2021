package day18

import cats.parse.Parser
import cats.parse.Rfc5234._
import scala.util.chaining._

sealed trait SnailNumber

object SnailNumber {
  case class Plain(int: Int) extends SnailNumber
  case class Complex(left: SnailNumber, right: SnailNumber) extends SnailNumber

  enum Direction:
    case Left, Right

  case class PlainWithPosition(
      int: Int,
      position: List[Direction]
  )

  def toPlainWithPositions(snailNumber: SnailNumber): List[PlainWithPosition] =
    snailNumber match
      case Plain(int) => List(PlainWithPosition(int, List.empty))
      case Complex(left, right) =>
        toPlainWithPositions(left).map(sn => sn.copy(position = Direction.Left +: sn.position))
          ++ toPlainWithPositions(right).map(sn => sn.copy(position = Direction.Right +: sn.position))

  val plainParser: Parser[Plain] = digit.map(c => s"$c".toInt.pipe(SnailNumber.Plain.apply))

  lazy val snailNumberParser: Parser[SnailNumber] = {
    val recursive = plainParser.orElse(Parser.defer(snailNumberParser))

    for {
      _ <- Parser.char('[')
      left <- recursive
      _ <- Parser.char(',')
      right <- recursive
      _ <- Parser.char(']')
    } yield SnailNumber.Complex(left, right)
  }

}
