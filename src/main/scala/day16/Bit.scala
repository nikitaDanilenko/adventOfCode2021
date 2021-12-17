package day16

import cats.parse.Parser
import spire.implicits.*
import spire.math.Natural
import spire.syntax.additiveMonoid.*

sealed trait Bit

object Bit {
  case object I extends Bit
  case object O extends Bit

  val parser: Parser[Bit] = Parser.oneOf(
    List(
      Parser.char('0').map(_ => O),
      Parser.char('1').map(_ => I)
    )
  )

  def bitsToNatural(bits: Seq[Bit]): Natural =
    bits.reverse.zipWithIndex.map {
      case (b, i) =>
        b match {
          case I => Natural(2).pow(Natural(i))
          case O => Natural.zero
        }
    }.qsum

}
