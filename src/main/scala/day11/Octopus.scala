package day11

import cats.parse.Parser
import cats.parse.Rfc5234._
import spire.math.Natural

case class Octopus(
    energy: Natural,
    flashedInRounds: Set[Int]
)

object Octopus {

  val parser: Parser[Octopus] = digit.map(c =>
    Octopus(
      energy = Natural(List(c).mkString),
      flashedInRounds = Set.empty
    )
  )

}
