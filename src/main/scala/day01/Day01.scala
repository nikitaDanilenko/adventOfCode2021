package day01

import scala.io.Source
import scala.util.Try

object Day01 {

  private lazy val numbers =
    Source
      .fromResource("Day01-1.txt").getLines()
      .flatMap(parseNumber)
      .toList

  def parseNumber(string: String): Option[BigInt] =
    Try(BigInt(string)).toOption

  def countIncreases(numbers: List[BigInt]): Int =
    if numbers.isEmpty then 0
    else
      numbers.lazyZip(numbers.tail)
        .count(_ < _)

  def logIncreases(numbers: List[BigInt]): Unit =
    val increases = countIncreases(numbers)
    pprint.log(increases)

  @main
  def go: Unit =
    logIncreases(numbers)

  @main
  def go2: Unit =
    val triples = numbers
      .lazyZip(numbers.tail)
      .lazyZip(numbers.drop(2))
      .map {
        case (n1, n2, n3) => n1 + n2 + n3
      }
    logIncreases(triples)
}
