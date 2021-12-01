package day01

import scala.io.Source
import scala.util.Try

object Day01 {

  def parseNumber(string: String): Option[BigInt] =
    Try(BigInt(string)).toOption

  def countIncreases(numbers: List[BigInt]): Int =
    if numbers.isEmpty then 0
    else
      numbers.lazyZip(numbers.tail)
        .count(_ < _)

  @main
  def go: Unit =
    val numbers = Source.fromResource("Day01-1.txt").getLines()
      .flatMap(parseNumber)
    val increases = countIncreases(numbers.toList)
    pprint.log(increases)


}
