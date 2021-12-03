package day03

import spire.math.Natural
import spire.implicits.*
import spire.compat.*

import scala.io.Source

object Day03 {

  private val input1: List[String] = Source
    .fromResource("Day03-1.txt").getLines()
    .toList

  def columnsVia(compare: (Int, Int) => Boolean)(numbers: Seq[String]): Natural =
     binaryToNatural(
       numbers.map(_.toList)
        .transpose
        .zipWithIndex
        .map { case (column, i) =>
           val ones = column.count(_ == '1')
           if compare(ones, column.length - ones) then '1' else '0'
        }
     )

  def binaryToNatural(bins: Seq[Char]): Natural =
    bins.reverse
      .zipWithIndex
      .map { case (c, i) =>
        if c == '1' then Natural.one << i else Natural.zero
      }
      .foldLeft(Natural.zero)(_ + _)

  def gammaOf(numbers: Seq[String]): Natural =
    columnsVia(_ >= _)(numbers)

  def epsilonOf(numbers: Seq[String]): Natural =
    columnsVia(_ <= _)(numbers)

  def comparedBitAt(moreOnes: Char, lessOnes: Char, position: Int, numbers: Seq[String]): Char =
    rearrange(numbers)
      .collectFirst { case (column, i) if i == position =>
        val ones = column.count(_ == '1')
        if ones >= column.length - ones then moreOnes else lessOnes
      }.getOrElse('0')

  def ratingWith(lessOnes: Char, moreOnes: Char, numbers: Seq[String]): Natural =
    val columns = numbers.headOption.fold(0)(_.length)
    0.until(columns).foldLeft(numbers) {
      (remainders, position) =>
        val relevantBit = comparedBitAt(
          moreOnes = moreOnes,
          lessOnes = lessOnes,
          position = position,
          numbers = remainders
        )
        //unsafe, but sufficient
        val filtered = remainders.filter(_.charAt(position) == relevantBit)
        if filtered.nonEmpty then filtered else remainders
    }
      .headOption
      .fold(Natural.zero)(s => binaryToNatural(s.toList))

  private def rearrange(numbers: Seq[String]): Seq[(Seq[Char], Int)] =
    numbers.map(_.toList)
      .transpose
      .zipWithIndex

  @main
  def solution1: Unit =
    val gamma = gammaOf(input1)
    val epsilon = epsilonOf(input1)
    pprint.log(gamma * epsilon)

  @main
  def solution2: Unit =
    val oxygen = ratingWith('0', '1', input1)
    val co2 = ratingWith('1', '0', input1)
    pprint.log(oxygen * co2)
}
