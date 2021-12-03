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
     numbers.map(_.toList)
      .transpose
      .reverse
      .zipWithIndex
      .map { case (column, i) =>
        val ones = column.count(_ == '1')
        if compare(ones, column.length - ones) then Natural.one << i else Natural.zero
      }.foldLeft(Natural.zero)(_ + _)

  def gammaOf(numbers: Seq[String]): Natural =
    columnsVia(_ >= _)(numbers)

  def epsilonOf(numbers: Seq[String]): Natural =
    columnsVia(_ <= _)(numbers)

  @main
  def solution1: Unit =
    val gamma = gammaOf(input1)
    val epsilon = epsilonOf(input1)
    pprint.log(gamma * epsilon)

}
