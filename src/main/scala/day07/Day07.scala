package day07

import scala.io.Source

object Day07 {

  val input: Vector[Int] =
    Source
      .fromResource("Day07.txt")
      .getLines()
      .flatMap(
        _.split(",")
          .map(_.toInt)
      )
      .toVector

  def moveToWith(target: Int, numbers: Vector[Int])(f: Int => Int => Int): Int =
    numbers.map(f(target)).sum


  def moveTo2(target: Int, numbers: Vector[Int]): Int =
    numbers.map { n =>
      val d = (n - target).abs
      (d * (1 + d)) / 2
    }.sum

  def solveWith(f: Int => Int => Int): Unit =
    val max = input.max
    val x = 0.to(max).map { t =>
      t -> moveToWith(t, input)(f)
    }.minBy(_._2)
      ._2
    pprint.log(x)

  @main
  def solution1: Unit =
    solveWith(t => n => (n - t).abs)

  @main
  def solution2: Unit =
    solveWith { t => n =>
      val d = (n - t).abs
      (d * (1 + d)) / 2
    }

}
