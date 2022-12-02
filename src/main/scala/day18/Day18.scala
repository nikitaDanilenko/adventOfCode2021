package day18

import cats.data.NonEmptyList
import scala.util.chaining._
import scala.io.Source

object Day18 {

  val input: Iterator[SnailNumber] = Source
    .fromResource("Day18.txt")
    .getLines()
    .flatMap(SnailNumber.snailNumberParser.parse(_).map(_._2).toOption)

  @main
  def solution1(): Unit =
    input.toList
      .pipe(NonEmptyList.fromListUnsafe)
      .pipe(SnailNumber.sum)
      .pipe(SnailNumber.magnitude)
      .pipe(pprint.log(_))

  @main
  def solution2(): Unit =
    val list = input.toList
    val magnitudes = for {
      sn1 <- list
      sn2 <- list
      if sn1 != sn2
    } yield SnailNumber.add(sn1, sn2).pipe(SnailNumber.magnitude)

    magnitudes.max
      .pipe(pprint.log(_))

}
