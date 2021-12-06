package day06

import algebra.ring.AdditiveMonoid
import spire.math.{Natural, abs}
import spire.syntax.additiveMonoid._
import spire.implicits._


import scala.io.Source

object Day06 {

  val input: LifeMap =
    Source.fromResource("Day06.txt")
      .getLines()
      .flatMap(_.split(",").toList.map(Natural.apply))
      .toVector
      .groupBy(identity).map {
         case (n, fs) =>
           val key = LifeCycle.valueOf(s"_$n")
           key -> Natural(fs.length)
       }.withDefault(_ => Natural.zero)

  @main
  def solution1: Unit =
    val iterated = List.iterate(input, 81)(stepMap)
    pprint.log(iterated.last.valuesIterator.toList.qsum.toBigInt)

  type LifeMap = Map[LifeCycle, Natural]

  def stepMap(map: LifeMap): LifeMap = {
    val zeroCycle = map(LifeCycle._0)
    map.toList.flatMap {
      case (key, n) =>
        previous(key).map(_ -> n)
    }.toMap
      .updatedWith(LifeCycle._6)(v => Some(v.getOrElse(Natural.zero) + zeroCycle))
      .updated(LifeCycle._8, zeroCycle)
      .withDefault(_ => Natural.zero)
  }

  @main
  def solution2: Unit =
    val iterated = List.iterate(input, 257)(stepMap)
    pprint.log(iterated.last.valuesIterator.toList.qsum.toBigInt)

}
