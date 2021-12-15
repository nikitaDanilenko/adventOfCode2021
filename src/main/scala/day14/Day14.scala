package day14

import spire.implicits._
import spire.compat._
import Translation.CharKey
import Translation.FrequencyMap

object Day14 {

  val frequencyMap: FrequencyMap = {
    val input = "OOVSKSPKPPPNNFFBCNOV"
    input.zip(input.tail)
      .groupBy(identity)
      .view
      .mapValues(ps => BigInt(ps.length))
      .toMap
  }

  def replace(dictionary: Map[CharKey, Char])(frequencyMap: FrequencyMap): FrequencyMap =
    def keyToKeys(key: CharKey): (CharKey, CharKey) =
      val dictionaryValue = dictionary(key)
      ((key._1, dictionaryValue), (dictionaryValue, key._2))

    def increase(inc: BigInt)(value: Option[BigInt]): Option[BigInt] =
      Some(value.getOrElse(BigInt(0)) + inc)

    frequencyMap.toList.foldLeft(frequencyMap) { case (map, (charKey, n)) =>
      val (key1, key2) = keyToKeys(charKey)
      map.updatedWith(charKey)(_.map(_ - n).filter(_ > BigInt(0)))
        .updatedWith(key1)(increase(n))
        .updatedWith(key2)(increase(n))
    }

  def evaluate(input: FrequencyMap): BigInt =
    val ordered = input
      .toList
      .flatMap { case ((c1, c2), n) => List(c1 -> n, c2 -> n) }
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).sum)
      .toList
      .sortBy(_._2)

    (ordered.last._2 - ordered.head._2 + BigInt(1)) / 2

  @main
  def solution1: Unit =
    val stepped = List.iterate(frequencyMap, 11)(replace(Translation.dictionary)).last
    pprint.log(evaluate(stepped))

  @main
  def solution2: Unit =
    val stepped = List.iterate(frequencyMap, 41)(replace(Translation.dictionary)).last
    pprint.log(evaluate(stepped))
}
