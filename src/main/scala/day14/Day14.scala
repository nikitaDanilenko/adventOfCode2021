package day14

object Day14 {

  val baseString: String = "OOVSKSPKPPPNNFFBCNOV"

  def replace(dictionary: Map[String, String])(input: String): String =
    input.zip(input.tail ++ " ").flatMap{ case (x, y) =>
      val key = List(x, y).mkString
      (x +: dictionary.getOrElse(key, "")).trim
    }.mkString


  def evaluate(input: String): BigInt =
    val ordered = input.groupBy(identity)
      .view
      .mapValues(_.length)
      .toList
      .sortBy(_._2)
    ordered.last._2 - ordered.head._2

  @main
  def solution1: Unit =
    val stepped = List.iterate(baseString, 11)(replace(Translation.dictionary)).last
    pprint.log(evaluate(stepped))
}
