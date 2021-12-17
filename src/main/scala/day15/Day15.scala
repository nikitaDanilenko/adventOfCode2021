package day15

import day09.Day09
import day09.Day09.*

import scala.io.Source

object Day15 {

  val input: WeightedGraph[Position] =
    val positionMap = stringToPositionMap(Source.fromResource("Day15.txt")
      .getLines())
    mkGraph(positionMap)


  def neighboursOf(position: Position, positionMap: PositionMap): List[(Position, Int)] =
    neighbours.map(add(position, _))
      .flatMap(p => positionMap.get(p).map(p -> _))

  def mkGraph(positionMap: PositionMap): WeightedGraph[Position] =
    val positions = positionMap.keySet
    val (min, max) = {
      val all = positions.flatMap { case (f, t) => Set(f, t)}
      (all.min, all.max)
    }

    val edges = for {
      i <- 1.to(max)
      j <- 1.to(max)
      pos = (i, j)
      (neighbourPos, weight) <- neighboursOf(pos, positionMap)
    } yield WeightedEdge(pos, neighbourPos, weight)

    val adjacency = edges.groupBy(_.from).view
      .mapValues(_.map(e => (e.to, e.weight)).toMap)
      .toMap

    WeightedGraph(adjacency)



  @main
  def solution1: Unit =
    val target = (100, 100)
    val result = WeightedGraph.dijkstra(input)((1, 1), target)
    pprint.log(result(target))

  def cycleOne(string: String): String =
    string.map { c =>
      if c.isDigit then ((s"$c".toInt % 9) + 1).toString.head
      else c
    }

  def combineHorizontally(ls1: List[String], ls2: List[String]): List[String] =
    ls1.zip(ls2).map(_ ++ _).toList

  def replicate25(string: String): String =
    val horizontal1 = List.iterate(string, 5)(cycleOne).map(_.linesIterator.toList)
    val firstLine = horizontal1.tail.foldLeft(horizontal1.head)(combineHorizontally).mkString("\n")
    List.iterate(firstLine, 5)(cycleOne).mkString("\n")

  // shockingly slow, but yields the correct result
  @main
  def solution2: Unit =
    val extendedPositionMap = stringToPositionMap(
      replicate25(
        Source
          .fromResource("Day15.txt")
          .getLines()
          .mkString("\n")
      ).linesIterator
    )
    val graph = mkGraph(extendedPositionMap)
    val target = (500, 500)
    val result = WeightedGraph.dijkstra(graph)((1, 1), target)
    pprint.log(result(target))


}
