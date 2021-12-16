package day15

import day09.Day09
import day09.Day09.{Position, PositionMap, add, neighbours, stringToPositionMap}

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
    } yield
      WeightedEdge(pos, neighbourPos, weight)

    WeightedGraph(edges.toSet)

  @main
  def solution1: Unit =
    val target = (100, 100)
    val result = WeightedGraph.dijkstra(input)((1, 1), target)
    pprint.log(result._1(target))
}
