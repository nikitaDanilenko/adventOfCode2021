package day09

import scala.io.Source

object Day09 {

  type Position = (Int, Int)
  type PositionMap = Map[Position, Int]

  val input: PositionMap =
    Source.fromResource("Day09.txt")
      .getLines()
      .toList
      .map(_.toList.zipWithIndex)
      .zipWithIndex
      .flatMap { case (l, i) =>
        l.map { case (c, j) =>
          (1+i, 1+j) -> s"$c".toInt
        }
      }
      .toMap

  def add(p1: Position, p2: Position): Position =
    (p1._1 + p2._1, p1._2 + p2._2)

  val neighbours: List[Position] = List(
    (-1, 0),
    (0, -1), (0, 1),
    (1, 0)
  )

  def neighboursOf(position: Position, positionMap: PositionMap): List[Int] =
    neighbours.map(add(position, _)).flatMap(positionMap.get)

  def neighbourPositionsOf(position: Position, positionMap: PositionMap): List[Position] =
    neighbours.map(add(position, _)).filter(positionMap.isDefinedAt)

  def isLocalMinimum(position: Position, positionMap: PositionMap): Boolean =
    val mv = positionMap.get(position)
    val ns = neighboursOf(position, positionMap)
    mv.fold(false) { v =>
      ns.forall(_ > v)
    }

  def flow(lowPoint: Position): Set[Position] =
    def repeat(visited: Set[Position], current: Set[Position]): List[Set[Position]] =
      if current.isEmpty then List.empty
      else
        val increasing = current.flatMap { c =>
          val valueOfC = input(c)
          val newNeighbours = neighbourPositionsOf(c, input).toSet -- visited
          newNeighbours.filter {
            n =>
              val valueOfN = input(n)
              valueOfN < 9 && valueOfN > valueOfC
          }
        }
        current +: repeat(visited ++ current, increasing)

    repeat(Set.empty, Set(lowPoint)).foldLeft(Set.empty[Position])(_ | _)

  @main
  def solution1: Unit =
    val localMinPositions = input.keysIterator.filter(isLocalMinimum(_, input)).toList
    val localMinSum = localMinPositions.map(p => 1 + input(p)).sum
    pprint.log(localMinSum)

  @main
  def solution2: Unit =
    val localMinPositions = input.keysIterator.filter(isLocalMinimum(_, input)).toList
    pprint.log(flow((29, 39)))
    val basinSizes = localMinPositions.map(flow(_).size).sorted.reverse.take(3).product
    pprint.log(basinSizes)
}
