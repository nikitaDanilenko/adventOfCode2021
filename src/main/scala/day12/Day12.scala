package day12

import cats.data.NonEmptyList
import cats.syntax.show.*
import monocle.syntax.all.*

import scala.annotation.tailrec
import scala.io.Source

object Day12 {

  val input: Graph[String] = Graph.mkSymmetric(Graph.stringParser.parse(Source.fromResource("Day12.txt")
    .getLines()
    .mkString(" ")
  ).map(_._2)
    .getOrElse(Graph(Vector.empty)))

  def prolong[A](
    path: Path[A]
  )(
    graph: Graph[A],
    allowRepetition: A => Boolean
  ): Vector[Path[A]] =
    val current = path.nodes.head
    val next =
      Graph.successorsOf(graph)(current).diff(
        path.nodes.iterator.filter { n =>
          !allowRepetition(n) }.toVector)
    next.map(n => Path(n :: path.nodes))

  def iterateFrom[A](
    start: A
  )(
    graph: Graph[A],
    isTarget: A => Boolean,
    allowRepetition: A => Boolean
  ): Vector[Path[A]] =
    def increase(paths: Vector[Path[A]]): Vector[Path[A]] =
      val next = paths.flatMap(prolong(_)(graph, allowRepetition))
      if next == paths then next else paths ++ increase(next)

    increase(Vector(Path(nodes = NonEmptyList.of(start)))).filter(p => isTarget(p.nodes.head))

  @main
  def solution1: Unit =
    val paths = iterateFrom("start")(input, _ == "end", _.forall(_.isUpper))
    pprint.log(paths.size)

  @main
  def solution2: Unit =
    val all = input.edges
      .collect { case e if
        (Set(e.from, e.to) & Set("start", "end")).isEmpty &&
          e.from.forall(_.isLower)
        =>
        Vector(e, Edge.flip(e))
      }
      .flatten
      .toSet
      .flatMap { newEdge =>
        val newGraph = input.focus(_.edges).modify(_ ++ Set(newEdge))
        iterateFrom("start")(newGraph, _ == "end", _.forall(_.isUpper)).toSet
      }
    pprint.log(all.size)


}
