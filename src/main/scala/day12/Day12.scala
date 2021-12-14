package day12

import cats.data.NonEmptyList

import scala.annotation.tailrec
import scala.io.Source

object Day12 {

  val input: Graph[String] = Graph.mkSymmetric(Graph.stringParser.parse(Source.fromResource("Day12.txt")
    .getLines()
    .mkString(" ")
  ).map(_._2)
    .getOrElse(Graph(Set.empty)))

  def prolong[A](
    path: Path[A]
  )(
    graph: Graph[A],
    allowRepetition: A => Boolean
  ): Set[Path[A]] =
    val current = path.nodes.head
    val next =
      Graph.successorsOf(graph)(current) --
        path.nodes.iterator.filter { n =>
          !allowRepetition(n) }.toSet
    next.map(n => Path(n :: path.nodes))

  def iterateFrom[A](
    start: A
  )(
    graph: Graph[A],
    isTarget: A => Boolean,
    allowRepetition: A => Boolean
  ): Set[Path[A]] =
    def increase(paths: Set[Path[A]]): Set[Path[A]] =
      val next = paths.flatMap(prolong(_)(graph, allowRepetition))
      if next == paths then next else paths ++ increase(next)

    increase(Set(Path(nodes = NonEmptyList.of(start)))).filter(p => isTarget(p.nodes.head))

  @main
  def solution1: Unit =
    val paths = iterateFrom("start")(input, _ == "end", _.forall(_.isUpper))
    pprint.log(paths.size)
//    pprint.log(paths)

}
