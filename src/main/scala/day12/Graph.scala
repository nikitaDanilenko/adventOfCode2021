package day12

import cats.parse.Parser

case class Graph[A](
    edges: Vector[Edge[A]]
)

object Graph {

  def successorsOf[A](graph: Graph[A])(node: A): Vector[A] =
    graph.edges.collect {
      case Edge(from, to) if from == node => to
    }

  def mkSymmetric[A](graph: Graph[A]): Graph[A] =
    val reversed = graph.edges.map(Edge.flip)
    Graph(graph.edges ++ reversed)

  val stringParser: Parser[Graph[String]] =
    Edge.stringParser.repSep(Parser.char(' ')).map(es => Graph(es.iterator.toVector))

}
