package day15

import algebra.ring.AdditiveMonoid
import cats.Order
import spire.compat.*
import spire.implicits.*
import spire.syntax.additiveMonoid.*

import scala.annotation.tailrec

case class WeightedGraph[A](
    edges: Set[WeightedEdge[A]]
)

object WeightedGraph {

  def verticesOf[A](graph: WeightedGraph[A]): Set[A] =
    graph.edges
      .flatMap(e => Set(e.from, e.to))

  def successorsOf[A](graph: WeightedGraph[A])(node: A): Set[A] =
    graph.edges
      .collect {
        case WeightedEdge(from, to, _) if from == node => to
      }

  case class WeightedPath[A](
      vertices: Vector[A],
      weight: Tropical[Int]
  )

  def weightOf[A](weightedGraph: WeightedGraph[A])(from: A, to: A): Tropical[Int] =
    weightedGraph.edges
      .collectFirst { case e if e.from == from && e.to == to => Tropical.Number(e.weight) }
      .getOrElse(Tropical.Infinity)

  def dijkstra[A](weightedGraph: WeightedGraph[A])(source: A, target: A): Map[A, Tropical[Int]] =

    @tailrec
    def processNext(
      queue: Set[A],
      distances: Map[A, Tropical[Int]],
      predecessors: Map[A, A]
    ): (Map[A, Tropical[Int]], Map[A, A]) =
      if queue.isEmpty then
        (distances, predecessors)
      else
        val (smallestVertex, smallestWeight) = queue.map(n => (n, distances(n))).minBy(_._2)

        if smallestVertex == target then
          (distances, predecessors)

        else
          val remainingQueue = queue - smallestVertex
          val neighbours = successorsOf(weightedGraph)(smallestVertex).intersect(remainingQueue)
          val (updatedDistances, updatedPredecessors) =
            neighbours.foldLeft((distances, predecessors)) {
              case ((ds, ps), n) =>
                val weightToN = smallestWeight + weightOf(weightedGraph)(smallestVertex, n)
                if weightToN < ds(n) then
                  (ds.updated(n, weightToN), ps.updated(n, smallestVertex))
                else
                  (ds, ps)
            }
          processNext(remainingQueue, updatedDistances, updatedPredecessors)

    val vertices = verticesOf(weightedGraph)

    val iterated = processNext(
      vertices,
      vertices.map(_ -> Tropical.Infinity)
        .toMap
        .updated(source, Tropical.Number(0)),
      Map.empty
    )

    iterated._1


}
