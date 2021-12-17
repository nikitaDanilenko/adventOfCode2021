package day15

import algebra.ring.AdditiveMonoid
import cats.Order
import spire.compat.*
import spire.implicits.*
import spire.syntax.additiveMonoid.*
import util.Measure

import scala.annotation.tailrec

case class WeightedGraph[A](
    adjacency: Map[A, Map[A, Int]]
)

object WeightedGraph {

  def verticesOf[A](graph: WeightedGraph[A]): Set[A] =
    graph.adjacency.keySet

  def successorsOf[A](graph: WeightedGraph[A])(node: A): Set[A] =
    graph.adjacency.get(node).fold(Set.empty[A])(_.keySet)

  case class WeightedPath[A](
      vertices: Vector[A],
      weight: Tropical[Int]
  )

  def weightOf[A](weightedGraph: WeightedGraph[A])(from: A, to: A): Tropical[Int] =
    weightedGraph.adjacency.get(from).flatMap { ws =>
      ws.get(to)
    }.fold(Tropical.Infinity: Tropical[Int])(Tropical.Number(_))

  def dijkstra[A: Order](weightedGraph: WeightedGraph[A])(source: A, target: A): Map[A, Tropical[Int]] =

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
            neighbours.foldLeft(List.empty[(A, Tropical[Int])], Vector.empty[(A, A)]) { case ((ds, ps), n) =>
              val weightToN = smallestWeight + weightOf(weightedGraph)(smallestVertex, n)
              if weightToN < distances(n) then
                ((n -> weightToN) +: ds, (n -> smallestVertex) +: ps)
              else (ds, ps)
            }

          processNext(remainingQueue, distances ++ updatedDistances.toMap, predecessors ++ updatedPredecessors.toMap)

    val vertices = verticesOf(weightedGraph)

    val iterated = processNext(
      vertices,
      Map(source -> Tropical.Number(0)).withDefault(_ => Tropical.Infinity),
      Map.empty
    )

    iterated._1


}
