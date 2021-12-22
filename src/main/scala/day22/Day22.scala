package day22

import cats.parse.{Parser, Parser0}
import day22.Volume.Cuboid

import scala.io.Source

object Day22 {

  enum Operation:
    case Union, Difference

  object Operation {
    val parser: Parser[Operation] =
      Parser.oneOf(
        List(
          Parser.string("on").map(_ => Operation.Union),
          Parser.string("off").map(_ => Operation.Difference),
        )
      )
  }

  case class OpCuboid(
    operation: Operation,
    cuboid: Cuboid
  )

  object OpCuboid {
    val parser: Parser0[OpCuboid] = for {
      operation <- Operation.parser
      _ <- Parser.char(' ').rep
      cuboid <- Cuboid.parser
    } yield OpCuboid(operation, cuboid)
  }

  val input: List[OpCuboid] =
    Source
      .fromResource("Day22.txt")
      .getLines()
      .flatMap(OpCuboid.parser.parse(_).toOption.map(_._2))
      .toList

  def combinedVolume(opCuboids: List[OpCuboid]): Volume =
    opCuboids.foldLeft(Volume.empty) { (v, oc) =>
      oc.operation match {
        case Operation.Union => Volume.union(v, oc.cuboid)
        case Operation.Difference => Volume.difference(v, oc.cuboid)
      }
    }

  @main
  def solution1: Int =
    val volume = combinedVolume(input)
    def rangeIterator = (-50).to(50).iterator

    val points = for {
      x <- rangeIterator
      y <- rangeIterator
      z <- rangeIterator
    } yield Point3D(x, y, z)

    val inside = points.count(volume.contains)
    pprint.log(inside)
    inside

}
