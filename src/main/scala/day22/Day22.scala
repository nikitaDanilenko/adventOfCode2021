package day22

import cats.parse.{ Parser, Parser0 }
import util.Point3D

import scala.io.Source

object Day22 {

  enum Operation:
    case Union, Difference

  object Operation {

    val parser: Parser[Operation] =
      Parser.oneOf(
        List(
          Parser.string("on").map(_ => Operation.Union),
          Parser.string("off").map(_ => Operation.Difference)
        )
      )

    def swap(operation: Operation): Operation =
      operation match {
        case Union      => Difference
        case Difference => Union
      }

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

  def combinedVolume(opCuboids: List[OpCuboid]): BigInt =

    case class Iteration(
        addedCuboids: List[OpCuboid]
    )

    /* For every A, B we have |A ∪ B| = |A| + |B| - |A ∩ B|
                         and  |A \ B| = |A| - |A ∩ B|
       This means that adding B is similar to subtracting it.

       Next, we maintain the total shape in terms of added or subtracted cuboids, whose volumes constitute the overall volume.

       For example: Suppose we have "Add C1, add C2, subtract C3"

       Step1: Volume components = [+C1]
       Step2: Volume components = [+C1, +C2, -(C1 ∩ C2)] (first formula)
       Step3:
         |Volume \ C3| = | Volume | - | Volume ∩ C3 | (second formula)
                       = | Volume | - (| C1 ∩ C3 | + | C2 ∩ C3 | - | C1 ∩ C3 ∩ C2 ∩ C3 |)
                       = | Volume | - | C1 ∩ C3 | - | C2 ∩ C3 | + | C1 ∩ C2 ∩ C3 |
         which means that we *subtract* those intersection CK with C3 that were previously *added*,
         and *add* those intersection CK with C3 that were previously *subtracted*.

         The same is true, if C3 is added, since we then have:

         |Volume ∪ C3| = | Volume | + |C3| -  | Volume ∩ C3 | (first formula)
                       = | Volume | + |C3| - (| C1 ∩ C3 | + | C2 ∩ C3 | - | C1 ∩ C3 ∩ C2 ∩ C3 |)
                       = | Volume | + |C3| - | C1 ∩ C3 | - | C2 ∩ C3 | + | C1 ∩ C2 ∩ C3 |

     */

    def step(
        iteration: Iteration,
        opCuboid: OpCuboid
    ): Iteration =
      val intersections = iteration.addedCuboids.flatMap { cuboid =>
        Cuboid
          .intersection(opCuboid.cuboid, cuboid.cuboid)
          .map(OpCuboid(Operation.swap(cuboid.operation), _))
      }

      val updatePresentCuboids: List[OpCuboid] => List[OpCuboid] = opCuboid.operation match
        case Operation.Union      => opCuboid :: _
        case Operation.Difference => identity

      Iteration(
        updatePresentCuboids(iteration.addedCuboids ++ intersections)
      )

    def iterate(
        opCuboids: List[OpCuboid]
    ): List[OpCuboid] =
      opCuboids
        .foldLeft(Iteration(List.empty))(step)
        .addedCuboids

    iterate(opCuboids).map { opCuboid =>
      val factor = opCuboid.operation match
        case Operation.Union      => 1
        case Operation.Difference => -1
      factor * Cuboid.volumeOf(opCuboid.cuboid)
    }.sum

  @main
  def solution1(): Unit =
    val boundingSide = Side(-50, 50)
    val boundingCuboid = Cuboid(boundingSide, boundingSide, boundingSide)
    val volume = combinedVolume(
      input.flatMap { opCuboid =>
        opCuboid.copy()
        Cuboid
          .intersection(opCuboid.cuboid, boundingCuboid)
          .map(OpCuboid(opCuboid.operation, _))
      }
    )
    println(volume)

  @main
  def solution2(): Unit =
    val volume = combinedVolume(input)
    println(volume)

}
