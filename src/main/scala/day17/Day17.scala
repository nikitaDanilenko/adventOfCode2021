package day17

import day05.Position
import monocle.syntax.all._

object Day17 {

  case class Velocity(
      xv: Int,
      yv: Int
  )

  def travel(position: Position, velocity: Velocity): (Position, Velocity) =
    val newPosition = position
      .focus(_.x).modify(_ + velocity.xv)
      .focus(_.y).modify(_ + velocity.yv)

    val newVelocity =
      velocity.focus(_.xv).modify { currentX =>
        if currentX > 0
          then currentX - 1
        else if currentX < 0
          then currentX + 1
        else currentX
      }
        .focus(_.yv).modify(_ - 1)

    (newPosition, newVelocity)

  def search(xMin: Int, xMax: Int, yMin: Int, yMax: Int): Seq[Fitting] =
    val yAdjusted = 2 * yMin.abs
    val velocities = for {
      x <- 1.to(200)
      y <- (-yAdjusted).to(yAdjusted)
    } yield Velocity(x, y)

    def checkTrajectory(velocity: Velocity) =
      val iterations = LazyList.iterate((Position(0, 0), velocity))(travel.tupled)
        .takeWhile {
          case (p, v) => p.x <= xMax && p.y >= yMin
        }
      if iterations.exists { case (p, _) => p.x >= xMin && p.y <= yMax }
        then
          val max = iterations.maxBy(_._1.y)
          Some(Fitting(iterations, max._1, max._2))
      else None

    velocities.flatMap(checkTrajectory)

  case class Fitting(
      trajectory: LazyList[(Position, Velocity)],
      maxYPosition: Position,
      maxYVelocity: Velocity
  )

  @main
  def solution1: Unit =
    val y = search(153, 199, -114, -75)
    val maxY = y.maxBy(_.maxYPosition.y)
    pprint.log(maxY)

  @main
  def solution2: Unit =
    val result = search(153, 199, -114, -75).size
    pprint.log(result)
}
