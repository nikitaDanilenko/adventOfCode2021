package day02

import spire.math.Natural
import monocle.syntax.all.*
import cats.parse.Parser
import cats.parse.Rfc5234._

import scala.io.Source

object Day02 {

  private val input1: List[Direction] = Source
    .fromResource("Day02-1.txt")
    .getLines()
    .flatMap(Direction.parse)
    .toList

  sealed trait Direction {
    def steps: Natural
  }

  object Direction {
    case class Forward(override val steps: Natural) extends Direction
    case class Up(override val steps: Natural) extends Direction
    case class Down(override val steps: Natural) extends Direction

    private def mkParser[A](word: String, constructor: Natural => A): Parser[A] =
      Parser.string(word) *>
        Parser.char(' ') *>
        digit.rep.map(cs => constructor(Natural(cs.toList.mkString)))


    val forwardParser: Parser[Forward] =
      mkParser("forward", Forward.apply)

    val upParser: Parser[Up] =
      mkParser("up", Up.apply)

    val downParser: Parser[Down] =
      mkParser("down", Down.apply)

    val directionParser: Parser[Direction] = Parser.oneOf(List(forwardParser, upParser, downParser))

    def parse(string: String): Option[Direction] =
      directionParser.parse(string).toOption.map(_._2)

  }

  case class Position(
      forward: Natural,
      depth: Natural
  )

  def move(position: Position, direction: Direction): Option[Position] =
    direction match {
      case Direction.Forward(steps) => Some(position.focus(_.forward).modify(_ + steps))
      case Direction.Up(steps) =>
        if steps > position.depth
          then None
        else
          Some(position.focus(_.depth).modify(_ - steps))
      case Direction.Down(steps) => Some(position.focus(_.depth).modify(_ + steps))
    }

  def moveManyWith[P](position: P, directions: Seq[Direction])(m: (P, Direction) => Option[P]): P =
    directions.foldLeft(position) {
      (position, direction) =>
        m(position, direction).getOrElse(position)
    }

  case class AimPosition(
      forward: Natural,
      depth: Natural,
      aim: Natural
  )

  def moveAim(aimPosition: AimPosition, direction: Direction): Option[AimPosition] =
    direction match {
      case Direction.Forward(steps) =>
        Some(
          aimPosition
            .focus(_.forward)
            .modify(_ + steps)
            .focus(_.depth)
            .modify(_ + aimPosition.aim * steps)
        )
      case Direction.Up(steps) =>
        if steps > aimPosition.aim
        then None
        else
          Some(aimPosition.focus(_.aim).modify(_ - steps))
      case Direction.Down(steps) =>
        Some(aimPosition.focus(_.aim).modify(_ + steps))
    }

  @main
  def solution1: Unit =
    val target = moveManyWith(Position(Natural.zero, Natural.zero), input1)(move)
    val result = target.depth * target.forward
    pprint.log(result)

  @main
  def solution2: Unit =
    val target = moveManyWith(AimPosition(Natural.zero, Natural.zero, Natural.zero), input1)(moveAim)
    val result = target.depth * target.forward
    pprint.log(result)

}
