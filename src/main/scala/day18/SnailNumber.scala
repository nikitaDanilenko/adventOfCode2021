package day18

import cats.data.NonEmptyList
import cats.parse.Parser
import cats.parse.Rfc5234.*

import scala.annotation.tailrec
import scala.util.chaining.*

sealed trait SnailNumber

object SnailNumber {
  case class Plain(int: Int) extends SnailNumber
  case class Complex(left: SnailNumber, right: SnailNumber) extends SnailNumber

  enum Direction:
    case Left, Right

  case class PlainWithPosition(
      int: Int,
      position: List[Direction]
  )

  type Positional = List[PlainWithPosition]

  def toPlainWithPositions(snailNumber: SnailNumber): Positional =
    snailNumber match
      case Plain(int) => List(PlainWithPosition(int, List.empty))
      case Complex(left, right) =>
        toPlainWithPositions(left).map(sn => sn.copy(position = Direction.Left +: sn.position))
          ++ toPlainWithPositions(right).map(sn => sn.copy(position = Direction.Right +: sn.position))

  def fromPlainWithPosition(positional: Positional): SnailNumber =
    val directionalMap = positional.map(pwp => pwp.position.toVector -> pwp).toMap
    def descendWith(path: Vector[Direction]): SnailNumber =
      directionalMap
        .get(path)
        .fold(
          SnailNumber.Complex(
            descendWith(path :+ Direction.Left),
            descendWith(path :+ Direction.Right)
          )
        ) { pwp =>
          SnailNumber.Plain(pwp.int)
        }

    descendWith(Vector.empty)

  val plainParser: Parser[Plain] = digit.map(c => s"$c".toInt.pipe(SnailNumber.Plain.apply))

  lazy val snailNumberParser: Parser[SnailNumber] = {
    val recursive = plainParser.orElse(Parser.defer(snailNumberParser))

    for {
      _ <- Parser.char('[')
      left <- recursive
      _ <- Parser.char(',')
      right <- recursive
      _ <- Parser.char(']')
    } yield SnailNumber.Complex(left, right)
  }

  def splitFirst(positional: Positional): Positional =
    positional match
      case (pwp @ PlainWithPosition(int, position)) :: tail =>
        if int > 9 then
          PlainWithPosition(int / 2, position :+ Direction.Left) :: PlainWithPosition(
            (1 + int) / 2,
            position :+ Direction.Right
          ) :: tail
        else pwp :: splitFirst(tail)
      case Nil => Nil

  case class Queue[A](
      reversedFront: List[A],
      tail: List[A]
  )

  object Queue {

    def toList[A](queue: Queue[A]): List[A] =
      queue.reversedFront.reverse ++ queue.tail

  }

  // The directions to the components of the same pair differ only by the last position.
  def initial[A](positional: List[A]): List[A] =
    positional.dropRight(1)

  def addToFirst(value: Int, positional: Positional): Positional =
    positional match
      case pwp :: tail => pwp.copy(int = pwp.int + value) :: tail
      case Nil         => Nil

  def explodeFirst(positional: Positional): Positional =
    @tailrec
    def explodeFirstQ(queue: Queue[PlainWithPosition]): Positional = queue.tail match
      case pwp1 :: pwp2 :: tail
          if pwp1.position.length == pwp2.position.length &&
            pwp1.position.length >= 5 &&
            initial(pwp1.position) == initial(pwp2.position) =>
        val explodedQueue = Queue(
          reversedFront = addToFirst(pwp1.int, queue.reversedFront),
          tail = PlainWithPosition(0, initial(pwp1.position)) :: addToFirst(pwp2.int, tail)
        )
        Queue.toList(explodedQueue)
      case pwp1 :: pwp2 :: tail =>
        explodeFirstQ(Queue(pwp1 :: queue.reversedFront, pwp2 :: tail))
      case _ =>
        Queue.toList(queue)

    explodeFirstQ(Queue(List.empty, positional))

  def reduce(snailNumber: SnailNumber): SnailNumber =
    val positional = toPlainWithPositions(snailNumber)

    @tailrec
    def reducePositional(positional: Positional): Positional =
      val exploded = explodeFirst(positional)
      lazy val split = splitFirst(positional)

      if exploded != positional then reducePositional(exploded)
      else if split != positional then reducePositional(split)
      else positional

    positional
      .pipe(reducePositional)
      .pipe(fromPlainWithPosition)

  def add(sn1: SnailNumber, sn2: SnailNumber): SnailNumber =
    reduce(SnailNumber.Complex(sn1, sn2))

  def magnitude(snailNumber: SnailNumber): BigInt = snailNumber match
    case Plain(int) => BigInt(int)
    case Complex(left, right) =>
      3 * magnitude(left) + 2 * magnitude(right)

  def sum(nonEmptyList: NonEmptyList[SnailNumber]): SnailNumber =
    nonEmptyList.tail.foldLeft(nonEmptyList.head)(add)

}
