package day25

import cats.parse.Parser

object Day25 {

  case class Pos(
      line: Int,
      column: Int
  )

  enum Direction {
    case East, South
  }

  object Direction {

    val parser: Parser[Direction] = {
      val east = Parser.string(">").as(Direction.East)
      val south = Parser.string("v").as(Direction.South)
      east | south
    }

  }

  case class Cucumber(
      direction: Direction,
      position: Pos
  )

  case class Seafloor(
      cucumbers: Vector[Cucumber],
      width: Int,
      height: Int
  )

  lazy val input: Seafloor = {
    val lines = io.Source
      .fromResource("day25.txt")
      .getLines()
      .toVector

    val cucumbers = lines.zipWithIndex
      .flatMap { (line, lineNumber) =>
        line.zipWithIndex.flatMap { (char, columnNumber) =>
          val position = Pos(line = lineNumber, column = columnNumber)
          Direction.parser
            .parseAll(char.toString)
            .toOption
            .map { direction =>
              Cucumber(direction, Pos(lineNumber, columnNumber))
            }

        }
      }

    Seafloor(
      cucumbers = cucumbers,
      width = lines.head.length,
      height = lines.length
    )
  }

  case class Groups(
      east: Vector[Cucumber],
      south: Vector[Cucumber]
  )

  private def move(width: Int, height: Int, groups: Groups): Groups = {
    val filledBeforeStep = (groups.east ++ groups.south).map(_.position).toSet

    val movedEast = groups.east.map { cucumber =>
      val nextPosCandidate =
        if (cucumber.position.column < width - 1)
          cucumber.position.copy(column = cucumber.position.column + 1)
        else
          cucumber.position.copy(column = 0)

      if (filledBeforeStep.contains(nextPosCandidate))
        cucumber
      else
        cucumber.copy(position = nextPosCandidate)
    }

    val filledAfterEast = (movedEast ++ groups.south).map(_.position).toSet

    val movedSouth = groups.south.map { cucumber =>
      val nextPosCandidate =
        if (cucumber.position.line < height - 1)
          cucumber.position.copy(line = cucumber.position.line + 1)
        else
          cucumber.position.copy(line = 0)

      if (filledAfterEast.contains(nextPosCandidate))
        cucumber
      else
        cucumber.copy(position = nextPosCandidate)
    }

    Groups(
      movedEast,
      movedSouth
    )
  }

  private def moveUntilStop(width: Int, height: Int, groups: Groups): Int =
    LazyList
      .iterate(groups)(move(width, height, _))
      .zipWithIndex
      .sliding(2)
      .dropWhile(gs => gs.head._1 != gs.last._1)
      .take(1)
      .toList
      .head
      .head
      ._2 + 1

  @main
  def solution1(): Unit =
    val (east, south) = input.cucumbers.partitionMap { cucumber =>
      cucumber.direction match {
        case Direction.East =>
          Left(cucumber)
        case Direction.South =>
          Right(cucumber)
      }
    }
    val groups = Groups(east, south)
    val result = moveUntilStop(input.width, input.height, groups)
    println(result)

}
