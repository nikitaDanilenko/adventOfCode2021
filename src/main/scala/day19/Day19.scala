package day19

import util.Point3D

import scala.annotation.tailrec
import scala.io.Source
import scala.util.chaining.*

object Day19 {

  case class Scanner(
      number: Int,
      points: Set[Point3D],
      transformation: Point3D => Point3D
  )

  private def distance(point1: Point3D, point2: Point3D): BigInt =
    List(
      point1.x - point2.x,
      point1.y - point2.y,
      point1.z - point2.z
    )
      .map(_.pipe(spire.math.abs).pipe(spire.math.pow(_, 2)))
      .sum

  private def distancesInScanner(scanner: Scanner): Map[(Point3D, Point3D), BigInt] = {
    val collection = for {
      point1 <- scanner.points
      point2 <- scanner.points - point1
    } yield (point1, point2) -> distance(point1, point2)

    collection.toMap
  }

  private case class Overlap(
      scanner1: Scanner,
      scanner2: Scanner
  )

  private def hasOverlap(scanner1: Scanner, scanner2: Scanner): Boolean =
    val distancesInScanner1 = distancesInScanner(scanner1)
    val distancesInScanner2 = distancesInScanner(scanner2)
    val distances1 = distancesInScanner1.values.toSet
    val distances2 = distancesInScanner2.values.toSet
    // 66 = 12 * 11 / 2, which is the number of pairwise combinations of 12 points
    distances1.intersect(distances2).size >= 66

  enum Direction:
    case X, Y, Z

  private case class Rotation(
      direction: Direction,
      numberOfRotations: Int
  )

  private case class Difference(
      rotations: List[Rotation],
      offset: Point3D
  )

  private def rotate(
      point: Point3D,
      direction: Direction
  ): Point3D = direction match {
    case Direction.X => point.copy(x = point.y, y = -point.x)
    case Direction.Y => point.copy(y = point.z, z = -point.y)
    case Direction.Z => point.copy(x = point.z, z = -point.x)
  }

  private def rotateViaRotation(
      point: Point3D,
      rotation: Rotation
  ): Point3D =
    // One more iteration, because the iteration otherwise starts with no function application
    List
      .iterate(point, 1 + rotation.numberOfRotations)(rotate(_, rotation.direction))
      .lastOption
      .getOrElse(point)

  // Computed via brute forcing all possible rotations, and only taking the distinct results.
  private val allRotations: List[List[Rotation]] = List(
    List(
      Rotation(direction = Direction.Y, numberOfRotations = 2)
    ),
    List(
      Rotation(direction = Direction.X, numberOfRotations = 1),
      Rotation(direction = Direction.Y, numberOfRotations = 2),
      Rotation(direction = Direction.Z, numberOfRotations = 2)
    ),
    List(
      Rotation(direction = Direction.Y, numberOfRotations = 3),
      Rotation(direction = Direction.Z, numberOfRotations = 1)
    ),
    List(
      Rotation(direction = Direction.Y, numberOfRotations = 1),
      Rotation(direction = Direction.Z, numberOfRotations = 2)
    ),
    List(
      Rotation(direction = Direction.Z, numberOfRotations = 3)
    ),
    List(
    ),
    List(
      Rotation(direction = Direction.Y, numberOfRotations = 3),
      Rotation(direction = Direction.Z, numberOfRotations = 2)
    ),
    List(
      Rotation(direction = Direction.X, numberOfRotations = 1),
      Rotation(direction = Direction.Z, numberOfRotations = 3)
    ),
    List(
      Rotation(direction = Direction.Z, numberOfRotations = 2)
    ),
    List(
      Rotation(direction = Direction.Z, numberOfRotations = 1)
    ),
    List(
      Rotation(direction = Direction.X, numberOfRotations = 1),
      Rotation(direction = Direction.Y, numberOfRotations = 2),
      Rotation(direction = Direction.Z, numberOfRotations = 1)
    ),
    List(
      Rotation(direction = Direction.Y, numberOfRotations = 3),
      Rotation(direction = Direction.Z, numberOfRotations = 3)
    ),
    List(
      Rotation(direction = Direction.X, numberOfRotations = 1),
      Rotation(direction = Direction.Y, numberOfRotations = 2),
      Rotation(direction = Direction.Z, numberOfRotations = 3)
    ),
    List(
      Rotation(direction = Direction.Y, numberOfRotations = 2),
      Rotation(direction = Direction.Z, numberOfRotations = 1)
    ),
    List(
      Rotation(direction = Direction.X, numberOfRotations = 1),
      Rotation(direction = Direction.Z, numberOfRotations = 2)
    ),
    List(
      Rotation(direction = Direction.Y, numberOfRotations = 2),
      Rotation(direction = Direction.Z, numberOfRotations = 3)
    ),
    List(
      Rotation(direction = Direction.Y, numberOfRotations = 1),
      Rotation(direction = Direction.Z, numberOfRotations = 1)
    ),
    List(
      Rotation(direction = Direction.Y, numberOfRotations = 2),
      Rotation(direction = Direction.Z, numberOfRotations = 2)
    ),
    List(
      Rotation(direction = Direction.Y, numberOfRotations = 1)
    ),
    List(
      Rotation(direction = Direction.Y, numberOfRotations = 3)
    ),
    List(
      Rotation(direction = Direction.X, numberOfRotations = 1),
      Rotation(direction = Direction.Z, numberOfRotations = 1)
    ),
    List(
      Rotation(direction = Direction.X, numberOfRotations = 1)
    ),
    List(
      Rotation(direction = Direction.X, numberOfRotations = 1),
      Rotation(direction = Direction.Y, numberOfRotations = 2)
    ),
    List(
      Rotation(direction = Direction.Y, numberOfRotations = 1),
      Rotation(direction = Direction.Z, numberOfRotations = 3)
    )
  )

  private def rotateAll(
      point: Point3D,
      rotations: List[Rotation]
  ): Point3D =
    rotations.foldLeft(point) { case (point, rotation) =>
      rotateViaRotation(point, rotation)
    }

  private def findDifference(
      overlap: Overlap
  ): Option[Difference] = {
    val collection = overlap.scanner1.points
      .flatMap(first1 =>
        overlap.scanner2.points
          .flatMap(first2 =>
            allRotations.flatMap { rotations =>
              val rotated = rotateAll(first1, rotations)
              val offset = Point3D.minus(first2, rotated)

              val transformedAll1 = overlap.scanner1.points.map { point =>
                Point3D.plus(rotateAll(point, rotations), offset)
              }

              if (transformedAll1.intersect(overlap.scanner2.points).size >= 12)
                Some(Difference(rotations, offset))
              else None
            }.toSet
          )
      )
    collection.headOption
  }

  private def alignOverlapping2(
      scanners: List[Scanner]
  ): List[Scanner] = {

    case class Alignment(
        aligned: List[Scanner],
        nextToBeAligned: List[Scanner],
        notAligned: List[Scanner]
    )

    /*
     * Largely based on a description on Reddit.
     * 1. Identify scanners by unique p2 distances
     * 2. Find matching rotation, and translation for each overlapping scanner
     * 3. Rotate and translate all scanners to the same coordinate system
     * */

    @tailrec
    def iterate(alignment: Alignment): Alignment =
      alignment.nextToBeAligned match
        case Nil => alignment
        case next :: remainingToBeAligned =>
          val overlapping = alignment.notAligned.filter(hasOverlap(next, _))
          val differences =
            overlapping.flatMap(candidate => findDifference(Overlap(candidate, next)).map(candidate -> _))
          val newNotAligned =
            alignment.notAligned.filterNot(scanner => differences.exists(_._1.number == scanner.number))
          val newToBeAligned = remainingToBeAligned ++ differences.map { case (scanner, difference) =>
            scanner.copy(
              points = scanner.points.map(p => Point3D.plus(difference.offset, rotateAll(p, difference.rotations))),
              transformation =
                p => Point3D.plus(difference.offset, rotateAll(scanner.transformation(p), difference.rotations))
            )
          }
          val nextAlignment = Alignment(
            aligned = next +: alignment.aligned,
            nextToBeAligned = newToBeAligned,
            notAligned = newNotAligned
          )
          iterate(nextAlignment)

    iterate(Alignment(List.empty, scanners.take(1), scanners.tail)).aligned
  }

  val input: List[Scanner] = Source
    .fromResource("day19.txt")
    .getLines()
    .mkString("\n")
    .split("\n\n")
    .zipWithIndex
    .map { (block, index) =>
      val positions = block
        .split("\n")
        .toList
        .drop(1)
        .flatMap { line =>
          line.split(",").toList match {
            case x :: y :: z :: Nil => Some(Point3D(x.toInt, y.toInt, z.toInt))
            case _                  => None
          }
        }
      Scanner(index, positions.toSet, transformation = identity)
    }
    .toList

  @main
  def solutions(): Unit =
    val transformedScanners = alignOverlapping2(input)
    val overlaps = transformedScanners.toSet.flatMap(_.points).toList
    println(s"Solution 1: ${overlaps.size}")
    val scannerPositions = transformedScanners.map(_.transformation(Point3D(0, 0, 0)))
    val pairwiseDistances = for {
      point <- scannerPositions
      otherPoint <- scannerPositions
    } yield Point3D.manhattanDistance(point, otherPoint)
    val maximum = pairwiseDistances.max
    println(s"Solution 2: $maximum")

}
