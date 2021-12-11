package day11

import day09.Day09.{Position, add}
import spire.math.Natural
import monocle.syntax.all.*

import scala.annotation.tailrec
import scala.io.Source

object Day11 {

  type OctoMap = Map[Position, Octopus]

  val input: OctoMap =
    Source
      .fromResource("Day11.txt")
      .getLines()
      .zipWithIndex
      .flatMap {
        case (line, i) =>
          line.zipWithIndex.flatMap {
            case (c, j) =>
              Octopus.parser.parse(List(c).mkString).toOption.map(r => (i, j) -> r._2)
          }
      }
      .toMap

  val neighbourOffsets: Vector[Position] = Vector(
    (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1)
  )

  def neighboursOf(position: Position, octoMap: OctoMap): Vector[(Position, Octopus)] =
    neighbourOffsets.flatMap { offset =>
      val neighbourPosition = add(position, offset)
      octoMap.get(neighbourPosition).map(neighbourPosition -> _)
    }

  def flash(octoMap: OctoMap, step: Int): OctoMap =
    val tooHigh = octoMap.toList.collect {
      case (position, octopus) if octopus.energy > Natural(9) =>
        position ->
          octopus
            .focus(_.energy)
            .set(Natural.zero)
            .focus(_.flashedInRounds)
            .modify(_ + step)
    }
    val neighbours = tooHigh.flatMap { case (position, _) =>
        neighboursOf(position, octoMap)
    }

    val octoMap2 = tooHigh.foldLeft(octoMap) { case (om, (p, octopus)) =>
      om.updated(p, octopus)
    }

    neighbours.foldLeft(octoMap2) { case (om, (pos, _)) =>
      val currentOctopus = om(pos)
      val nextOctopus =
        if currentOctopus.flashedInRounds.contains(step)
          then currentOctopus
        else currentOctopus.focus(_.energy).modify(_ + Natural.one)
      om.updated(pos, nextOctopus)
    }

  @tailrec
  def iterateFlash(octoMap: OctoMap, step: Int): OctoMap =
    if octoMap.exists(_._2.energy > Natural(9))
      then
        iterateFlash(flash(octoMap, step), step)
    else octoMap

  def processStep(octoMap: OctoMap, step: Int): OctoMap =
    iterateFlash(
      octoMap.view
        .mapValues(_.focus(_.energy).modify(_ + Natural.one))
        .toMap,
      step
    )

  @main
  def solution1: Unit =
//    val m = 1.to(10).foldLeft(input)(processStep).toList.sortBy(_._1._1).grouped(10)
//      .map { l =>
//        l.sortBy(_._1).map(_._2.energy.toString).mkString
//      }.mkString("\n")
//    println(m)
    val result = 1.to(100).foldLeft(input)(processStep).map(_._2.flashedInRounds.size).sum
    pprint.log(result)

  @tailrec
  def searchStep(octoMap: OctoMap, currentStep: Int): Int =
    val intersection = octoMap.map(_._2.flashedInRounds).foldLeft(1.to(currentStep).toSet)(_ & _)
    if intersection.nonEmpty then currentStep - 1 else searchStep(processStep(octoMap, 1 + currentStep), 1 + currentStep)

  @main
  def solution2: Unit =
    val step = searchStep(input, 1)
    pprint.log(step)
}
