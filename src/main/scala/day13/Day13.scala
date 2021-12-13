package day13

import day05.Position
import monocle.syntax.all.*

import scala.io.{BufferedSource, Source}

object Day13 {

  case class DotMap(
      dots: Set[Position],
      verticalSize: Int,
      horizontalSize: Int
  )

  def splitVertically(dotMap: DotMap): (DotMap, DotMap) =
    val newVerticalSize = dotMap.verticalSize / 2
    val preparedNewMap = dotMap.focus(_.verticalSize).set(newVerticalSize)
    val left = preparedNewMap
      .focus(_.dots).modify(_.filter(_.y <= newVerticalSize) )
    val right = preparedNewMap
      .focus(_.dots).modify(_.collect { case position if position.y > newVerticalSize =>
      position.focus(_.y).modify(_ - newVerticalSize)}
    )
    (left, right)

  def splitHorizontally(dotMap: DotMap): (DotMap, DotMap) =
    val newHorizontalSize = dotMap.horizontalSize / 2
    val preparedNewMap = dotMap.focus(_.horizontalSize).set(newHorizontalSize)
    val top = preparedNewMap
      .focus(_.dots).modify(_.filter(_.x <= newHorizontalSize))
    val bottom = preparedNewMap
      .focus(_.dots).modify(_.collect { case position if position.x > newHorizontalSize =>
      position.focus(_.x).modify(_ - newHorizontalSize)}
    )
    (top, bottom)

  def flipVertically(dotMap: DotMap): DotMap =
    dotMap.focus(_.dots).modify(_.map { position =>
      position.focus(_.y).modify(dotMap.verticalSize - _)
    })

  def flipHorizontally(dotMap: DotMap): DotMap =
    dotMap.focus(_.dots).modify(_.map { position =>
      position.focus(_.x).modify(dotMap.horizontalSize - _)
    })

  def overlayVertically(dotMap: DotMap): DotMap =
    val (left, right) = splitVertically(dotMap)
    val newRight = flipVertically(right)
    left.focus(_.dots).modify(newRight.dots | _)

  def overlayHorizontally(dotMap: DotMap): DotMap =
    val (top, bottom) = splitHorizontally(dotMap)
    val newBottom = flipHorizontally(bottom)
    top.focus(_.dots).modify(newBottom.dots | _)

  def parseDotMap(bufferedSource: BufferedSource): DotMap =
    val dots = bufferedSource.getLines()
      .flatMap {
      l =>
        Some(l.split(",").toList).collect {
          case x :: y :: _ => Position(x.toInt, y.toInt)
        }
    }.toSet
    val verticalSize = 1 + dots.maxBy(_.y).y
    val horizontalSize = 1 + dots.maxBy(_.x).x
    DotMap(
      dots = dots,
      verticalSize = verticalSize,
      horizontalSize = horizontalSize
    )

  val input: DotMap = parseDotMap(Source.fromResource("Day13-map.txt"))

  @main
  def solution1: Unit =
    val result = overlayHorizontally(input)
    pprint.log(result.dots.size)

  def show(dotMap: DotMap): Unit =
    val display = for {
      j <- 0.until(dotMap.verticalSize)
    } yield {
      0.until(dotMap.horizontalSize).map(i => if dotMap.dots.contains(Position(i, j)) then "#" else ".").mkString
    }
    println(display.mkString("\n"))

  @main
  def solution2: Unit =
    val directions: Vector[DotMap => DotMap] = Vector(
      overlayHorizontally,
      overlayVertically,
      overlayHorizontally,
      overlayVertically,
      overlayHorizontally,
      overlayVertically,
      overlayHorizontally,
      overlayVertically,
      overlayHorizontally,
      overlayVertically,
      overlayVertically,
      overlayVertically
    )
    val result = directions.foldLeft(input)((map, fold) => fold(map))
    show(result)
}
