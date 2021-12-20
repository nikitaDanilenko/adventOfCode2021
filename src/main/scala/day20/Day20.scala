package day20

import spire.math.Natural
import day05.Position

import scala.io.Source

object Day20 {

  val key: Map[Natural, Pixel] =
    Source
      .fromResource("Day20-code.txt")
      .getLines()
      .toList
      .head
      .toList
      .zipWithIndex
      .flatMap {
        case (c, i) =>
          Pixel.parser
            .parse(s"$c")
            .toOption
            .map(Natural(i) -> _._2)
      }
      .toMap

  val pixelMap: PixelMap =
    PixelMap.fromPixels(
      Source
        .fromResource("Day20-map.txt")
        .getLines()
        .toList
        .zipWithIndex
        .flatMap {
          case (l, i) =>
            l.toList.zipWithIndex.flatMap {
              case (c, j) =>
                Pixel.parser
                  .parse(s"$c")
                  .toOption
                  .map(Position(i, j) -> _._2)
            }
        }
        .toMap
        .withDefaultValue(Pixel.Dark)
    )

  @main
  def solution1: Int = {
    val twice = PixelMap.extendTwiceWith(key, pixelMap)
    val lit = twice.pixels.values.collect {
      case Pixel.Light => ()
    }.size
    pprint.log(lit)
    lit
  }

  @main
  def solution2: Int = {
    val iterated = List.iterate(pixelMap, 26)(PixelMap.extendTwiceWith(key, _)).last
    val lit = iterated.pixels.values.collect {
      case Pixel.Light => ()
    }.size
    pprint.log(lit)
    lit
  }

}
