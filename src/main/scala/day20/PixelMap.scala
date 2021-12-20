package day20

import day05.Position
import day16.Bit
import spire.math.Natural
import spire.syntax.additiveMonoid.*

sealed abstract case class PixelMap(
    dimensionFrom: Int,
    dimensionTo: Int,
    pixels: Map[Position, Pixel]
)

object PixelMap {

  def fromPixels(pixels: Map[Position, Pixel]): PixelMap =
    val allIndices = pixels.keySet.flatMap(p => List(p.x, p.y))
    new PixelMap(
      dimensionFrom = allIndices.min,
      dimensionTo = allIndices.max,
      pixels = pixels
    ) {}

  private val neighbourOffsets: Vector[Position] = Vector(
    Position(-1, -1),
    Position(-1, 0),
    Position(-1, 1),
    Position(0, -1),
    Position(0, 0),
    Position(0, 1),
    Position(1, -1),
    Position(1, 0),
    Position(1, 1)
  )

  def neighbours(position: Position, pixelMap: PixelMap): Vector[Pixel] =
    neighbourOffsets.map(offset => pixelMap.pixels(offset + position))

  def toNumber(pixels: Vector[Pixel]): Natural =
    Bit.bitsToNatural(pixels.map {
      case Pixel.Light => Bit.I
      case Pixel.Dark  => Bit.O
    })

  def extendTwiceWith(key: Map[Natural, Pixel], pixelMap: PixelMap): PixelMap = {

    val first = key(Natural.zero)
    val last = key(Natural(511))

    def extendOnce(pixelMap: PixelMap, offset: Int): PixelMap =
      val extendedFrom = pixelMap.dimensionFrom - offset
      val extendedTo = pixelMap.dimensionTo + offset
      val range = extendedFrom.until(extendedTo)
      val collection = for {
        x <- range
        y <- range
      } yield {
        val position = Position(x, y)
        position -> key(toNumber(neighbours(position, pixelMap)))
      }
      new PixelMap(
        dimensionFrom = extendedFrom,
        dimensionTo = extendedTo,
        pixels = collection.toMap.withDefaultValue(first)
      ) {}

    // Why is 2 twice correct?
    val extendedTwice = extendOnce(extendOnce(pixelMap, 2), 2)
    first match {
      case Pixel.Light =>
        new PixelMap(
          extendedTwice.dimensionFrom,
          extendedTwice.dimensionTo,
          extendedTwice.pixels.withDefaultValue(last)
        ) {}
      case _ => extendedTwice
    }
  }

}
