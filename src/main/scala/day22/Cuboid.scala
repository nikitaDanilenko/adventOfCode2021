package day22

import cats.parse.{ Parser, Parser0 }
import util.Point3D

case class Cuboid(
    x: Side,
    y: Side,
    z: Side
)

object Cuboid {

  val parser: Parser0[Cuboid] = for {
    _ <- Parser.string("x=")
    xSide <- Side.parser
    _ <- Parser.char(',')
    _ <- Parser.string("y=")
    ySide <- Side.parser
    _ <- Parser.char(',')
    _ <- Parser.string("z=")
    zSide <- Side.parser
  } yield Cuboid(
    x = xSide,
    y = ySide,
    z = zSide
  )

  // Option, because the intersection of two cuboids may be empty.
  // TODO: Is a designated empty cuboid more declarative?
  def intersection(cuboid1: Cuboid, cuboid2: Cuboid): Option[Cuboid] =
    for {
      x <- Side.intersection(cuboid1.x, cuboid2.x)
      y <- Side.intersection(cuboid1.y, cuboid2.y)
      z <- Side.intersection(cuboid1.z, cuboid2.z)
    } yield Cuboid(
      x = x,
      y = y,
      z = z
    )

  def volumeOf(cuboid: Cuboid): BigInt =
    List(
      cuboid.x,
      cuboid.y,
      cuboid.z
    )
      .map(Side.length)
      .product

}
