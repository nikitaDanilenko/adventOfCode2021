package day22

import cats.parse.{ Parser, Parser0 }

sealed trait Volume {
  def contains(point3D: Point3D): Boolean
}

object Volume {

  def union(volume1: Volume, volume2: Volume): Volume =
    new Volume {

      override def contains(point3D: Point3D): Boolean =
        volume1.contains(point3D) || volume2.contains(point3D)

    }

  def difference(volume1: Volume, volume2: Volume): Volume =
    new Volume {

      override def contains(point3D: Point3D): Boolean =
        volume1.contains(point3D) && !volume2.contains(point3D)

    }

  val empty: Volume = new Volume {
    override def contains(point3D: Point3D): Boolean = false
  }

  case class Cuboid(
      x: Side,
      y: Side,
      z: Side
  ) extends Volume {

    override def contains(point3D: Point3D): Boolean =
      List(
        (x, point3D.x),
        (y, point3D.y),
        (z, point3D.z)
      ).forall(Side.on.tupled)

  }

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

  }

}
