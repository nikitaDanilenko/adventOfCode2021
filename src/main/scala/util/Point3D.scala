package util

case class Point3D(
    x: Int,
    y: Int,
    z: Int
)

object Point3D {

  def minus(point1: Point3D, point2: Point3D): Point3D = Point3D(
    x = point1.x - point2.x,
    y = point1.y - point2.y,
    z = point1.z - point2.z
  )

  def plus(point1: Point3D, point2: Point3D): Point3D = Point3D(
    x = point1.x + point2.x,
    y = point1.y + point2.y,
    z = point1.z + point2.z
  )

  def manhattanDistance(point1: Point3D, point2: Point3D): Int =
    List(
      point1.x - point2.x,
      point1.y - point2.y,
      point1.z - point2.z
    )
      .map(Math.abs)
      .sum

}
