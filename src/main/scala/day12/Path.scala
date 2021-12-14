package day12

import cats.Show
import cats.data.NonEmptyList
import cats.syntax.show.*

case class Path[A](
    nodes: NonEmptyList[A]
)

object Path {

  implicit def showPath[A: Show]: Show[Path[A]] =
    Show.show(_.nodes.toList.reverse.map(_.show).mkString(","))

}
