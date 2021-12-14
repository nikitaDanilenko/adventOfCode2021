package day12

import cats.data.NonEmptyList

case class Path[A](
    nodes: NonEmptyList[A]
)
