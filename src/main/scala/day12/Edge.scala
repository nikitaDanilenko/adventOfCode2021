package day12

import cats.parse.Parser
import cats.parse.Rfc5234._

case class Edge[A](
    from: A,
    to: A
)

object Edge {

  val stringParser: Parser[Edge[String]] = for {
    from <- alpha.rep
    _ <- Parser.char('-')
    to <- alpha.rep
  } yield Edge(
    from = from.toList.mkString,
    to = to.toList.mkString
  )

  def flip[A](edge: Edge[A]): Edge[A] =
    Edge(
      from = edge.to,
      to = edge.from
    )

}
