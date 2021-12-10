package day10

import cats.parse.Parser

case class Brace(
    symbol: Symbol,
    direction: Direction
)

object Brace {

  val parser: Parser[Brace] =
    Parser.oneOf(
      List(
        Parser.char('<').map(_ => Brace(Symbol.Angle, Direction.Open)),
        Parser.char('>').map(_ => Brace(Symbol.Angle, Direction.Close)),
        Parser.char('(').map(_ => Brace(Symbol.Parenthesis, Direction.Open)),
        Parser.char(')').map(_ => Brace(Symbol.Parenthesis, Direction.Close)),
        Parser.char('{').map(_ => Brace(Symbol.Curly, Direction.Open)),
        Parser.char('}').map(_ => Brace(Symbol.Curly, Direction.Close)),
        Parser.char('[').map(_ => Brace(Symbol.Square, Direction.Open)),
        Parser.char(']').map(_ => Brace(Symbol.Square, Direction.Close))
      )
    )

}
