package day20

import cats.parse.Parser

enum Pixel:
  case Light, Dark

object Pixel {
  val parser: Parser[Pixel] =
    Parser.oneOf(
      List(
        Parser.char('#').map(_ => Light),
        Parser.char('.').map(_ => Dark)
      )
    )
}
