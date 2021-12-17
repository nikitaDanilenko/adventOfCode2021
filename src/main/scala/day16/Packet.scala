package day16

import cats.parse.{ Parser, Parser0 }
import spire.math.{ Natural, e }

sealed trait Packet {
  def version: Natural
  def id: Natural
}

object Packet {

  case class Quad(
      b0: Bit,
      b1: Bit,
      b2: Bit,
      b3: Bit
  )

  case class Literal(
      override val version: Natural,
      override val id: Natural,
      number: Natural
  ) extends Packet

  case class Collection(
      override val version: Natural,
      override val id: Natural,
      packets: List[Packet]
  ) extends Packet

  val threeBitParser: Parser[Natural] = for {
    b0 <- Bit.parser
    b1 <- Bit.parser
    b2 <- Bit.parser
  } yield Bit.bitsToNatural(List(b0, b1, b2))

  val literalParser: Parser0[Literal] = {
    val quadParser: Parser[Seq[Bit]] =
      for {
        b0 <- Bit.parser
        b1 <- Bit.parser
        b2 <- Bit.parser
        b3 <- Bit.parser
      } yield Seq(b0, b1, b2, b3)

    val nonFinalQuad = Parser.char('1') *> quadParser
    val finalQuad = Parser.char('0') *> quadParser

    val valueParser = for {
      nfqs <- nonFinalQuad.rep0
      fq <- finalQuad
      _ <- Parser.char('0').rep0
    } yield Bit.bitsToNatural((nfqs :+ fq).flatten)

    for {
      version <- threeBitParser
      _ = pprint.log(version)
      id <- threeBitParser.filter(_ == Natural(4))
      _ = pprint.log(id)
      number <- valueParser
    } yield Literal(
      version = version,
      id = id,
      number = number
    )
  }

}
