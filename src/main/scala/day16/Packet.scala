package day16

import cats.parse.{ Parser, Parser0 }
import spire.math.{ Natural, e }
import cats.syntax.traverse._

sealed trait Packet {
  def version: Natural
  def id: Natural
  def length: Int
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
      override val length: Int,
      number: Natural
  ) extends Packet

  case class Collection(
      override val version: Natural,
      override val id: Natural,
      override val length: Int,
      packets: List[Packet]
  ) extends Packet

  val threeBitParser: Parser[Natural] = for {
    b0 <- Bit.parser
    b1 <- Bit.parser
    b2 <- Bit.parser
  } yield Bit.bitsToNatural(List(b0, b1, b2))

  val quadParser: Parser0[List[Bit]] =
    List.fill(4)(Bit.parser).sequence

  val literalParser: Parser0[Literal] = {
    val nonFinalQuad = Parser.char('1') *> quadParser
    val finalQuad = Parser.char('0') *> quadParser

    val valueParser = for {
      nfqs <- nonFinalQuad.rep0
      fq <- finalQuad
    } yield (Bit.bitsToNatural((nfqs :+ fq).flatten), (1 + nfqs.length) * 5)

    for {
      version <- threeBitParser
      id <- threeBitParser.filter(_ == Natural(4)).backtrack
      numberWithLength <- valueParser
    } yield Literal(
      version = version,
      id = id,
      length = 6 + numberWithLength._2,
      number = numberWithLength._1
    )
  }

  val collectionParser: Parser[Collection] = {
    for {
      version <- threeBitParser
      id <- threeBitParser.filter(_ != Natural(4)).backtrack
      tpe <- Bit.parser
      collection <- {
        tpe match {
          case Bit.I =>
            for {
              numberOfPacketsInBits <- List.fill(11)(Bit.parser).sequence
              numberOfPackets = Bit.bitsToNatural(numberOfPacketsInBits)
              packets <- List.fill(numberOfPackets.toInt)(packetParser).sequence
            } yield
              (packets, 11)
          case Bit.O =>

            def repeat(remainingLength: Int): Parser0[List[Packet]] =
              if remainingLength > 0 then
                for {
                  packet <- packetParser.filter(_.length <= remainingLength)
                  packets <- repeat(remainingLength - packet.length)
                } yield
                  packet :: packets
              else Parser.pure(List.empty)

            for {
              overallLengthInBits <- List.fill(15)(Bit.parser).sequence
              overallLength = Bit.bitsToNatural(overallLengthInBits)
              packets <- repeat(overallLength.toInt)
            } yield (packets, 15)
        }
      }
    } yield Collection(
      version = version,
      id = id,
      length = 7 + collection._2 + collection._1.map(_.length).sum,
      packets = collection._1
    )
  }

  lazy val packetParser: Parser0[Packet] = literalParser.backtrack.orElse(collectionParser)

}
