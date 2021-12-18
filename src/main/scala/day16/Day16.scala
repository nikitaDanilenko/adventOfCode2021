package day16

import spire.math.Natural
import spire.syntax.additiveMonoid._
import spire.syntax.multiplicativeMonoid._
import spire.implicits._
import spire.compat._

import scala.io.Source

object Day16 {

  val input: String =
    Source
      .fromResource("Day16.txt")
      .flatMap(c => Integer.parseInt(s"$c", 16).toBinaryString.reverse.padTo(4, '0').reverse)
      .mkString

  def sumVersions(packet: Packet): Natural =
    packet match {
      case literal: Packet.Literal => literal.version
      case collection: Packet.Collection =>
        collection.version + collection.packets.map(sumVersions).qsum
    }

  def valueOf(packet: Packet): Natural =
    packet match {
      case literal: Packet.Literal =>
        literal.number
      case collection: Packet.Collection =>
        val values = collection.packets.map(valueOf)
        collection.id.toInt match {
          case 0 =>
            values.qsum
          case 1 =>
            values.qproduct
          case 2 =>
            values.min
          case 3 =>
            values.max
          case 5 =>
            values match {
              case p1 :: p2 :: _ => if p1 > p2 then Natural.one else Natural.zero
            }
          case 6 =>
            values match {
              case p1 :: p2 :: _ => if p1 < p2 then Natural.one else Natural.zero
            }
          case 7 =>
            values match {
              case p1 :: p2 :: _ => if p1 == p2 then Natural.one else Natural.zero
            }
        }

    }

  @main
  def solution1: Unit = {
    val result = Packet.packetParser.parse(input).map {
      case (_, p) =>
        sumVersions(p)
    }

    pprint.log(result)
  }

  @main
  def solution2: Unit =
    val result = Packet.packetParser.parse(input).map {
      case (rem, p) =>
        valueOf(p).toString
    }

    pprint.log(result)

}
