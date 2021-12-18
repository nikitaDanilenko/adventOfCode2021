package day16

import spire.math.Natural
import spire.syntax.additiveMonoid._
import spire.implicits._

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

  @main
  def solution1: Unit = {
    val result = Packet.packetParser.parse(input).map {
      case (_, p) =>
        sumVersions(p)
    }

    pprint.log(result)
  }

}
