package day24

import cats.parse.Parser

import scala.io.Source
sealed trait Register

object Register {
  case object W extends Register
  case object X extends Register
  case object Y extends Register
  case object Z extends Register

  val parser: Parser[Register] = Parser.oneOf(
    List(
      Parser.char('w').as(W),
      Parser.char('x').as(X),
      Parser.char('y').as(Y),
      Parser.char('z').as(Z)
    )
  )

}

sealed trait Operand

object Operand {
  case class RegisterReference(register: Register) extends Operand
  case class Value(int: Int) extends Operand

  val parser: Parser[Operand] =
    Register.parser
      .map(RegisterReference.apply)
      .backtrack
      .orElse(
        day05.nonNegativeIntParser.map(Value.apply)
      )

}

sealed trait Instruction

object Instruction {
  case class Inp(register: Register) extends Instruction
  case class Add(register: Register, operand: Operand) extends Instruction
  case class Mul(register: Register, operand: Operand) extends Instruction
  case class Div(register: Register, operand: Operand) extends Instruction
  case class Mod(register: Register, operand: Operand) extends Instruction
  case class Eql(register: Register, operand: Operand) extends Instruction

  val spaces = Parser.char(' ').rep

  val registerWithOperandParser: Parser[(Register, Operand)] =
    for {
      register <- Register.parser
      _ <- spaces
      operand <- Operand.parser
    } yield (register, operand)

  val parser: Parser[Instruction] = Parser.oneOf(
    List(
      (Parser.string("inp") *> spaces *> Register.parser).map(Inp.apply),
      (Parser.string("add") *> spaces *> registerWithOperandParser).map(Add.apply),
      (Parser.string("mul") *> spaces *> registerWithOperandParser).map(Mul.apply),
      (Parser.string("div") *> spaces *> registerWithOperandParser).map(Div.apply),
      (Parser.string("mod") *> spaces *> registerWithOperandParser).map(Mod.apply),
      (Parser.string("eql") *> spaces *> registerWithOperandParser).map(Eql.apply)
    )
  )

}

val input = Source
  .fromResource("day24test.txt")
  .getLines()
  .flatMap(Instruction.parser.parseAll(_).toOption)
  .toList

@main
def solution1: Unit =
  pprint.pprintln(input)
