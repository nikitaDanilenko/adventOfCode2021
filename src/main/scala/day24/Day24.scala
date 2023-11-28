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

val intParser = day05.nonNegativeIntParser.orElse(
  Parser.char('-') *> day05.nonNegativeIntParser.map(-_)
)

sealed trait Operand

object Operand {
  case class RegisterReference(register: Register) extends Operand
  case class Value(int: Int) extends Operand

  val parser: Parser[Operand] =
    Register.parser
      .map(RegisterReference.apply)
      .backtrack
      .orElse(
        intParser.map(Value.apply)
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

lazy val input = Source
  .fromResource("day24.txt")
  .getLines()
  .flatMap(Instruction.parser.parseAll(_).toOption)
  .toList

def valueOf(operand: Operand, registers: Map[Register, Int]): Int =
  operand match
    case Operand.RegisterReference(register) => registers(register)
    case Operand.Value(int)                  => int

case class State(
    registers: Map[Register, Int],
    numbers: List[Int]
)

object State {

  val initialRegisters: Map[Register, Int] = Map(
    Register.W -> 0,
    Register.X -> 0,
    Register.Y -> 0,
    Register.Z -> 0
  )

}

def interpret(
    instruction: Instruction,
    state: State
): State =
  val updatedNumbers = instruction match
    case Instruction.Inp(_) => state.numbers.tail
    case _                  => state.numbers

  val updatedRegisters =
    instruction match
      case Instruction.Inp(register) =>
        state.registers.updated(register, state.numbers.head)
      case Instruction.Add(register, operand) =>
        val value = valueOf(operand, state.registers)
        state.registers.updated(register, state.registers(register) + value)
      case Instruction.Mul(register, operand) =>
        val value = valueOf(operand, state.registers)
        state.registers.updated(register, state.registers(register) * value)
      case Instruction.Div(register, operand) =>
        val value = valueOf(operand, state.registers)
        state.registers.updated(register, state.registers(register) / value)
      case Instruction.Mod(register, operand) =>
        val value = valueOf(operand, state.registers)
        state.registers.updated(register, state.registers(register) % value)
      case Instruction.Eql(register, operand) =>
        val value = valueOf(operand, state.registers)
        state.registers.updated(register, if state.registers(register) == value then 1 else 0)

  State(
    updatedRegisters,
    updatedNumbers
  )

def interpretAll(instructions: List[Instruction], numbers: List[Int]): State =
  instructions.foldLeft(State(State.initialRegisters, numbers)) { (state, instruction) =>
    interpret(instruction, state)
  }

@main
def solution1: Unit =
  println(input)
