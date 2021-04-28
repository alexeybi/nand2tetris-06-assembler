package Parser

import Binary.Symbols
import Binary.Symbols.mergeSymbols
import Errors.{AppError, ConvertError, ParseError}
import Parser.Instruction.{makeFrom, makeFromTwo}
import cats.implicits.toBifunctorOps

import scala.annotation.tailrec

sealed trait Instruction {
  def makeBinary(symbols: Symbols): Either[ConvertError, String] = this match {
    case ANum(value)      => makeFrom(value, symbols)
    case AVariable(value) => makeFrom(value, symbols)
    case CCompute(dest, comp) =>
      makeFromTwo(dest, comp, symbols)((dest, comp) => comp + dest + "000")
    case CJump(comp, jump) =>
      makeFromTwo(comp, jump, symbols)((comp, jump) => comp + "000" + jump)
    case Label(_, _) => Left(ConvertError("Can't make binary from a Label"))
  }
}

object Instruction {
  def makeAll(
      xs: Either[ParseError, List[Instruction]],
      symbols: Symbols = Symbols()
  ): Either[AppError, String] = xs
    .map { list =>
      val labels = processLabels(list, symbols)
      val instructions = removeLabels(Right(list))
      val variables =
        processVariables(instructions.getOrElse(List.empty), labels)
      val allSymbols = mergeSymbols(labels, variables)

      instructions
        .getOrElse(List.empty)
        .map(_.makeBinary(allSymbols).getOrElse(""))
        .mkString("\n")
    }
    .leftMap[AppError](error => ConvertError(error.msg))

  def makeFrom(token: Token, symbols: Symbols): Either[ConvertError, String] =
    token.lookUp(symbols) match {
      case None        => Left(ConvertError("Error making binary string"))
      case Some(value) => Right(value)
    }

  def makeFromTwo(t1: Token, t2: Token, symbols: Symbols)(
      f: (String, String) => String
  ): Either[ConvertError, String] =
    makeFrom(t1, symbols).flatMap(a => makeFrom(t2, symbols).map(b => f(a, b)))

  def processVariables(
      instructions: List[Instruction],
      variables: Symbols
  ): Symbols =
    instructions
      .collect { case AVariable(token) =>
        token
      }
      .foldLeft(variables)((acc, token) =>
        acc.copy(variables = token.add(token.v, acc).variables)
      )

  def processLabels(
      instructions: List[Instruction],
      symbols: Symbols
  ): Symbols =
    instructions.view.zipWithIndex
      .collect { case (label: Label, index) =>
        processNested(label, index, symbols)
      }
      .foldLeft(symbols)((acc, s) =>
        acc.copy(symbols = acc.symbols ++ s.symbols)
      )

  def processNested(
      label: Label,
      index: Int,
      s: Symbols
  ): Symbols =
    label match {
      case Label(label, inst: Label) =>
        s.copy(symbols =
          label.add(label.v, index, s).symbols ++
            processNested(inst, index, s).symbols
        )
      case Label(label, _) => label.add(label.v, index, s)
    }

  def removeLabels(
      result: Either[ParseError, List[Instruction]]
  ): Either[ParseError, List[Instruction]] =
    result
      .map(list =>
        list.collect {
          case a: AInstruction => a
          case c: CInstruction => c
          case l: Label        => removeNested(l)
        }
      )

  @tailrec
  def removeNested(l: Label): Instruction = l.inst match {
    case l: Label => removeNested(l)
    case inst     => inst
  }
}

sealed trait AInstruction extends Instruction
case class ANum(num: NumToken) extends AInstruction
case class AVariable(symbol: VariableToken) extends AInstruction

sealed trait CInstruction extends Instruction
case class CCompute(
    destination: DestinationToken,
    computation: ComputationToken
) extends CInstruction
case class CJump(computation: ComputationToken, jump: JumpToken)
    extends CInstruction

sealed trait LabelSymbol extends Instruction
case class Label(label: LabelToken, inst: Instruction) extends LabelSymbol
