package Parser

/* Grammar:
 *   <Instruction> ::= {<AInstruction> | <CInstruction> | <Label>}
 *   <AInstruction> ::= "@" <Number> | "@" <Symbol>
 *   <CInstruction> ::= <Destination> "=" <Computation> | <Destination> ";" <Jump>
 *   <Label> ::= <Label Symbol> (<AInstruction> | <CInstruction> | <Label>)
 *   <Symbol> ::= <Predefined Constant> | <Variable Alphanumeric Symbol> | <Number>
 *   <Label Symbol> ::= "(" <Variable Alphanumeric Symbol> ")"
 *   <Predefined Constant> ::= {A-Z}
 *   <Variable Alphanumeric Symbol> ::= [\w]+(?:.[\w]+)*
 *   <Number> ::= {0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9}
 */

import Errors.ParseError

import scala.util.matching.Regex
import scala.util.parsing.combinator._

object Parse extends RegexParsers with PackratParsers {
  override val whiteSpace: Regex = """(\s|//.*\r?\n?)+""".r

  def apply(source: String): Either[ParseError, List[Instruction]] =
    parseAll(instructions, source) match {
      case NoSuccess(msg, _) =>
        Left(ParseError(msg))
      case Success(result, _) =>
        Right(result)
    }

  lazy val label: PackratParser[Label] =
    ("(" ~> labelSymbol <~ ")") ~ (instructionC | instructionA | label) ^^ {
      case label ~ inst => Label(label, inst)
    }

  def instructions: Parser[List[Instruction]] = rep1(
    instructionA | instructionC | label
  )

  def instructionA: Parser[AInstruction] =
    "@" ~> num ^^ (num => ANum(num)) | "@R" ~> num ^^ (rnum =>
      ANum(rnum)
    ) | "@" ~> variable ^^ (symbol => AVariable(symbol))

  def instructionC: Parser[CInstruction] =
    (destination ~ "=" ~ computation) ^^ { case dest ~ "=" ~ comp =>
      CCompute(dest, comp)
    } |
      (computation ~ ";" ~ jump) ^^ { case comp ~ ";" ~ jump =>
        CJump(comp, jump)
      }

  def num: Parser[NumToken] = "[0-9]+".r ^^ (str => NumToken(str))

  def variable: Parser[VariableToken] =
    "[\\w]+(?:.[\\w]+)*".r ^^ (str => VariableToken(str))

  def labelSymbol: Parser[LabelToken] =
    "[\\w]+(?:.[\\w]+)*".r ^^ (str => LabelToken(str))

  def destination: Parser[DestinationToken] =
    "[0ADM]{1,3}".r ^^ (str => DestinationToken(str))

  def computation: Parser[ComputationToken] =
    "[ADM]*[-+!&|]*[ADM]*0?1?".r ^^ (str => ComputationToken(str))

  def jump: Parser[JumpToken] = "[EGJLMNPQT]{3}".r ^^ (str => JumpToken(str))
}
