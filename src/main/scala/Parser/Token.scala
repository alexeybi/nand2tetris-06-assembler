package Parser

import Binary.Constants._
import Binary.Symbols
import Binary.Symbols.{updateLabels, updateVariables}

sealed trait Token {
  def lookUp(s: Symbols): Option[String] = this match {
    case VariableToken(v)    => getVariable(v, s)
    case NumToken(v)         => makeBinaryFromNumber(v)
    case DestinationToken(v) => getDestination(v)
    case ComputationToken(v) => getComputation(v)
    case JumpToken(v)        => getJump(v)
  }
}

case class LabelToken(v: String) {
  def add(k: String, v: Int, s: Symbols): Symbols =
    updateLabels(k, v, s)
}
case class VariableToken(v: String) extends Token {
  def add(k: String, s: Symbols): Symbols = updateVariables(k, s)
}
case class NumToken(v: String) extends Token
case class DestinationToken(v: String) extends Token
case class ComputationToken(v: String) extends Token
case class JumpToken(v: String) extends Token
