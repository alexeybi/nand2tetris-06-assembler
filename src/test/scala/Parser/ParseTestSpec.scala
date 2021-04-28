package Parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import Errors.ParseError

class ParseTestSpec extends AnyFlatSpec with Matchers {

  "Parser" should "parse string with basic A-instruction" in {
    Parse("@10 @20 @33") should equal(
      Right(
        List(
          ANum(NumToken("10")),
          ANum(NumToken("20")),
          ANum(NumToken("33"))
        )
      )
    )
  }

  it should "parse A-instruction with variable symbol" in {
    Parse("@a @b @c @d") should equal(
      Right(
        List(
          AVariable(VariableToken("a")),
          AVariable(VariableToken("b")),
          AVariable(VariableToken("c")),
          AVariable(VariableToken("d"))
        )
      )
    )
  }

  it should "parse CCompute instructions" in {
    Parse("M=1 D=D-M") should equal(
      Right(
        List(
          CCompute(DestinationToken("M"), ComputationToken("1")),
          CCompute(DestinationToken("D"), ComputationToken("D-M"))
        )
      )
    )
  }

  it should "parse CJump instructions" in {
    Parse("0;JMP D;JGT") should equal(
      Right(
        List(
          CJump(ComputationToken("0"), JumpToken("JMP")),
          CJump(ComputationToken("D"), JumpToken("JGT"))
        )
      )
    )
  }

  it should "not parse incorrect CCompute instructions" in {
    Parse("X=2 Z=D-M") should equal(
      Left(ParseError("""'(' expected but 'X' found"""))
    )
  }

  it should "not parse incorrect CJump instructions" in {
    Parse("Y;JGT D;LMP") should equal(
      Left(ParseError("""'(' expected but 'Y' found"""))
    )
  }

  it should "not parse empty instructions string" in {
    Parse("") should equal(
      Left(ParseError("""'(' expected but end of source found"""))
    )
  }

  it should "parse R-instructions" in {
    Parse("@R1 @R2 @R3") should equal {
      Right(
        List(
          ANum(NumToken("1")),
          ANum(NumToken("2")),
          ANum(NumToken("3"))
        )
      )
    }
  }

  it should "parse labels" in {
    Parse("(LOOP) @LOOP (END) @END (GOTO) @GOTO") should be(
      Right(
        List(
          Label(LabelToken("LOOP"), AVariable(VariableToken("LOOP"))),
          Label(LabelToken("END"), AVariable(VariableToken("END"))),
          Label(LabelToken("GOTO"), AVariable(VariableToken("GOTO")))
        )
      )
    )
  }
}
