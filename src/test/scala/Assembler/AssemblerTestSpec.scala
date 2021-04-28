package Assembler

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Errors.ParseError
import Parser.{Instruction, Parse}

class AssemblerTestSpec extends AnyFlatSpec with Matchers {
  import AssemblerTestSpec._
  "Assembler" should "convert A-instruction with numeric value" in {
    val parsed = Parse("@1")
    val instruction = getInstructionFrom(parsed)
    instruction should contain("0000000000000001")
  }
  it should "convert CCompute instruction" in {
    val parsed = Parse("MD=D+1")
    val instruction = getInstructionFrom(parsed)
    instruction should contain("1110011111011000")
  }

  it should "convert CJump instruction" in {
    val parsed = Parse("D;JLE")
    val instruction = getInstructionFrom(parsed)
    instruction should contain("1110001100000110")
  }

  it should "not convert incorrect CCompute instruction" in {
    val parsed = Parse("ZZ=D+1")
    val instruction = getInstructionFrom(parsed)
    instruction should equal(List(""))
  }

  it should "not convert incorrect CJump instruction" in {
    val parsed = Parse("X;JGT")
    val instruction = getInstructionFrom(parsed)
    instruction should equal(List(""))
  }

  it should "convert constant symbols" in {
    val parsed = Parse("@SCREEN @THAT @KBD")
    val instructions = getInstructionFrom(parsed)
    instructions should be(
      List(
        "0100000000000000",
        "0000000000000100",
        "0110000000000000"
      )
    )
  }

  it should "convert Go-to and A-instructions" in {
    val parsed = Parse("@KBD @a @LOOP @b (LOOP) @c")
    val instructions = getInstructionFrom(parsed)
    instructions should be(
      List(
        "0110000000000000",
        "0000000000010000",
        "0000000000000100",
        "0000000000010001",
        "0000000000010010"
      )
    )
  }

  it should "convert R-instructions" in {
    val parsed = Parse("@R1 @R2 @R3")
    val instructions = getInstructionFrom(parsed)
    instructions should be {
      List(
        "0000000000000001",
        "0000000000000010",
        "0000000000000011"
      )
    }

  }

  it should "convert A-instructions with variables" in {
    val parsed = Parse("@a @b @c @d")
    val instructions = getInstructionFrom(parsed)
    instructions should be(
      List(
        "0000000000010000",
        "0000000000010001",
        "0000000000010010",
        "0000000000010011"
      )
    )
  }

}

object AssemblerTestSpec {
  def getInstructionFrom(
      inst: Either[ParseError, List[Instruction]]
  ): List[String] = Instruction.makeAll(inst).getOrElse("").split("\n").toList
}
