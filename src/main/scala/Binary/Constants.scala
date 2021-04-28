package Binary

object Constants {

  def makeBinaryFromNumber(n: String): Option[String] =
    n.toIntOption.map(
      _.toBinaryString.reverse.padTo(16, "0").reverse.mkString
    )

  def getDestination(s: String): Option[String] = Destination.codes.get(s)

  def getComputation(s: String): Option[String] = Computation.codes
    .get(s)
    .map("111" + _)

  def getJump(s: String): Option[String] = Jump.codes.get(s)

  def getVariable(k: String, s: Symbols): Option[String] =
    s.symbols
      .get(k)
      .flatMap { i =>
        makeBinaryFromNumber(i.toString)
      }
}

object Destination {
  lazy val codes: Map[String, String] = Map(
    "null" -> "000",
    "M"    -> "001",
    "D"    -> "010",
    "MD"   -> "011",
    "A"    -> "100",
    "AM"   -> "101",
    "AD"   -> "110",
    "AMD"  -> "111"
  )
}

object Computation {
  lazy val codes: Map[String, String] = Map(
    "0"   -> "0101010",
    "1"   -> "0111111",
    "-1"  -> "0111010",
    "D"   -> "0001100",
    "M"   -> "1110000",
    "A"   -> "0110000",
    "!D"  -> "0001101",
    "!A"  -> "0110001",
    "!M"  -> "1110001",
    "-D"  -> "0001111",
    "-A"  -> "0110011",
    "-M"  -> "1110011",
    "D+1" -> "0011111",
    "A+1" -> "0110111",
    "M+1" -> "1110111",
    "D-1" -> "0001110",
    "A-1" -> "0110010",
    "M-1" -> "1110010",
    "D+A" -> "0000010",
    "D+M" -> "1000010",
    "D-A" -> "0010011",
    "D-M" -> "1010011",
    "A-D" -> "0000111",
    "M-D" -> "1000111",
    "D&A" -> "0000000",
    "D&M" -> "1000000",
    "D|A" -> "0010101",
    "D|M" -> "1010101"
  )
}

object Jump {
  lazy val codes: Map[String, String] = Map(
    "null" -> "000",
    "JGT"  -> "001",
    "JEQ"  -> "010",
    "JGE"  -> "011",
    "JLT"  -> "100",
    "JNE"  -> "101",
    "JLE"  -> "110",
    "JMP"  -> "111"
  )
}

case class Symbols(
    variables: Map[String, Int] = Map[String, Int](),
    symbols: Map[String, Int] = Map[String, Int](
      "SP"     -> 0,
      "LCL"    -> 1,
      "ARG"    -> 2,
      "THIS"   -> 3,
      "THAT"   -> 4,
      "SCREEN" -> 16384,
      "KBD"    -> 24576
    )
)
object Symbols {

  def mergeSymbols(s1: Symbols, s2: Symbols): Symbols =
    s1.copy(symbols = s1.symbols ++ s2.variables)

  def updateLabels(k: String, v: Int, s: Symbols): Symbols =
    s.copy(symbols = s.symbols.updated(k, v))

  def updateVariables(k: String, s: Symbols): Symbols =
    if (s.symbols.contains(k) || s.variables.contains(k)) s
    else
      s.copy(variables = s.variables.updatedWith(k) { _ =>
        Some(16 + s.variables.size)
      })
}
