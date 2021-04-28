package Assembler

import IOUtils.IOUtils.{readFile, start, writeAll}
import cats.effect.IO
import Parser.Instruction.makeAll
import Parser.Parse

object Assembler {
  def main(args: Array[String]): Unit = {

    def run: IO[Unit] = for {
      file     <- readFile(args(0))
      parsed   <- IO(Parse(file.getOrElse("")))
      binaries <- IO(makeAll(parsed))
      _        <- writeAll(binaries, args(0))
    } yield ()

    start(args, run)
  }
}
