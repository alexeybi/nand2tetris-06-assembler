package IOUtils

import cats.effect.{IO, Resource}
import Errors.{AppError, IOErr, ParseError, ReadFileError, WriteFileError}

import java.io._
import scala.io.Source

object IOUtils {
  private def fileIO[A <: AutoCloseable, T](
      source: A,
      f: A => IO[Either[IOErr, T]],
      error: Throwable => IO[Either[IOErr, T]]
  ): IO[Either[IOErr, T]] =
    Resource
      .fromAutoCloseable(IO(source))
      .use(f)
      .handleErrorWith(error)

  def readFile(file: String): IO[Either[IOErr, String]] =
    fileIO(
      Source.fromFile(file),
      (s: Source) => IO(Right(s.mkString)),
      throwable => IO(Left(ReadFileError(throwable.getMessage)))
    )

  def writeFile(
      file: String,
      instructions: String
  ): IO[Either[IOErr, Unit]] =
    fileIO(
      new BufferedWriter(new FileWriter(new File(file))),
      (w: BufferedWriter) => IO(Right(w.write(instructions))),
      throwable => IO(Left(WriteFileError(throwable.getMessage)))
    )

  def writeAll(binaries: Either[AppError, String], file: String): IO[Unit] =
    binaries.fold(
      error => IO(println(error)),
      _ =>
        writeFile(s"${file.split('.')(0)}.hack", binaries.getOrElse("")) *>
          IO(println("Success!"))
    )

  def start(args: Array[String], run: => IO[Unit]): Unit =
    if (args.isEmpty)
      IO(println("File with instructions is not specified")).unsafeRunSync()
    else run.unsafeRunSync()
}
