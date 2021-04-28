package Errors

sealed trait IOErr
final case class ReadFileError(msg: String) extends IOErr
final case class WriteFileError(msg: String) extends IOErr

sealed trait AppError
final case class ParseError(msg: String) extends AppError
final case class ConvertError(msg: String) extends AppError
