package shipreq.base.util

final case class ErrorMsg(value: String) {

  // Keep this as a val so that the stack trace points to where the error was created, as opposed to thrown.
  val exception: ErrorMsg.Exception =
    ErrorMsg.Exception(this)

  def throwException(): Nothing =
    throw exception

  def modMsg(f: String => String): ErrorMsg = {
    val e = ErrorMsg(f(value))
    e.exception.setStackTrace(exception.getStackTrace)
    e
  }

  def withPrefix(s: String): ErrorMsg =
    modMsg(s + _)
}

object ErrorMsg {

  implicit def univEq: UnivEq[ErrorMsg] =
    UnivEq.derive

  def fromThrowable(t: Throwable): ErrorMsg =
    apply(Option(t.getMessage).getOrElse(t.toString).trim)

  final case class Exception(msg: ErrorMsg) extends RuntimeException(msg.value)
}
