package shipreq.webapp.util

object QuietException extends RuntimeException {
  override def toString = "QuietException"
  override def getMessage = "QuietException"
  override def printStackTrace() = ()
  override def printStackTrace(s: java.io.PrintStream) = ()
  override def printStackTrace(s: java.io.PrintWriter) = ()
  override def getStackTrace = Array.empty
}