package shipreq.webapp.client.lib

import org.scalajs.dom.console
import scala.annotation.elidable
import scala.scalajs.js
import scalaz.effect.IO

trait ConsoleIO {
  def info      (msg: js.Any, extra: js.Any*)               : IO[Unit]
  def warn      (msg: js.Any, extra: js.Any*)               : IO[Unit]
  def error     (msg: js.Any, extra: js.Any*)               : IO[Unit]
  def log       (msg: js.Any, extra: js.Any*)               : IO[Unit]
  def assert    (test: Boolean, msg: String, extra: js.Any*): IO[Unit]
  def clear     ()                                          : IO[Unit]
  def dir       (value: js.Any, extra: js.Any*)             : IO[Unit]
  def profile   (reportName: String = ???)                  : IO[Unit] // TODO Change ??? on Scala.js 0.6
  def profileEnd()                                          : IO[Unit]
}

object ConsoleIO {

  private final val L = elidable.INFO

  @elidable(L) private def newInstance: ConsoleIO = {
    new ConsoleIO {
      override def info      (msg: js.Any, extra: js.Any*)                = IO[Unit](console.info      (msg, extra: _*))
      override def warn      (msg: js.Any, extra: js.Any*)                = IO[Unit](console.warn      (msg, extra: _*))
      override def error     (msg: js.Any, extra: js.Any*)                = IO[Unit](console.error     (msg, extra: _*))
      override def log       (msg: js.Any, extra: js.Any*)                = IO[Unit](console.log       (msg, extra: _*))
      override def assert    (test: Boolean, msg: String, extra: js.Any*) = IO[Unit](console.assert    (test, msg, extra: _*))
      override def clear     ()                                           = IO[Unit](console.clear     ())
      override def dir       (value: js.Any, extra: js.Any*)              = IO[Unit](console.dir       (value, extra: _*))
      override def profile   (reportName: String = ???)                   = IO[Unit](console.profile   (reportName))
      override def profileEnd()                                           = IO[Unit](console.profileEnd())
    }
  }

  private val instance = newInstance

  private val nopIO = IO(())

  @inline final def apply(f: ConsoleIO => IO[Unit]): IO[Unit] =
    if (instance == null) nopIO else f(instance)
}
