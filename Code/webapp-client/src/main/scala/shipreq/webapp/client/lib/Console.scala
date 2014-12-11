package shipreq.webapp.client.lib

import org.scalajs.dom.console
import scala.annotation.elidable
import scala.scalajs.js
import scalaz.effect.IO

/**
 * Note: All arguments are still eager.
 */
object Console {

  private final val L = elidable.INFO
  
  @elidable(L) @inline final def info      (msg: js.Any, extra: js.Any*)               : Unit = console.info      (msg, extra: _*)
  @elidable(L) @inline final def warn      (msg: js.Any, extra: js.Any*)               : Unit = console.warn      (msg, extra: _*)
  @elidable(L) @inline final def error     (msg: js.Any, extra: js.Any*)               : Unit = console.error     (msg, extra: _*)
  @elidable(L) @inline final def log       (msg: js.Any, extra: js.Any*)               : Unit = console.log       (msg, extra: _*)
  @elidable(L) @inline final def assert    (test: Boolean, msg: String, extra: js.Any*): Unit = console.assert    (test, msg, extra: _*)
  @elidable(L) @inline final def clear     ()                                          : Unit = console.clear     ()
  @elidable(L) @inline final def dir       (value: js.Any, extra: js.Any*)             : Unit = console.dir       (value, extra: _*)
  @elidable(L) @inline final def profile   (reportName: String = ???)                  : Unit = console.profile   (reportName)
  @elidable(L) @inline final def profileEnd()                                          : Unit = console.profileEnd()


  @inline final def infoIO      (msg: js.Any, extra: js.Any*)                = io(info      (msg, extra: _*))
  @inline final def warnIO      (msg: js.Any, extra: js.Any*)                = io(warn      (msg, extra: _*))
  @inline final def errorIO     (msg: js.Any, extra: js.Any*)                = io(error     (msg, extra: _*))
  @inline final def logIO       (msg: js.Any, extra: js.Any*)                = io(log       (msg, extra: _*))
  @inline final def assertIO    (test: Boolean, msg: String, extra: js.Any*) = io(assert    (test, msg, extra: _*))
  @inline final def clearIO     ()                                           = io(clear     ())
  @inline final def dirIO       (value: js.Any, extra: js.Any*)              = io(dir       (value, extra: _*))
  @inline final def profileIO   (reportName: String = ???)                   = io(profile   (reportName))
  @inline final def profileEndIO()                                           = io(profileEnd())

  private val nopIO = IO(())

  @elidable(L) @inline final private def ioN(f: => Unit): IO[Unit] = IO(f)

  @inline final private def io(f: => Unit): IO[Unit] = {
    val io = ioN(f)
    if (io == null) nopIO else io
  }
}
