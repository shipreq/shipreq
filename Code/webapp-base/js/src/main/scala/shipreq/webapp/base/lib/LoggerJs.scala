package shipreq.webapp.base.lib

import japgolly.scalajs.react.{AsyncCallback, Callback}
import org.scalajs.dom.console
import scala.annotation.elidable
import scala.scalajs.js
import js.{UndefOr, undefined}

/**
 * Logger that only logs in dev-mode.
 * In prod-mode, the logging functionality is elided and replaced with `Callback.empty`.
 */
sealed trait LoggerJs {
  def debug     (msg: js.Any, extra: js.Any*)               : Callback
  def info      (msg: js.Any, extra: js.Any*)               : Callback
  def warn      (msg: js.Any, extra: js.Any*)               : Callback
  def error     (msg: js.Any, extra: js.Any*)               : Callback
  def exception (err: Throwable)                            : Callback
  def log       (msg: js.Any, extra: js.Any*)               : Callback
  def assert    (test: Boolean, msg: String, extra: js.Any*): Callback
  def dir       (value: js.Any, extra: js.Any*)             : Callback
  def time      (label: String)                             : Callback
  def timeLog   (label: String)                             : Callback
  def timeEnd   (label: String)                             : Callback
  def profile   (reportName: UndefOr[String] = undefined)   : Callback
  val profileEnd                                            : Callback
  val clear                                                 : Callback
}

object LoggerJs {

  private final val L = elidable.INFO

  @js.native
  private trait Console2 extends js.Object {
    def debug(message: js.Any, optionalParams: js.Any*): Unit = js.native
    def time(label: String): Unit = js.native
    def timeLog(label: String): Unit = js.native
    def timeEnd(label: String): Unit = js.native
  }

  @elidable(L) private def newInstance: LoggerJs = {
    new LoggerJs {
      val consol2 = console.asInstanceOf[Console2]
      private def wrap(f: => Any): Callback = Callback(f).attempt.void
      override def exception (err: Throwable)                             = wrap(err.printStackTrace(System.err))
      override def debug     (msg: js.Any, extra: js.Any*)                = wrap(consol2.debug     (msg, extra: _*))
      override def info      (msg: js.Any, extra: js.Any*)                = wrap(console.info      (msg, extra: _*))
      override def warn      (msg: js.Any, extra: js.Any*)                = wrap(console.warn      (msg, extra: _*))
      override def error     (msg: js.Any, extra: js.Any*)                = wrap(console.error     (msg, extra: _*))
      override def log       (msg: js.Any, extra: js.Any*)                = wrap(console.log       (msg, extra: _*))
      override def assert    (test: Boolean, msg: String, extra: js.Any*) = wrap(console.assert    (test, msg, extra: _*))
      override def dir       (value: js.Any, extra: js.Any*)              = wrap(console.dir       (value, extra: _*))
      override def time      (label: String)                              = wrap(consol2.time      (label))
      override def timeLog   (label: String)                              = wrap(consol2.timeLog   (label))
      override def timeEnd   (label: String)                              = wrap(consol2.timeEnd   (label))
      override def profile   (reportName: UndefOr[String] = undefined)    = wrap(reportName.fold(console.profile())(console.profile))
      override val profileEnd                                             = wrap(console.profileEnd())
      override val clear                                                  = wrap(console.clear())
    }
  }

  private val instance = newInstance

  @elidable(L) def runNow(f: => (LoggerJs => Callback)): Unit =
    if (instance ne null) f(instance).runNow()

  @inline def apply(f: => (LoggerJs => Callback)): Callback =
    Callback(runNow(f))

  @inline def async(f: => (LoggerJs => Callback)): AsyncCallback[Unit] =
    AsyncCallback.delay(runNow(f))

  sealed class Dsl {
    @elidable(L) def runNow(f: => (LoggerJs => Callback)): Unit =
      if (instance ne null) f(instance).runNow()

    @inline def apply(f: => (LoggerJs => Callback)): Callback =
      Callback(runNow(f))

    @inline def async(f: => (LoggerJs => Callback)): AsyncCallback[Unit] =
      AsyncCallback.delay(runNow(f))
  }

  val on: Dsl = new Dsl

  val off: Dsl = new Dsl {
    @elidable(L) override def runNow(f: => (LoggerJs => Callback)) = ()
    @inline override def apply(f: => (LoggerJs => Callback)) = Callback.empty
    @inline override def async(f: => (LoggerJs => Callback)) = AsyncCallback.pure(())
  }
}
