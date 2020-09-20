package shipreq.webapp.base.lib

import japgolly.scalajs.react.{AsyncCallback, Callback}
import scala.scalajs.js
import scala.scalajs.LinkingInfo

trait LoggerJs {
  import LoggerJs.Dsl

  def apply(f: => (Dsl[Unit] => Unit)): Unit

  def pure(f: => (Dsl[Callback] => Callback)): Callback

  def async(f: => (Dsl[AsyncCallback[Unit]] => AsyncCallback[Unit])): AsyncCallback[Unit]
}

object LoggerJs {

  trait Dsl[Out] { self =>
    def debug     (msg: js.Any, extra: js.Any*)               : Out
    def info      (msg: js.Any, extra: js.Any*)               : Out
    def warn      (msg: js.Any, extra: js.Any*)               : Out
    def error     (msg: js.Any, extra: js.Any*)               : Out
    def exception (err: Throwable)                            : Out
    def log       (msg: js.Any, extra: js.Any*)               : Out
    def assert    (test: Boolean, msg: String, extra: js.Any*): Out
    def dir       (value: js.Any, extra: js.Any*)             : Out
    def time      (label: String)                             : Out
    def timeLog   (label: String)                             : Out
    def timeEnd   (label: String)                             : Out
    def profile   (reportName: String)                        : Out
    def profileEnd                                            : Out
    def clear                                                 : Out

    def map[B](f: (() => Out) => B): Dsl[B] =
      new Dsl[B] {
        override def exception (err: Throwable)                             = f(() => self.exception (err))
        override def debug     (msg: js.Any, extra: js.Any*)                = f(() => self.debug     (msg, extra: _*))
        override def info      (msg: js.Any, extra: js.Any*)                = f(() => self.info      (msg, extra: _*))
        override def warn      (msg: js.Any, extra: js.Any*)                = f(() => self.warn      (msg, extra: _*))
        override def error     (msg: js.Any, extra: js.Any*)                = f(() => self.error     (msg, extra: _*))
        override def log       (msg: js.Any, extra: js.Any*)                = f(() => self.log       (msg, extra: _*))
        override def assert    (test: Boolean, msg: String, extra: js.Any*) = f(() => self.assert    (test, msg, extra: _*))
        override def dir       (value: js.Any, extra: js.Any*)              = f(() => self.dir       (value, extra: _*))
        override def time      (label: String)                              = f(() => self.time      (label))
        override def timeLog   (label: String)                              = f(() => self.timeLog   (label))
        override def timeEnd   (label: String)                              = f(() => self.timeEnd   (label))
        override def profile   (reportName: String)                         = f(() => self.profile   (reportName))
        override def profileEnd                                             = f(() => self.profileEnd)
        override def clear                                                  = f(() => self.clear)
      }
  }

  private object real extends Dsl[Unit] {
    override def exception (err: Throwable)                             = LoggerJs.exception(err)
    override def debug     (msg: js.Any, extra: js.Any*)                = console.debug     (msg, extra: _*)
    override def info      (msg: js.Any, extra: js.Any*)                = console.info      (msg, extra: _*)
    override def warn      (msg: js.Any, extra: js.Any*)                = console.warn      (msg, extra: _*)
    override def error     (msg: js.Any, extra: js.Any*)                = console.error     (msg, extra: _*)
    override def log       (msg: js.Any, extra: js.Any*)                = console.log       (msg, extra: _*)
    override def assert    (test: Boolean, msg: String, extra: js.Any*) = console.assert    (test, msg, extra: _*)
    override def dir       (value: js.Any, extra: js.Any*)              = console.dir       (value, extra: _*)
    override def time      (label: String)                              = console.time      (label)
    override def timeLog   (label: String)                              = console.asInstanceOf[js.Dynamic].timeLog(label)
    override def timeEnd   (label: String)                              = console.timeEnd   (label)
    override def profile   (reportName: String)                         = console.profile   (reportName)
    override def profileEnd                                             = console.profileEnd()
    override def clear                                                  = console.clear()
  }

  object on extends LoggerJs {
    override def apply(f: => (Dsl[Unit] => Unit)): Unit =
      f(real)

    private val pureDsl =
      real.map(f => Callback(f()))

    override def pure(f: => (Dsl[Callback] => Callback)): Callback =
      f(pureDsl)

    private val asyncDsl =
      pureDsl.map(_().asAsyncCallback)

    override def async(f: => (Dsl[AsyncCallback[Unit]] => AsyncCallback[Unit])): AsyncCallback[Unit] =
      f(asyncDsl)
  }

  object off extends LoggerJs {
    @elidable(elidable.INFO)
    override def apply(f: => (Dsl[Unit] => Unit)): Unit =
      ()

    override def pure(f: => (Dsl[Callback] => Callback)): Callback =
      Callback.empty

    override def async(f: => (Dsl[AsyncCallback[Unit]] => AsyncCallback[Unit])): AsyncCallback[Unit] =
      AsyncCallback.unit
  }

  @inline def devOnly: LoggerJs =
    if (LinkingInfo.developmentMode)
      on
    else
      off

  def exception(err: Throwable): Unit =
    err.printStackTrace(System.err)
}
