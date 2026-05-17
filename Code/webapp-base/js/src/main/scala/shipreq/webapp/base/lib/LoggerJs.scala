package shipreq.webapp.base.lib

import japgolly.scalajs.react.{AsyncCallback, Callback}
import scala.scalajs.{LinkingInfo, js}

trait LoggerJs {
  import LoggerJs.Dsl

  def apply(f: => (Dsl[Unit] => Unit)): Unit

  def pure(f: => (Dsl[Callback] => Callback)): Callback

  def async(f: => (Dsl[AsyncCallback[Unit]] => AsyncCallback[Unit])): AsyncCallback[Unit]

  def prefixedWith(prefix: => String): LoggerJs
}

object LoggerJs {

  trait Dsl[Out] { self =>
    def debug     (msg: Any, extra: Any*)                  : Out
    def info      (msg: Any, extra: Any*)                  : Out
    def warn      (msg: Any, extra: Any*)                  : Out
    def error     (msg: Any, extra: Any*)                  : Out
    def exception (err: Throwable)                         : Out
    def log       (msg: Any, extra: Any*)                  : Out
    def assert    (test: Boolean, msg: String, extra: Any*): Out
    def dir       (value: Any, extra: Any*)                : Out
    def time      (label: String)                          : Out
    def timeLog   (label: String)                          : Out
    def timeEnd   (label: String)                          : Out
    def profile   (reportName: String)                     : Out
    def profileEnd                                         : Out
    def clear                                              : Out

    def map[B](f: (() => Out) => B): Dsl[B] =
      new Dsl[B] {
        override def exception (err: Throwable)                          = f(() => self.exception (err))
        override def debug     (msg: Any, extra: Any*)                   = f(() => self.debug     (msg, extra: _*))
        override def info      (msg: Any, extra: Any*)                   = f(() => self.info      (msg, extra: _*))
        override def warn      (msg: Any, extra: Any*)                   = f(() => self.warn      (msg, extra: _*))
        override def error     (msg: Any, extra: Any*)                   = f(() => self.error     (msg, extra: _*))
        override def log       (msg: Any, extra: Any*)                   = f(() => self.log       (msg, extra: _*))
        override def assert    (test: Boolean, msg: String, extra: Any*) = f(() => self.assert    (test, msg, extra: _*))
        override def dir       (value: Any, extra: Any*)                 = f(() => self.dir       (value, extra: _*))
        override def time      (label: String)                           = f(() => self.time      (label))
        override def timeLog   (label: String)                           = f(() => self.timeLog   (label))
        override def timeEnd   (label: String)                           = f(() => self.timeEnd   (label))
        override def profile   (reportName: String)                      = f(() => self.profile   (reportName))
        override def profileEnd                                          = f(() => self.profileEnd)
        override def clear                                               = f(() => self.clear)
      }

    def prefixedWith(prefix: String): Dsl[Out] =
      new Dsl[Out] {
        override def exception (err: Throwable)                          = self.exception (err)
        override def debug     (msg: Any, extra: Any*)                   = self.debug     (prefix + msg, extra: _*)
        override def info      (msg: Any, extra: Any*)                   = self.info      (prefix + msg, extra: _*)
        override def warn      (msg: Any, extra: Any*)                   = self.warn      (prefix + msg, extra: _*)
        override def error     (msg: Any, extra: Any*)                   = self.error     (prefix + msg, extra: _*)
        override def log       (msg: Any, extra: Any*)                   = self.log       (prefix + msg, extra: _*)
        override def assert    (test: Boolean, msg: String, extra: Any*) = self.assert    (test, prefix + msg, extra: _*)
        override def dir       (value: Any, extra: Any*)                 = self.dir       (value, extra: _*)
        override def time      (label: String)                           = self.time      (prefix + label)
        override def timeLog   (label: String)                           = self.timeLog   (prefix + label)
        override def timeEnd   (label: String)                           = self.timeEnd   (prefix + label)
        override def profile   (reportName: String)                      = self.profile   (prefix + reportName)
        override def profileEnd                                          = self.profileEnd
        override def clear                                               = self.clear
      }
  }

  private object realDsl extends Dsl[Unit] {
    override def exception (err: Throwable)                          = LoggerJs.exception(err)
    override def debug     (msg: Any, extra: Any*)                   = console.debug     (msg, extra: _*)
    override def info      (msg: Any, extra: Any*)                   = console.info      (msg, extra: _*)
    override def warn      (msg: Any, extra: Any*)                   = console.warn      (msg, extra: _*)
    override def error     (msg: Any, extra: Any*)                   = console.error     (msg, extra: _*)
    override def log       (msg: Any, extra: Any*)                   = console.log       (msg, extra: _*)
    override def assert    (test: Boolean, msg: String, extra: Any*) = console.assert    (test, msg, extra: _*)
    override def dir       (value: Any, extra: Any*)                 = console.dir       (value, extra: _*)
    override def time      (label: String)                           = console.time      (label)
    override def timeLog   (label: String)                           = console.asInstanceOf[js.Dynamic].timeLog(label)
    override def timeEnd   (label: String)                           = console.timeEnd   (label)
    override def profile   (reportName: String)                      = console.profile   (reportName)
    override def profileEnd                                          = console.profileEnd()
    override def clear                                               = console.clear()
  }

  private final class On(dsl: Dsl[Unit]) extends LoggerJs {
    override def apply(f: => (Dsl[Unit] => Unit)): Unit =
      f(dsl)

    private val pureDsl =
      dsl.map(f => Callback(f()))

    override def pure(f: => (Dsl[Callback] => Callback)): Callback =
      f(pureDsl)

    private val asyncDsl =
      pureDsl.map(_().asAsyncCallback)

    override def async(f: => (Dsl[AsyncCallback[Unit]] => AsyncCallback[Unit])): AsyncCallback[Unit] =
      f(asyncDsl)

    override def prefixedWith(prefix: => String) =
      new On(dsl.prefixedWith(prefix))
  }

  lazy val on: LoggerJs =
    new On(realDsl)

  object off extends LoggerJs {
    @elidable(elidable.INFO)
    override def apply(f: => (Dsl[Unit] => Unit)): Unit =
      ()

    override def pure(f: => (Dsl[Callback] => Callback)): Callback =
      Callback.empty

    override def async(f: => (Dsl[AsyncCallback[Unit]] => AsyncCallback[Unit])): AsyncCallback[Unit] =
      AsyncCallback.unit

    override def prefixedWith(prefix: => String) =
      this
  }

  @inline def devOnly: LoggerJs =
    if (LinkingInfo.developmentMode)
      on
    else
      off

  def exception(err: Throwable): Unit =
    err.printStackTrace(System.err)
}
