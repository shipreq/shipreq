package shipreq.webapp.client.lib

import japgolly.scalajs.react.Callback
import org.scalajs.dom.console
import scala.annotation.elidable
import scala.scalajs.js
import js.{UndefOr, undefined}

trait ConsoleCB {
  def info      (msg: js.Any, extra: js.Any*)               : Callback
  def warn      (msg: js.Any, extra: js.Any*)               : Callback
  def error     (msg: js.Any, extra: js.Any*)               : Callback
  def log       (msg: js.Any, extra: js.Any*)               : Callback
  def assert    (test: Boolean, msg: String, extra: js.Any*): Callback
  def clear     ()                                          : Callback
  def dir       (value: js.Any, extra: js.Any*)             : Callback
  def profile   (reportName: UndefOr[String] = undefined)   : Callback
  def profileEnd()                                          : Callback
}

object ConsoleCB {

  private final val L = elidable.INFO

  @elidable(L) private def newInstance: ConsoleCB = {
    new ConsoleCB {
      override def info      (msg: js.Any, extra: js.Any*)                = Callback(console.info      (msg, extra: _*))
      override def warn      (msg: js.Any, extra: js.Any*)                = Callback(console.warn      (msg, extra: _*))
      override def error     (msg: js.Any, extra: js.Any*)                = Callback(console.error     (msg, extra: _*))
      override def log       (msg: js.Any, extra: js.Any*)                = Callback(console.log       (msg, extra: _*))
      override def assert    (test: Boolean, msg: String, extra: js.Any*) = Callback(console.assert    (test, msg, extra: _*))
      override def clear     ()                                           = Callback(console.clear     ())
      override def dir       (value: js.Any, extra: js.Any*)              = Callback(console.dir       (value, extra: _*))
      override def profile   (reportName: UndefOr[String] = undefined)    = Callback(reportName.fold(console.profile())(console.profile))
      override def profileEnd()                                           = Callback(console.profileEnd())
    }
  }

  private val instance = newInstance

  @inline final def apply(f: ConsoleCB => Callback): Callback =
    if (instance eq null) Callback.empty else f(instance)

  @elidable(L) final def run(f: ConsoleCB => Callback): Unit =
    apply(f).runNow()
}
