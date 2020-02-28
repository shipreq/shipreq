package shipreq.webapp.client.project.app.pages.config_old.shared

import japgolly.scalajs.react.{Callback, CallbackTo}
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.ScalazReact._

object SimpleEditor {
  val  ST    = ReactS.FixCB[Unit]
  type ST    = ST.T[Unit]
  val  nopST = ST.nop

  def const[I](v: VdomElement): SimpleEditor[I] =
    Editor(_ => v)

  @inline final def callbackH[I](event: CallbackEvent[I], st: ST = nopST): CallbackH[I, CallbackTo, Unit, Unit] =
    CallbackH(event, st, ())

  def onChangeAndEditFinished[I](f: CallbackH[I, CallbackTo, Unit, Unit] => Callback)(i: I): Callback =
    f(callbackH(OnChange(i))) >> f(callbackH(OnEditFinished(i)))
}
