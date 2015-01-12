package shipreq.webapp.client.lib.ui

import japgolly.scalajs.react.ReactElement
import japgolly.scalajs.react.ScalazReact._
import scalaz.effect.IO
import scalaz.syntax.bind._

object SimpleEditor {
  val  ST    = ReactS.FixT[IO, Unit]
  type ST    = ST.T[Unit]
  val  nopST = ST.nop

  def const[I](v: ReactElement): SimpleEditor[I] =
    Editor(_ => v)

  @inline final def callbackH[I](event: CallbackEvent[I], st: ST = nopST): CallbackH[I, IO, Unit, Unit] =
    CallbackH(event, st, ())

  def onChangeAndEditFinished[I](f: CallbackH[I, IO, Unit, Unit] => IO[Unit])(i: I): IO[Unit] =
    f(callbackH(OnChange(i))) >> f(callbackH(OnEditFinished(i)))
}
