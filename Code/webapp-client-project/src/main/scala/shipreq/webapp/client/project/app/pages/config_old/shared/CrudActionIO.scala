package shipreq.webapp.client.project.app.pages.config_old.shared

import japgolly.scalajs.react.Callback
import scalaz.{-\/, \/-}
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.data.{DataIdAux, TCB}
import shipreq.webapp.base.data.TCB.{Failure, Success}
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.util.CallbackHelpers._

/**
 * @tparam D Data type.
 * @tparam I Data ID.
 * @tparam U Updated data values.
 */
trait CrudActionIO[D, I, U] { self =>
  def createIO(values: U, s: TCB.Success, f: TCB.Failure): Callback
  def updateIO(data: D, u: U, s: TCB.Success, f: TCB.Failure): Callback
  def deleteIO(id: I, a: DeletionAction, s: TCB.Success, f: TCB.Failure): Callback

  final def _deleteIO: (I, DeletionAction) => (TCB.Success, TCB.Failure) => Callback =
    (id, a) => (s, f) => deleteIO(id, a, s, f)

  final def contramapValues[A](g: A => U): CrudActionIO[D, I, A] =
    new CrudActionIO[D, I, A] {
      override def createIO(a: A, s: Success, f: Failure) = self.createIO(g(a), s, f)
      override def updateIO(d: D, a: A, s: Success, f: Failure) = self.updateIO(d, g(a), s, f)
      override def deleteIO(i: I, da: DeletionAction, s: Success, f: Failure) = self.deleteIO(i, da, s, f)
    }
}

object CrudActionIO {
  def apply[D, I, U, Cmd](proc: ServerSideProcInvoker[Cmd, ErrorMsg, VerifiedEvent.Seq])
                         (create: U => Cmd,
                          update: (I, U) => Cmd,
                          delete: I => Cmd,
                          restore: I => Cmd)
                         (implicit I: DataIdAux[D, I]): CrudActionIO[D, I, U] =
    new CrudActionIO[D, I, U] {

      private def crudIO(s: TCB.Success, f: TCB.Failure, cmd: Cmd): Callback =
        proc(cmd).flatTapSync {
          case \/-(_) => s
          case -\/(_) => f
        }.toCallback

      override def createIO(values: U, s: Success, f: Failure): Callback =
        crudIO(s, f, create(values))

      override def updateIO(data: D, u: U, s: Success, f: Failure): Callback =
        crudIO(s, f, update(data.id, u))

      override def deleteIO(id: I, a: DeletionAction, s: Success, f: Failure): Callback =
        crudIO(s, f, a match {
          case Delete  => delete(id)
          case Restore => restore(id)
        })
    }
}