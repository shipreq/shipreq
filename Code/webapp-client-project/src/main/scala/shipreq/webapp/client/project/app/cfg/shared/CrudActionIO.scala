package shipreq.webapp.client.project.app.cfg.shared

import japgolly.scalajs.react.Callback
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.data.TCB
import DataImplicits._
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.event.VerifiedEvent

/**
 * @tparam D Data type.
 * @tparam I Data ID.
 * @tparam U Updated data values.
 */
final case class CrudActionIO[D, I, U](proc: ServerSideProcInvoker[CrudAction[I, U], ErrorMsg, VerifiedEvent.Seq])
                                      (implicit I: DataIdAux[D, I]) {

  private def crudIO(s: TCB.Success, f: TCB.Failure, a: CrudAction[I, U]): Callback =
    proc(a, _ => s, _ => f)

  def createIO(values: U, s: TCB.Success, f: TCB.Failure): Callback =
    crudIO(s, f, CrudAction.Create(values))

  def updateIO(data: D, u: U, s: TCB.Success, f: TCB.Failure): Callback =
    crudIO(s, f, CrudAction.Update(data.id, u))

  def deleteIO(id: I, a: DeletionAction, s: TCB.Success, f: TCB.Failure): Callback =
    crudIO(s, f, a.crudAction(id))

  def _deleteIO: (I, DeletionAction) => (TCB.Success, TCB.Failure) => Callback =
    (id, a) => (s, f) => deleteIO(id, a, s, f)
}
