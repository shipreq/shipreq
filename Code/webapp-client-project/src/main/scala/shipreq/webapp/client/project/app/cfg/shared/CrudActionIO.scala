package shipreq.webapp.client.project.app.cfg.shared

import japgolly.scalajs.react.Callback
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol._
import shipreq.webapp.client.project.app.state.ClientData
import shipreq.webapp.base.data.TCB
import shipreq.webapp.base.protocol.ClientProtocol
import DataImplicits._

object CrudActionIO {
  def apply[O, D, I, U](o: O, rd: CrudProtocol.Aux[I, U])
                       (cp: ClientProtocol, remote: rd.Instance, clientData: ClientData)
                       (implicit O: ObjDataId[O, D, I]) =
    new CrudActionIO[D, I, U, rd.type](cp, remote, clientData)
}

/**
 * @tparam D Data type.
 * @tparam I Data ID.
 * @tparam U Updated data values.
 */
final class CrudActionIO[D, I, U, RD <: CrudProtocol.Aux[I, U]](cp        : ClientProtocol,
                                                                remote    : ServerSideProc.For[RD],
                                                                clientData: ClientData)
                                                               (implicit I: DataIdAux[D, I]) {

  private def crudIO(s: TCB.Success, f: TCB.Failure, a: CrudAction[I, U]): Callback = {
    cp.call(remote)(a,
      clientData.applyEventSeqSCB(_) >> s,
      _.consume >> f)
  }

  def createIO(values: U, s: TCB.Success, f: TCB.Failure): Callback =
    crudIO(s, f, CrudAction.Create(values))

  def updateIO(data: D, u: U, s: TCB.Success, f: TCB.Failure): Callback =
    crudIO(s, f, CrudAction.Update(data.id, u))

  def deleteIO(id: I, a: DeletionAction, s: TCB.Success, f: TCB.Failure): Callback =
    crudIO(s, f, a.crudAction(id))

  def _deleteIO: (I, DeletionAction) => (TCB.Success, TCB.Failure) => Callback =
    (id, a) => (s, f) => deleteIO(id, a, s, f)
}
