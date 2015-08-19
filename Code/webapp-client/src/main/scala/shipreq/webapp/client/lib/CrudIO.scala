package shipreq.webapp.client.lib

import japgolly.scalajs.react.Callback
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.DeletionAction
import shipreq.webapp.base.protocol._
import shipreq.webapp.client.app.state.ClientData
import shipreq.webapp.client.protocol.ClientProtocol
import DataImplicits._

object CrudIO {
  def apply[O, D, I, U](o: O, rd: CrudFn.Aux[I, U])
                       (cp: ClientProtocol, remote: rd.Instance, clientData: ClientData)
                       (implicit O: ObjDataId[O, D, I]) =
    new CrudIO[D, I, U, rd.type](cp, remote, clientData)
}

/**
 * @tparam D Data type.
 * @tparam I Data ID.
 * @tparam U Updated data values.
 */
final class CrudIO[D, I, U, RD <: CrudFn.Aux[I, U]](cp: ClientProtocol,
                                                    remote: RemoteFn.InstanceFor[RD],
                                                    clientData: ClientData)
                                                   (implicit I: DataIdAux[D, I]) {

  private def crudIO(s: TCB.Success, f: TCB.Failure, a: CrudAction[I, U]): Callback = {
    cp.call(remote)(a,
      s << clientData.applyEvents(_),
      cp.consumeGenericFailure(_) >> f)
  }

  def createIO(values: U, s: TCB.Success, f: TCB.Failure): Callback =
    crudIO(s, f, CrudAction.Create(values))

  def updateIO(data: D, u: U, s: TCB.Success, f: TCB.Failure): Callback =
    crudIO(s, f, CrudAction.Update(data.id, u))

  def deleteIO(id: I, a: DeletionAction, s: TCB.Success, f: TCB.Failure): Callback =
    crudIO(s, f, CrudAction.Delete(id, a))

  def _deleteIO: (I, DeletionAction) => (TCB.Success, TCB.Failure) => Callback =
    (id, a) => (s, f) => deleteIO(id, a, s, f)
}