package shipreq.webapp.client.lib

import scalaz.effect.IO
import scalaz.syntax.bind._
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol._, Routine._
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.protocol.ClientProtocol
import DataImplicits._

object CrudIO {
  def apply[O, D, I, U](o: O, rd: Crudable.Aux[I, U])
                       (cp: ClientProtocol, remote: rd.Remote, clientData: ClientData)
                       (implicit O: ObjDataId[O, D, I]) =
    new CrudIO[D, I, U, rd.type](cp, remote, clientData)
}

/**
 * @tparam D Data type.
 * @tparam I Data ID.
 * @tparam U Updated data values.
 */
final class CrudIO[D, I, U, RD <: Crudable.Aux[I, U]](cp: ClientProtocol,
                                                      remote: Remote[RD],
                                                      clientData: ClientData)
                                                     (implicit I: DataIdAux[D, I]) {

  private def crudIO(s: SuccessIO, f: FailureIO, a: CrudAction[I, U]): IO[Unit] =
    cp.call(remote)(a, clientData.update(_) >> s.io, f)

  def createIO(values: U, s: SuccessIO, f: FailureIO): IO[Unit] =
    crudIO(s, f, CrudAction.Create(values))

  def updateIO(data: D, u: U, s: SuccessIO, f: FailureIO): IO[Unit] =
    crudIO(s, f, CrudAction.Update(data.id, u))

  def deleteIO(id: I, a: DeletionAction, s: SuccessIO, f: FailureIO): IO[Unit] =
    crudIO(s, f, CrudAction.Delete(id, a))

  def _deleteIO: (I, DeletionAction) => (SuccessIO, FailureIO) => IO[Unit] =
    (id, a) => (s, f) => deleteIO(id, a, s, f)
}