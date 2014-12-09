package shipreq.webapp.client.util.ui.tablespec2

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.experiment.{Listenable, OnUnmount}
import scalaz.Scalaz.Id
import scalaz.effect.IO
import scalaz.syntax.bind._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.delta._
import shipreq.webapp.base.protocol._, Routine._
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.delta.LocalDelta
import shipreq.webapp.client.protocol.{ClientProtocol, FailureIO}
import shipreq.webapp.client.util.ui.table.SuccessIO
import DataImplicits._

object RemoteDeltaListener {
  def apply[O, D, I, V](o: O, rd: Crudable.Aux[I, V])(implicit O: ObjDataId[O, D, I]) =
    new RemoteDeltaListener[D, I, rd.type]
}
class RemoteDeltaListener[D, I, RD <: Desc {type O = RemoteDelta}](implicit I: DataIdAux[D, I]) {
  final type Store[S] = SavedRowStore[S, I, D, _]

  private def recvExtUpdate[S](store: Store[S], partition: Partition.Aux[D, I]) =
    (d: LocalDelta) => ReactS.mod[S](s1 => {
      val ds = LocalDelta.filter(partition, d)
      val s2 = (s1 /: ds.del)((s, id)   => store.remove(id)(s))
      val s3 = (s2 /: ds.upd)((s, data) => store.set(I id data, data)(s))
      s3
    })

  def recvExtUpdates[CP, CB <: OnUnmount, S](store: Store[S], partition: Partition.Aux[D, I], f: CP => ClientData) =
    Listenable.installS[CP, S, CB, Id, LocalDelta](f, recvExtUpdate(store, partition))
}

object TableIO {
  def apply[O, D, I, V](o: O, rd: Crudable.Aux[I, V])(remote: rd.Remote, clientData: ClientData)
                             (implicit O: ObjDataId[O, D, I]) =
    new TableIO[D, I, V, rd.type](remote, clientData)
}

class TableIO[D, I, _V, RD <: Crudable.Aux[I, _V]](remote: Remote[RD], clientData: ClientData)
                                                      (implicit I: DataIdAux[D, I]) {
  final type U = _V

  private def crudIO(s: SuccessIO, f: FailureIO, a: CrudAction[I, U]): IO[Unit] =
    ClientProtocol.call(remote)(a, clientData.update(_) >> s.io, f)

  def createIO(u: U, s: SuccessIO, f: FailureIO): IO[Unit] =
    crudIO(s, f, CrudAction.Create(u))

  def updateIO(p: D, u: U, s: SuccessIO, f: FailureIO): IO[Unit] =
    crudIO(s, f, CrudAction.Update(p.id, u))

  def deleteIO(id: I, a: DeletionAction, s: SuccessIO, f: FailureIO): IO[Unit] =
    crudIO(s, f, CrudAction.Delete(id, a))

  def _deleteIO: (I, DeletionAction) => (SuccessIO, FailureIO) => IO[Unit] =
    (id, a) => (s, f) => deleteIO(id, a, s, f)
}