package shipreq.webapp.client.lib.ui

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.{Listenable, OnUnmount}
import scalaz.Scalaz.Id
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.delta._
import shipreq.webapp.base.protocol._, Routine._
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.delta.LocalDelta

object RemoteDeltaListener {
  def apply[O, D, I, V](o: O, rd: Crudable.Aux[I, V])(implicit O: ObjDataId[O, D, I]) =
    new RemoteDeltaListener[D, I, rd.type]
}

/**
 * @tparam D Data type.
 * @tparam I Data ID.
 */
class RemoteDeltaListener[D, I, RD <: Desc {type O = RemoteDelta}](implicit I: DataIdAux[D, I]) {
  final type Store[S] = SavedRowStore[S, I, D, _]

  private def recvExtUpdate[S](store: Store[S], partition: Partition.Aux[D, I]) =
    (d: LocalDelta) => ReactS.mod[S](s1 => {
      val ds = LocalDelta.filter(partition, d)
      val s2 = (s1 /: ds.del)((s, id)   => store.remove(id)(s))
      val s3 = (s2 /: ds.upd)((s, data) => store.set(I id data, data)(s))
      s3
    })

  def install[CP, CB <: OnUnmount, S](store: Store[S], partition: Partition.Aux[D, I], f: CP => ClientData) =
    Listenable.installS[CP, S, CB, Id, LocalDelta](f, recvExtUpdate(store, partition))
}