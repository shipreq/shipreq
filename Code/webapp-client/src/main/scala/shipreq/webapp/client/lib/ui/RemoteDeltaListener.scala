package shipreq.webapp.client.lib.ui

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.{Listenable, OnUnmount}
import scalaz.Scalaz.Id
import shipreq.webapp.base.data._
import shipreq.webapp.base.delta._
import shipreq.webapp.base.protocol._
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.delta.LocalDelta
import RemoteDeltaListener.StateFns

object RemoteDeltaListener {
  def apply[O, D, I](o: O)(implicit O: ObjDataId[O, D, I]) = new RemoteDeltaListener[D, I]

  def apply[D](d: DataId[D]) = new RemoteDeltaListener[D, d.I]()(d)

  class StateFns[S, I, D](val remove: (S, I) => S,
                          val put: (S, I, D) => S)
}

/**
 * @tparam D Data type.
 * @tparam I Data ID.
 */
class RemoteDeltaListener[D, I](implicit I: DataIdAux[D, I]) {

  private def recvExtUpdate[S](stateFns: StateFns[S, I, D], partition: Partition.Aux[D, I]) =
    (d: LocalDelta) => ReactS.mod[S](s1 => {
      val ds = LocalDelta.filter(partition, d)
      val s2 = (s1 /: ds.del)((s, id)   => stateFns.remove(s, id))
      val s3 = (s2 /: ds.upd)((s, data) => stateFns.put(s, I id data, data))
      s3
    })

  def installS[CP, CB <: OnUnmount, S](store: SavedRowStore[S, I, D, _],
                                      partition: Partition.Aux[D, I], f: CP => ClientData) = {
    val sf = new StateFns[S, I, D]((s, i) => store.remove(i)(s),
                                   (s, i, d) => store.set(i, d)(s))
    install[CP, CB, S](sf, partition, f)
  }

  def install[CP, CB <: OnUnmount, S](stateFns: StateFns[S, I, D],
                                      partition: Partition.Aux[D, I], f: CP => ClientData) =
    Listenable.installS[CP, S, CB, Id, LocalDelta](f, recvExtUpdate(stateFns, partition))
}