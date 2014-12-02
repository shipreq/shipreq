package shipreq.webapp.client.util.ui.tablespec2

import japgolly.scalajs.react._, vdom.ReactVDom._, all._, ScalazReact._
import japgolly.scalajs.react.experiment.{Listenable, OnUnmount}
import shipreq.webapp.client.util.ui.Util.checkbox
import shipreq.webapp.client.util.ui.table.SuccessIO
import scalaz.Scalaz.Id
import scalaz.effect.IO
import scalaz.syntax.bind._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.delta._
import shipreq.webapp.base.protocol._, Routine._
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.delta.LocalDelta
import shipreq.webapp.client.protocol.{ClientProtocol, FailureIO}
import shipreq.webapp.client.util.OnUnmountBackend
//import shipreq.webapp.client.util.ui.table._
import DataImplicits._

final case class DataIO[RD <: Desc](remote: Remote[RD], clientData: ClientData)

final class TableIoProps[RD <: Desc](val arb: DataIO[RD], val showDeleted: Boolean)
object TableIoProps {
  @inline def apply[RD <: Desc](arb: DataIO[RD], showDeleted: Boolean) =
    new TableIoProps(arb, showDeleted)

  @inline def apply[RD <: Desc](remote: Remote[RD], clientData: ClientData, showDeleted: Boolean) =
    new TableIoProps(DataIO(remote, clientData), showDeleted)
}

class RemoteDeltaListener[_P, _D, RD <: Desc {type O = RemoteDelta}](implicit I: DataIdAux[_P, _D]) {
  final type P = _P
  final type D = _D
  final type Arb = DataIO[RD]
  final type Store[S] = SavedRowStore[S, D, P, _]

  private def recvExtUpdate[S](store: Store[S], partition: Partition.Aux[P, D]) =
    (d: LocalDelta) => ReactS.mod[S](s1 => {
      val ds = LocalDelta.filter(partition, d)
      val s2 = (s1 /: ds.del)((s, id)   => store.remove(id)(s))
      val s3 = (s2 /: ds.upd)((s, data) => store.set(I id data, data)(s))
      s3
    })

  def recvExtUpdates[CP, CB <: OnUnmount, S](store: Store[S], partition: Partition.Aux[P, D], f: CP => Arb) =
    Listenable.installS[CP, S, CB, Id, LocalDelta](f(_).clientData, recvExtUpdate(store, partition))
}

object TableIO {
  def apply[O, P, D, V](o: O, rd: Crudable.Aux[D, V])(implicit O: ObjDataId[O, P, D]) =
    new TableIO[P, D, V, rd.type]
}

class TableIO[_P, _D, _V, RD <: Crudable.Aux[_D, _V]](implicit I: DataIdAux[_P, _D])
  extends RemoteDeltaListener[_P, _D, RD] {

  final type U = _V

  private def crudIO(arb: Arb, s: SuccessIO, f: FailureIO, a: CrudAction[D, U]): IO[Unit] =
    ClientProtocol.call(arb.remote)(a, arb.clientData.update(_) >> s.io, f)

  def createIO(arb: Arb, u: U, s: SuccessIO, f: FailureIO): IO[Unit] =
    crudIO(arb, s, f, CrudAction.Create(u))

  def updateIO(arb: Arb, p: P, u: U, s: SuccessIO, f: FailureIO): IO[Unit] =
    crudIO(arb, s, f, CrudAction.Update(p.id, u))

  def deleteIO(arb: Arb, id: D, a: DeletionAction, s: SuccessIO, f: FailureIO): IO[Unit] =
    crudIO(arb, s, f, CrudAction.Delete(id, a))

//  final type Props = TableIoProps[RD]
//  def innerComponent[S, Q <: Partition](//spec: TableSpecU[Arb, S, T#Id, C#V, T#Data, _, _],
//                                        store: Store[S],
//                                        partition: Q,
//                                        render: ComponentScopeU[Props, S, _] => ReactElement)
//                                       (implicit DSA: DataSetAccessor[T], ei: Q#Id =:= T#Id, ed: Q#Data =:= T#Data)
//  = ReactComponentB[Props]("TableIO")
//    .getInitialState[S](p => store.initStateS(DSA.getData(p.arb.clientData.project), _.id))
//    .backend(_ => new OnUnmountBackend)
//    .render(render)
//    .configure(recvExtUpdates(store, partition, _.arb))
//    .build


//  def renderOuter[P0, S0](S: ComponentStateFocus[Boolean],
//                          arb: Arb,
//                          innerComponent: ReactComponentC.ReqProps[Props, _, _, _<:TopNode]): ReactElement = {
//    val s = S.state
//    div(
//      label(
//        checkbox(s)(onchange --> S.modState(b => !b)),
//        raw(if (s) "Showing deleted" else "Not showing deleted")),
//      innerComponent(TableIoProps(arb, s)))
//  }
//
//  def outerComponent(name: String, innerComponent: ReactComponentC.ReqProps[Props, _, _, _<:TopNode]) =
//    ReactComponentB[Props](name)
//      .getInitialState(p => p.showDeleted)
//      .render(S => renderOuter(S, S.props.x, innerComponent))
//      .build
}

class RemoteDeltaListener2[D, I, RD <: Desc {type O = RemoteDelta}](implicit I: DataIdAux[D, I]) {
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

object TableIO2 {
//  def apply[O, D, I, V, RD <: Crudable.Aux[I, V]](o: O, remote: Remote[RD], clientData: ClientData)
//                       (implicit O: ObjDataId[O, D, I]) =
//    new TableIO2[D, I, V, RD](remote, clientData)
//
  //  def apply[O, D, I](o: O)(implicit O: ObjDataId[O, D, I]) =
  //    new {
  //      def rem[V](rd: Crudable.Aux[I, V])(remote: rd.Remote, clientData: ClientData) =
  //        new TableIO2[D, I, V, rd.type](remote, clientData)
  //    }

  def apply[O, D, I, V](o: O, rd: Crudable.Aux[I, V])(remote: rd.Remote, clientData: ClientData)
                             (implicit O: ObjDataId[O, D, I]) =
    new TableIO2[D, I, V, rd.type](remote, clientData)
}

class TableIO2[D, I, _V, RD <: Crudable.Aux[I, _V]](remote: Remote[RD], clientData: ClientData)
                                                      (implicit I: DataIdAux[D, I])
  extends RemoteDeltaListener2[D, I, RD] {

  final type U = _V

  private def crudIO(s: SuccessIO, f: FailureIO, a: CrudAction[I, U]): IO[Unit] =
    ClientProtocol.call(remote)(a, clientData.update(_) >> s.io, f)

  def createIO(u: U, s: SuccessIO, f: FailureIO): IO[Unit] =
    crudIO(s, f, CrudAction.Create(u))

  def updateIO(p: D, u: U, s: SuccessIO, f: FailureIO): IO[Unit] =
    crudIO(s, f, CrudAction.Update(p.id, u))

  def deleteIO(id: I, a: DeletionAction, s: SuccessIO, f: FailureIO): IO[Unit] =
    crudIO(s, f, CrudAction.Delete(id, a))
}