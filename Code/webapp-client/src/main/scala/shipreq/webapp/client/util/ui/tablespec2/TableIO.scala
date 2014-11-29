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

class RemoteDeltaListener[T <: DataAndId, RD <: DescT[_, RemoteDelta]](implicit I: IdAccessor[T]) {
  final type P = T#Data
  final type D = T#Id
  final type Arb = DataIO[RD]
  final type Store[S] = SavedRowStore[S, D, P, _]

  private def recvExtUpdate[S, Q <: Partition](store: Store[S], partition: Q)
                                              (implicit ei: Q#Id =:= T#Id, ed: Q#Data =:= T#Data) =
    (d: LocalDelta) => ReactS.mod[S](s1 => {
      val ds = LocalDelta.filter(partition, d)
      val s2 = (s1 /: ds.del)((s, id)   => store.remove(id)(s))
      val s3 = (s2 /: ds.upd)((s, data) => store.set(I id data, data)(s))
      s3
    })

  def recvExtUpdates[CP, CB <: OnUnmount, S, Q <: Partition](store: Store[S], partition: Q, f: CP => Arb)
                                                            (implicit ei: Q#Id =:= T#Id, ed: Q#Data =:= T#Data) =
    Listenable.installS[CP, S, CB, Id, LocalDelta](f(_).clientData, recvExtUpdate(store, partition))
}

class TableIO[T <: DataAndId, C <: Crudable, RD <: CrudableCompanion[C]](implicit t_c_id: T#Id =:= C#Id, I: IdAccessor[T])
    extends RemoteDeltaListener[T, RD] {

  final type U = C#V

  private def crudIO(arb: Arb, s: SuccessIO, f: FailureIO, a: CrudAction[C]): IO[Unit] =
    ClientProtocol.call(arb.remote)(a, arb.clientData.update(_) >> s.io, f)

  def createIO(arb: Arb, u: U, s: SuccessIO, f: FailureIO): IO[Unit] =
    crudIO(arb, s, f, CrudAction.Create[C](u))

  def updateIO(arb: Arb, p: P, u: U, s: SuccessIO, f: FailureIO): IO[Unit] =
    crudIO(arb, s, f, CrudAction.Update[C](p.id, u))

  def deleteIO(arb: Arb, id: D, a: DeletionAction, s: SuccessIO, f: FailureIO): IO[Unit] =
    crudIO(arb, s, f, CrudAction.Delete[C](id, a))

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

class RemoteDeltaListener2[T <: DataAndId, RD <: DescT[_, RemoteDelta]](implicit I: IdAccessor[T]) {
  final type Store[S] = SavedRowStore[S, T#Id, T#Data, _]

  private def recvExtUpdate[S, Q <: Partition](store: Store[S], partition: Q)
                                              (implicit ei: Q#Id =:= T#Id, ed: Q#Data =:= T#Data) =
    (d: LocalDelta) => ReactS.mod[S](s1 => {
      val ds = LocalDelta.filter(partition, d)
      val s2 = (s1 /: ds.del)((s, id)   => store.remove(id)(s))
      val s3 = (s2 /: ds.upd)((s, data) => store.set(I id data, data)(s))
      s3
    })

  def recvExtUpdates[CP, CB <: OnUnmount, S, Q <: Partition](store: Store[S], partition: Q, f: CP => ClientData)
                                                            (implicit ei: Q#Id =:= T#Id, ed: Q#Data =:= T#Data) =
    Listenable.installS[CP, S, CB, Id, LocalDelta](f, recvExtUpdate(store, partition))
}

class TableIO2[T <: DataAndId, C <: Crudable, RD <: CrudableCompanion[C]]
  (remote: Remote[RD], clientData: ClientData)
  (implicit t_c_id: T#Id =:= C#Id, I: IdAccessor[T]) {

  final type U = C#V

  private def crudIO(s: SuccessIO, f: FailureIO, a: CrudAction[C]): IO[Unit] =
    ClientProtocol.call(remote)(a, clientData.update(_) >> s.io, f)

  def createIO(u: U, s: SuccessIO, f: FailureIO): IO[Unit] =
    crudIO(s, f, CrudAction.Create[C](u))

  def updateIO(p: T#Data, u: U, s: SuccessIO, f: FailureIO): IO[Unit] =
    crudIO(s, f, CrudAction.Update[C](p.id, u))

  def deleteIO(id: T#Id, a: DeletionAction, s: SuccessIO, f: FailureIO): IO[Unit] =
    crudIO(s, f, CrudAction.Delete[C](id, a))
}