package shipreq.webapp.client.lib

import japgolly.scalajs.react._, vdom.ReactVDom._, all._, ScalazReact._
import japgolly.scalajs.react.experiment.{Listenable, OnUnmount}
import shipreq.webapp.client.util.ui.Util.checkbox
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
import shipreq.webapp.client.util.ui.table._
import DataImplicits._

final case class TableIoArb[RD <: Desc](remote: Remote[RD], clientData: ClientData)

final class TableIoProps[RD <: Desc](val x: TableIoArb[RD], val showDeleted: Boolean)
object TableIoProps {
  @inline def apply[RD <: Desc](x: TableIoArb[RD], showDeleted: Boolean) =
    new TableIoProps(x, showDeleted)

  @inline def apply[RD <: Desc](remote: Remote[RD], clientData: ClientData, showDeleted: Boolean) =
    new TableIoProps(TableIoArb(remote, clientData), showDeleted)
}

class RemoteDeltaListener[_P, _D, RD <: DescT[_, RemoteDelta]](implicit I: DataIdAux[_P, _D]) {
  final type P = _P
  final type D = _D
  final type Arb = TableIoArb[RD]

  private def recvExtUpdate[S](spec: TableSpecU[_, S, D, _, P, _, _], partition: Partition.Aux[P, D]) =
    (d: LocalDelta) => ReactS.mod[S](s1 => {
      val ds = LocalDelta.filter(partition, d)
      val s2 = (s1 /: ds.del)((s, id)   => spec.savedRemoveF(id)(s))
      val s3 = (s2 /: ds.upd)((s, data) => spec.savedSetF(I id data, data)(s))
      s3
    })

  def recvExtUpdates[CP, CB <: OnUnmount, S](spec: TableSpecU[_, S, D, _, P, _, _], partition: Partition.Aux[P, D], f: CP => Arb) =
    Listenable.installS[CP, S, CB, Id, LocalDelta](f(_).clientData, recvExtUpdate(spec, partition))
}

// TODO I want to specify TableIO in two arguments, not four type args.
class TableIO[_P, _D, C <: Crudable {type Id = _D}, RD <: CrudableCompanion[C]](implicit I: DataIdAux[_P, _D])
    extends RemoteDeltaListener[_P, _D, RD] {

  final type U = C#V

  private def crudIO(arb: Arb, s: SuccessIO, f: FailureIO, a: CrudAction[C]): IO[Unit] =
    ClientProtocol.call(arb.remote)(a, arb.clientData.update(_) >> s.io, f)

  def createIO(arb: Arb, u: U, s: SuccessIO, f: FailureIO): IO[Unit] =
    crudIO(arb, s, f, CrudAction.Create[C](u))

  def updateIO(arb: Arb, p: P, u: U, s: SuccessIO, f: FailureIO): IO[Unit] =
    crudIO(arb, s, f, CrudAction.Update[C](p.id, u))

  def deleteIO(arb: Arb, id: D, a: DeletionAction, f: FailureIO): IO[Unit] =
    crudIO(arb, SuccessIO.nop, f, CrudAction.Delete[C](id, a))

  final type Props = TableIoProps[RD]
  def innerComponent[S](spec: TableSpecU[Arb, S, D, C#V, P, _, _],
                        partition: Partition.Aux[P, D],
                        render: ComponentScopeU[Props, S, _] => ReactElement)
                       (implicit DSA: DataSetAccessor[P])
  = ReactComponentB[Props]("TableIO")
    .getInitialState(p => spec.initialState(DSA.getData(p.x.clientData.project), _.id))
    .backend(_ => new OnUnmountBackend)
    .render(render)
    .configure(recvExtUpdates(spec, partition, _.x))
    .build


  def renderOuter[P0, S0](S: ComponentStateFocus[Boolean],
                          arb: Arb,
                          innerComponent: ReactComponentC.ReqProps[Props, _, _, _<:TopNode]): ReactElement = {
    val s = S.state
    div(
      label(
        checkbox(s)(onchange --> S.modState(b => !b)),
        raw(if (s) "Showing deleted" else "Not showing deleted")),
      innerComponent(TableIoProps(arb, s)))
  }

  def outerComponent(name: String, innerComponent: ReactComponentC.ReqProps[Props, _, _, _<:TopNode]) =
    ReactComponentB[Props](name)
      .getInitialState(p => p.showDeleted)
      .render(S => renderOuter(S, S.props.x, innerComponent))
      .build
}
