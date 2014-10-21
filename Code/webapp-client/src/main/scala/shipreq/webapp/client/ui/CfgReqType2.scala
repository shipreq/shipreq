package shipreq.webapp.client.ui

import japgolly.scalajs.react.ReactComponentB
import scalaz.effect.IO
import scalaz.std.anyVal.booleanInstance
import scalaz.syntax.bind._
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.delta.Partition
import shipreq.webapp.base.protocol.Routines.CustomReqTypeImpUpd
import shipreq.webapp.client.lib._
import shipreq.webapp.client.protocol.{ClientProtocol, FailureIO}
import shipreq.webapp.client.util.OnUnmountBackend
import shipreq.webapp.client.util.ui.table._
import shipreq.webapp.client.util.ui.{Editors => E}

object CfgReqType2 {

  val tableIO = new RemoteDeltaListener[CustomReqTypeAndId, CustomReqTypeImpUpd.type]
  import tableIO.{Arb, D, P}

  private val prespec = TableSpecBuilder[P](
    FieldSpec[P].noValidation(_.imp, ImplicationRequired)(E.CheckboxEditor))
    .dataId[D]

  private val spec = prespec
    .tableConstraints(None)
    .saveNotNeededWhenE(_.imp)
    .asyncSaveP(updateIO)

  def updateIO(arb: Arb, p: P, u: prespec.U, s: SuccessIO, f: FailureIO): IO[Unit] =
    ClientProtocol.call(arb.remote)((p.id, u), arb.clientData.update(_) >> s.io, f)

  val Component = ReactComponentB[Arb]("asdf")
    .getInitialState(p => spec.initialState(p.clientData.project.customReqTypes.data, _.id))
    .backend(_ => new OnUnmountBackend)
    .render(Render.render _)
    .configure(tableIO.recvExtUpdates(spec, Partition.CustomReqTypes, identity))
    .build

  // TODO doesn't handle static reqs

  // ===================================================================================================================
  private object Render {
    import japgolly.scalajs.react._, vdom.ReactVDom.{Tag => _, _}, all._

    private def savedRow(implicit x: Arb) = // TODO fuck this implicit shit off
      spec.savedRowP((F, id, rs, p, vv) => {
        val c = UiLib.rowStatusRowClass(rs)
        val ctrls = UiLib.rowStatusCtrls(rs, EmptyTag)
        tr(cls := c, key := id.value, td(label(vv, p.fullName), ctrls))
      })

    def render(T: ComponentScopeU[Arb, prespec.S, _]): VDom = {
      implicit def x = T.props
      val rows = spec.savedRows(T, savedRow)(_.filter(_.p.alive == Alive).sortBy(_.p.mnemonic))
      table(
        cls := "reqimp",
        thead(tr(th("ReqTypes requiring implication"))),
        tbody(rows)
      )
    }
  }
}