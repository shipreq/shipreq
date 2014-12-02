package shipreq.webapp.client.ui

import japgolly.scalajs.react._, vdom.ReactVDom.{Tag => _, _}, all._
import scalaz.effect.IO
import scalaz.std.anyVal.booleanInstance
import scalaz.std.option._
import scalaz.std.string.stringInstance
import scalaz.std.tuple._
import scalaz.syntax.bind._
import shipreq.base.util.TaggedTypes.taggedStringInstance
import shipreq.webapp.base.TextMod
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.data.Validators.{customIncmpType => V}
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.delta.Partition
import shipreq.webapp.base.protocol.Routines.{CustomIncmpTypeCrud, CustomReqTypeImplicationMod}
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.lib._
import shipreq.webapp.client.protocol.{ClientProtocol, FailureIO}
import shipreq.webapp.client.util.OnUnmountBackend
import shipreq.webapp.client.util.ui.table._
import shipreq.webapp.client.util.ui.{Editors => E}

object CfgIncompletions {

  case class Props(a: CustomIncmpTypeCrud.Remote,
                   b: CustomReqTypeImplicationMod.Remote,
                   c: ClientData,
                   showDeleted: Boolean)

  val comp = ReactComponentB[Props]("Cfg: Incompletions")
    .render(p =>
      div(
        h4("User-Defined Incompletion Types"),
        UserDefIncompletions.comp(TableIoProps(p.a, p.c, p.showDeleted)),
        h4("Other Causes of Incompletion"),
        OtherCauses.comp(TableIoArb(p.b, p.c)))
    ).build

  // ===================================================================================================================

  object UserDefIncompletions {

    val tableIO = TableIO(CustomIncmpType, CustomIncmpTypeCrud)
    import tableIO.{D, P}

    private val prespec = TableSpecBuilder[P](
      FieldSpec[P](_.key.value)(V.key)(E.TextInputEditor),
      FieldSpec[P](_.desc)(V.desc)(E.TextareaEditor))
      .dataId[D]

    private val spec = prespec
      .tableConstraints(
        Some(prespec.uniquenessCheck(_.key).fieldName(FieldNames.refKey)),
        None)
      .saveNotNeededWhenE(p => (p.key, p.desc))
      .asyncSaveP(tableIO.updateIO)

    private val specC = TableSpecC(spec)(tableIO.createIO)
    private val specD = TableSpecD(spec)(_.alive, tableIO.deleteIO)
    private val compI = tableIO.innerComponent(spec, Partition.CustomIncmpTypes, renderInner)

    val comp = tableIO.outerComponent("CfgIncmpTypes", compI)

    private def cells = new CfgTableCells[P, spec.VV, spec.VV] {
      override def mklist = { case (key, desc) => List(key, desc) }
      override def newRow = identity
      override def savedRow = (v,p) => v
      override def deletedRow = p => (raw(p.key.value), raw(TextMod.nonBlank from p.desc))
    }

    private val tbl = CfgTable(CustomIncmpType).b1(spec)(specC, specD, ("", ""), _.key).b2(cells)

    private def renderInner(S: ComponentScopeU[tableIO.Props, prespec.S, _]): ReactElement =
      tbl(S.props.showDeleted, S)(S.props.x)
        .tableness(List(FieldNames.refKey, FieldNames.desc), identity)
  }

  // ===================================================================================================================

  object OtherCauses {

    val tableIO = new RemoteDeltaListener[CustomReqType, CustomReqType.Id, CustomReqTypeImplicationMod.type]
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

    val comp = ReactComponentB[Arb]("OtherCauses")
      .getInitialState(p => spec.initialState(p.clientData.project.customReqTypes.data, _.id))
      .backend(_ => new OnUnmountBackend)
      .render(render _)
      .configure(tableIO.recvExtUpdates(spec, Partition.CustomReqTypes, identity))
      .build

    // TODO doesn't handle static reqs

    private def savedRow(implicit x: Arb) = // TODO fuck this implicit shit off
      spec.savedRowP((F, id, rs, p, vv) => {
        val c = UiLib.rowStatusRowClass(rs)
        val ctrls = UiLib.rowStatusCtrls(rs, EmptyTag)
        tr(cls := c, key := id.value, td(label(vv, p.fullName), ctrls))
      })

    private def render(T: ComponentScopeU[Arb, prespec.S, _]): ReactElement = {
      implicit def x = T.props
      val rows = spec.savedRows(T, savedRow)(_.filter(_.p.alive == Alive).sortBy(_.p.mnemonic))
      table(
        cls := "reqimp",
        thead(tr(th("ReqTypes requiring implication"))),
        tbody(rows))
    }
  }
}