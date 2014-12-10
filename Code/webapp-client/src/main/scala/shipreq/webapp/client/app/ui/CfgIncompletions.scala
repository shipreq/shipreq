package shipreq.webapp.client.app.ui

import japgolly.scalajs.react._, vdom.prefix_<*._, ScalazReact._
import japgolly.scalajs.react.experiment.OnUnmount
import scala.language.reflectiveCalls
import scalaz.std.AllInstances._

import shipreq.webapp.base.data._, DataImplicits._
import shipreq.webapp.base.data.delta.Partition
import shipreq.webapp.base.data.Validators.{customIncmpType => V}
import shipreq.webapp.base.protocol.Routines.{CustomIncmpTypeCrud, CustomReqTypeImplicationMod}
import shipreq.webapp.base.TextMod
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.lib.CrudIO
import shipreq.webapp.client.lib.ui._

object CfgIncompletions {

  case class Props(a: CustomIncmpTypeCrud.Remote,
                   b: CustomReqTypeImplicationMod.Remote,
                   c: ClientData,
                   showDeleted: Boolean)

  val comp = ReactComponentB[Props]("Cfg: Incompletions")
    .render(p =>
      <.div(
        <.h4("User-Defined Incompletion Types"),
        UserDefIncompletions.Props(p.a, p.c, p.showDeleted).component)
        //h4("Other Causes of Incompletion"),
        //OtherCauses.comp(TableIoArb(p.b, p.c)))
    ).build

  // ===================================================================================================================

  object UserDefIncompletions {

    case class Props(remote: CustomIncmpTypeCrud.Remote, clientData: ClientData, showDeleted: Boolean) {
      def component = Component(this)
    }

    val fields = FieldSet2[CustomIncmpType](_.key.value, _.desc getOrElse "")(("", ""))
    val storesAndState = TypicalStoresAndState(fields).keyedBy[CustomIncmpType.Id]
    import storesAndState._

    val Component =
      ReactComponentB[Props]("Cfg: User-Defined Incompletions")
        .getInitialState(initialState)
        .backend(new Backend(_))
        .render(_.backend.render)
        .configure(
          RemoteDeltaListener(CustomIncmpType, CustomIncmpTypeCrud)
            .install(savedRowStoreS, Partition.CustomIncmpTypes, _.clientData))
        .build

    private def initialState(p: Props): S =
      State(newRowStore.initState,
        savedRowStore.initStateS(p.clientData.project.customIncmpTypes.data, _.id),
        p.showDeleted)

    final class Backend(c: BackendScope[Props, S]) extends OnUnmount {
      val crudIO = CrudIO(CustomIncmpType, CustomIncmpTypeCrud)(c.props.remote, c.props.clientData)
      val supp = TypicalSupp(storesAndState, crudIO)(c, _.alive)

      val rowE = {
        val keyE  = Editors.textInputEditor.applyValidator(V.keyS)
        val descE = Editors.textareaEditor.applyValidator(V.descS)
        val e = Editor.merge2S(fields, keyE, descE).tupleI.zoomU[S]
        supp.addEditorFeatures(e)(V.all, _._1._2, p => (p.key, p.desc))
      }

      val table = {
        def rowRenderer =
          new CfgTable.RowRenderer[CustomIncmpType, rowE.View, (Modifier, Modifier)] {
            override def newRow     = identity
            override def savedRow   = (v, p) => v
            override def deletedRow = p => (p.key.value, TextMod.nonBlank from p.desc)
            override def render     = { case (key, desc) => List(key, desc) }
          }
        val t = CfgTable.typical(storesAndState)(rowE)(_.key, rowRenderer, supp.deletion, c)
        val headerRow = CfgTable.header(List(FieldNames.refKey, FieldNames.desc))
        () => t.table(headerRow, Stream.empty)
      }

      def render: ReactElement =
        CfgTable.outer(storesAndState)(c, table())
    }
  }

  // ===================================================================================================================

  object OtherCauses {

    case class Props(remote: CustomIncmpTypeCrud.Remote, clientData: ClientData) {
//      def component = Component(this)
    }

    //    val tableIO = new RemoteDeltaListener[CustomReqType, CustomReqType.Id, CustomReqTypeImplicationMod.type]
//    import tableIO.{Arb, D, P}
//
//    private val prespec = TableSpecBuilder[P](
//      FieldSpec[P].noValidation(_.imp, ImplicationRequired)(E.CheckboxEditor))
//      .dataId[D]
//
//    private val spec = prespec
//      .tableConstraints(None)
//      .saveNotNeededWhenE(_.imp)
//      .asyncSaveP(updateIO)
//
//    def updateIO(arb: Arb, p: P, u: prespec.U, s: SuccessIO, f: FailureIO): IO[Unit] =
//      ClientProtocol.call(arb.remote)((p.id, u), arb.clientData.update(_) >> s.io, f)
//
//    val comp = ReactComponentB[Arb]("OtherCauses")
//      .getInitialState(p => spec.initialState(p.clientData.project.customReqTypes.data, _.id))
//      .backend(_ => new OnUnmountBackend)
//      .render(render _)
//      .configure(tableIO.recvExtUpdates(spec, Partition.CustomReqTypes, identity))
//      .build
//
//    // TODO doesn't handle static reqs
//
//    private def savedRow(implicit x: Arb) = // TODO fuck this implicit shit off
//      spec.savedRowP((F, id, rs, p, vv) => {
//        val c = UiLib.rowStatusRowClass(rs)
//        val ctrls = UiLib.rowStatusCtrls(rs, EmptyTag)
//        tr(cls := c, key := id.value, td(label(vv, p.fullName), ctrls))
//      })
//
//    private def render(T: ComponentScopeU[Arb, prespec.S, _]): ReactElement = {
//      implicit def x = T.props
//      val rows = spec.savedRows(T, savedRow)(_.filter(_.p.alive == Alive).sortBy(_.p.mnemonic))
//      table(
//        cls := "reqimp",
//        thead(tr(th("ReqTypes requiring implication"))),
//        tbody(rows))
//    }
  }
}