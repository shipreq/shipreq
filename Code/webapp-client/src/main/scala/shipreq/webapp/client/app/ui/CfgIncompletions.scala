package shipreq.webapp.client.app.ui

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.experiment.OnUnmount
import scala.language.reflectiveCalls
import scalaz.effect.IO
import scalaz.std.AllInstances._

import shipreq.webapp.base.data._, DataImplicits._
import shipreq.webapp.base.data.delta.Partition
import shipreq.webapp.base.data.Validators.{customIncmpType => V}
import shipreq.webapp.base.protocol.Routines._
import shipreq.webapp.base.TextMod
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.lib.CrudIO
import shipreq.webapp.client.lib.ui._
import shipreq.webapp.client.protocol.ClientProtocol
import ReqType.Mnemonic

object CfgIncompletions {

  case class Props(a: CustomIncmpTypeCrud.Remote,
                   b: CustomReqTypeImplicationMod.Remote,
                   c: ClientData,
                   showDeleted: Boolean)

  val comp = ReactComponentB[Props]("Cfg: Incompletions")
    .render(p =>
      <.div(
        <.h4("User-Defined Incompletion Types"),
        UserDefIncompletions.Props(p.a, p.c, p.showDeleted).component,
        <.h4("Other Causes of Incompletion"),
        OtherCauses.Props(p.b, p.c).component)
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

    case class Props(remote: CustomReqTypeImplicationMod.Remote, clientData: ClientData) {
      def component = Component(this)
    }

    val savedRowStore = SavedRowStore.data[CustomReqType](_.imp)
    import savedRowStore.{State => S}
    val ST = ReactS.FixT[IO, S]
    type ST = ST.T[Unit]

    val Component = ReactComponentB[Props]("OtherCauses")
      .getInitialState(initialState)
      .backend(new Backend(_))
      .render(_.backend.render)
      .configure(
        RemoteDeltaListener(CustomReqType, CustomReqTypeCrud)
          .install(savedRowStore, Partition.CustomReqTypes, _.clientData))
      .build

    private def initialState(p: Props): S =
      savedRowStore.initStateS(p.clientData.project.customReqTypes.data, _.id)

    def label(r: ReqType): String = s"${r.mnemonic.value}: ${r.name}"

    final class Backend(c: BackendScope[Props, S]) extends OnUnmount {

      def save(p: Props, id: CustomReqType.Id): ST =
        ReactS.liftR[IO, S, Unit](state => {
          val setStatus = savedRowStore.setStatusST[IO](id)
          val saveio = Persistence.retryably[ST](retry => {
            val v = savedRowStore.getI(id)(state)
            val f = Persistence.failureIO(retry)(c runState _, setStatus)
            val io = ClientProtocol.call(p.remote)((id, v), p.clientData.update, f)
            ST ret io
          })
          saveio >> setStatus(RowStatus.Locked)
        })

      val genEditor =
        Editors.checkboxEditor.imap(ImplicationRequired)
          .strengthR[ReqType].labelSuffix(a => label(a._2))

      val editor =
        genEditor.cmapA[(ImplicationRequired, CustomReqType)](a => a)
          .zoomU[S].applyRowUpdate(savedRowStore)(_._2.id)
          .paddSTA(a => { case OnEditFinished(_) => save(c.props, a._2.id) })

      val editable = editor.editableByRowStatus(c)

      def editorI(r: savedRowStore.Row): editor.Input =
        EditorI((r.i, r.p), "", editable(r.status))

      type Rows = Stream[(Mnemonic, ReactElement)]

      def savedRows: Rows =
        savedRowStore.getAll(c.state).filter(_.p.alive == Alive).map(r => {
          val re: ReactElement =
            <.tr(^.key := r.p.id.value,
              <.td(
                editor render editorI(r),
                UI.rowStatusCtrls(r.status, EmptyTag)))
          (r.p.mnemonic, re)
        })

      val staticRows: Rows =
        ReqType.static.toStream.map(s => {
          val re: ReactElement =
            <.tr(^.key := s.mnemonic.value,
              <.td(genEditor render EditorI((s.imp, s), "", None)))
          (s.mnemonic, re)
        })

      def renderRows =
        (staticRows #::: savedRows).sortBy(_._1).map(_._2).toReactNodeArray

      def render: ReactElement =
        <.table(
          <.thead(<.tr(<.th("ReqTypes requiring implication"))),
          <.tbody(renderRows))
    }
  }
}