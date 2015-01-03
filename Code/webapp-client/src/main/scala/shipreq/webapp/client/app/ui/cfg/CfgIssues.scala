package shipreq.webapp.client.app.ui.cfg

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.extra.OnUnmount
import scala.language.reflectiveCalls
import scalaz.effect.IO
import scalaz.std.AllInstances._

import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._, DataImplicits._
import shipreq.webapp.base.delta.Partition
import shipreq.webapp.base.data.Validators.{customIssueType => V}
import shipreq.webapp.base.data.Validators.shared.RefKeyVS
import shipreq.webapp.base.protocol.Routines._
import shipreq.webapp.base.TextMod
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.lib.CrudIO
import shipreq.webapp.client.lib.ui._
import shipreq.webapp.client.protocol.ClientProtocol
import ReqType.Mnemonic

object CfgIssues {

  case class Props(cp: ClientProtocol,
                   a: CustomIssueTypeCrud.Remote,
                   b: CustomReqTypeImplicationMod.Remote,
                   cd: ClientData,
                   showDeleted: Boolean)

  val comp = ReactComponentB[Props]("Cfg: Issues")
    .render(p =>
      <.div(
        <.h4("User-Defined Issue Types"),
        UserDefIssues.Props(p.cp, p.a, p.cd, p.showDeleted).component,
        <.h4("Other Causes of Issues"),
        OtherCauses.Props(p.cp, p.b, p.cd).component)
    ).build

  // ===================================================================================================================

  object UserDefIssues {

    case class Props(cp: ClientProtocol, remote: CustomIssueTypeCrud.Remote, clientData: ClientData, showDeleted: Boolean) {
      def component = Component(this)
    }

    val fields = FieldSet2[CustomIssueType](_.key.value, _.desc getOrElse "")(("", ""))
    val storesAndState = TypicalStoresAndState(fields).keyedBy[CustomIssueType.Id]
    import storesAndState._

    val Component =
      ReactComponentB[Props]("Cfg: User-Defined Issue Types")
        .getInitialState(initialState)
        .backend(new Backend(_))
        .render(_.backend.render)
        .configure(
          RemoteDeltaListener(CustomIssueType, CustomIssueTypeCrud)
            .installS(savedRowStoreS, Partition.CustomIssueTypes, _.clientData))
        .build

    private def initialState(p: Props): S =
      State(newRowStore.initState,
        savedRowStore.initStateIM(p.clientData.project.customIssueTypes.data),
        p.showDeleted)

    def validatorState(k: Option[CustomIssueType.Id], cd: ClientData): S => V.S =
      s => {
        val ts: RefKeyVS.Data[Tag.Id] = // TODO cacheable
          (None, cd.project.tags.data.vstream(_.tag)
            .map(t => t.keyO.map(k => (t.id.some, k))).filter(_.isDefined).map(_.get))
        val is: RefKeyVS.Data[CustomIssueType.Id] =
          (k, savedRowStoreS.getAllP(s).map(i => (i.id.some, i.key)))
        RefKeyVS(ts, is)
      }

    final class Backend(c: BackendScope[Props, S]) extends OnUnmount {
      val crudIO = CrudIO(CustomIssueType, CustomIssueTypeCrud)(c.props.cp, c.props.remote, c.props.clientData)
      val supp = TypicalSupp(storesAndState, crudIO)(c, _.alive)

      def valState(k: Option[CustomIssueType.Id]) = validatorState(k, c.props.clientData)

      val rowE = {
        val keyE  = Editors.textInputEditor.applyValidator(V.keyS)
        val descE = Editors.textareaEditor.applyValidator(V.descS)
        val e = Editor.merge2S(fields, keyE, descE).tupleI.zoomU[S]

        // TODO simplify
        val saveFn = Persistence.asyncSaveS(V.all, savedRowStoreS)(
          newRowStoreS,
          valState(None),
          k => valState(k.some),
          supp.saveNeed(p => (p.key, p.desc)),
          crudIO.createIO, crudIO.updateIO,
          c runState _)

        supp.addEditorFeatures2(e)(saveFn, _._1.customIssueData._1)
      }

      val table = {
        def rowRenderer =
          new CfgTable.RowRenderer[CustomIssueType, rowE.View, (TagMod, TagMod)] {
            override def newRow     = identity
            override def savedRow   = (v, p) => v
            override def deletedRow = p => (p.key.value, TextMod.nonBlank from p.desc)
            override def render     = { case (key, desc) => List(key, desc) }
          }
        val t = CfgTable(rowE, savedRowStoreS, newRowStoreS).build(
          _.key, rowRenderer,
          i => (valState(None)(c.state), i),
          k => (valState(k.some)(c.state), savedRowStoreS.getI(k)(c.state)),
          supp.deletion,  _.showDeleted, c)
        val headerRow = CfgTable.header(List(FieldNames.refKey, FieldNames.desc))
        () => t.table(headerRow, Stream.empty)
      }

      def render: ReactElement =
        CfgTable.outer(storesAndState)(c, table())
    }
  }

  // ===================================================================================================================

  object OtherCauses {

    case class Props(cp: ClientProtocol, remote: CustomReqTypeImplicationMod.Remote, clientData: ClientData) {
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
          .installS(savedRowStore, Partition.CustomReqTypes, _.clientData))
      .build

    private def initialState(p: Props): S =
      savedRowStore.initStateIM(p.clientData.project.customReqTypes.data)

    def label(r: ReqType): String = s"${r.mnemonic.value}: ${r.name}"

    final class Backend(c: BackendScope[Props, S]) extends OnUnmount {

      def save(p: Props, id: CustomReqType.Id): ST =
        ReactS.liftR[IO, S, Unit](state => {
          val setStatus = savedRowStore.setStatusST[IO](id)
          val saveio = Persistence.retryably[ST](retry => {
            val v = savedRowStore.getI(id)(state)
            val f = Persistence.failureIO(retry)(c runState _, setStatus)
            val io = c.props.cp.call(p.remote)((id, v), p.clientData.update, f)
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
