package shipreq.webapp.client.project.app.cfg.issues

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.OnUnmount
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.On
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.protocol.ProjectSpaProtocols.WsReqRes.ReqTypeImplicationMod
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.client.project.app.state.{ChangeListener, ClientData}
import shipreq.webapp.client.project.app.cfg.shared._
import DataImplicits._
import ReqType.Mnemonic

private[issues] object ReqTypeImplication {

  final case class Props(remote: ServerSideProcInvoker[ReqTypeImplicationMod.RequestType, ErrorMsg, VerifiedEvent.Seq],
                         clientData: ClientData) {
    @inline def component = Component(this)
  }

  val rowStore = SavedRowStore.data[CustomReqType](_.imp)
  import rowStore.{State => S}
  val  ST = ReactS.FixCB[S]
  type ST = ST.T[Unit]

  val changeListener = ChangeListener.store(rowStore)(_.customReqTypes, _.config.reqTypes.custom.get)

  val Component = ScalaComponent.builder[Props]("ReqTypeImplication")
    .initialStateFromProps(initialState)
    .renderBackend[Backend]
    .configure(changeListener.install(_.clientData))
    .build

  private def initialState(p: Props): S =
    rowStore.initStateIM(p.clientData.project().config.reqTypes.custom)

  private def label(r: ReqType): String =
    s"${r.mnemonic.value}: ${r.name}"

  final class Backend($: BackendScope[Props, S]) extends OnUnmount {

    def save(id: CustomReqTypeId): CallbackTo[ST] =
      $.props.map(p =>
        Persistence.simpleAsyncUpdate(rowStore)(p.remote, $ runState _, id))

    val genEditor =
      Editors.checkboxEditor.imap(On <=> ImplicationRequired)
        .strengthR[ReqType].labelSuffix(a => label(a._2))

    val editor =
      genEditor.cmapA[(ImplicationRequired, CustomReqType)](a => a)
        .zoomU[S].applyRowUpdate(rowStore)(_._2.id)
        .paddSTA(a => { case OnEditFinished(_) => save(a._2.id).runNow() })

    val editable = editor.editableByRowStatus($)

    def editorI(r: rowStore.Row): editor.Input =
      EditorI((r.i, r.p), "", editable(r.status))

    type Rows = Stream[(Mnemonic, VdomElement)]

    def customRows(s: S): Rows =
      rowStore.getAll(s).filter(_.p.live is Live).map(r => {
        val re: VdomElement =
          <.div(^.key := r.p.id.value,
            editor render editorI(r),
            rowStatusCtrls(r.status, EmptyVdom))
        (r.p.mnemonic, re)
      })

    val staticRows: Rows =
      StaticReqType.values.toStream.map(s => {
        val re: VdomElement =
          <.div(^.key := s.mnemonic.value,
            genEditor render EditorI((s.imp, s), "", None))
        (s.mnemonic, re)
      })

    def renderRows(s: S) =
      (staticRows #::: customRows(s)).sortBy(_._1).map(_._2).toVdomArray

    def render(s: S): VdomElement =
      <.section(
        <.h5("Req-Types Requiring Implication"),
        renderRows(s))
  }
}
