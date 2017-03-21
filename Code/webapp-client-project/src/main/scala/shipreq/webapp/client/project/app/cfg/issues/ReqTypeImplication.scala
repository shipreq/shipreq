package shipreq.webapp.client.project.app.cfg.issues

import japgolly.scalajs.react._, vdom.html_<^._, ScalazReact._
import japgolly.scalajs.react.extra.OnUnmount
import shipreq.webapp.base.data._, DataImplicits._
import shipreq.webapp.base.protocol.ReqTypeImplicationMod
import shipreq.webapp.client.base.data.On
import shipreq.webapp.client.base.protocol.ClientProtocol
import shipreq.webapp.client.project.app.state.{ClientData, ChangeListener}
import shipreq.webapp.client.project.app.cfg.shared._
import ReqType.Mnemonic

private[issues] object ReqTypeImplication {

  final case class Props(cp: ClientProtocol, remote: ReqTypeImplicationMod.Instance, clientData: ClientData) {
    @inline def component = Component(this)
  }

  val rowStore = SavedRowStore.data[CustomReqType](_.imp)
  import rowStore.{State => S}
  val  ST = ReactS.FixCB[S]
  type ST = ST.T[Unit]

  val changeListener = ChangeListener.store(rowStore)(_.customReqTypes, _.config.reqTypes.custom.get)

  val Component = ScalaComponent.builder[Props]("ReqTypeImplication")
    .initialState_P(initialState)
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
        Persistence.simpleAsyncUpdate(rowStore)(p.remote, p.clientData, p.cp, $ runState _, id))

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
      rowStore.getAll(s).filter(_.p.live :: Live).map(r => {
        val re: VdomElement =
          <.tr(^.key := r.p.id.value,
            <.td(
              editor render editorI(r),
              rowStatusCtrls(r.status, EmptyVdom)))
        (r.p.mnemonic, re)
      })

    val staticRows: Rows =
      StaticReqType.values.toStream.map(s => {
        val re: VdomElement =
          <.tr(^.key := s.mnemonic.value,
            <.td(genEditor render EditorI((s.imp, s), "", None)))
        (s.mnemonic, re)
      })

    def renderRows(s: S) =
      (staticRows #::: customRows(s)).sortBy(_._1).map(_._2).toVdomArray

    def render(s: S): VdomElement =
      <.table(
        <.thead(<.tr(<.th("Req-Types Requiring Implication"))),
        <.tbody(renderRows(s)))
  }
}
