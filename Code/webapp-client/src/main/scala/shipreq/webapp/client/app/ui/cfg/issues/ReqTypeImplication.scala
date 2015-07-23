package shipreq.webapp.client.app.ui.cfg.issues

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.extra.OnUnmount
import scala.language.reflectiveCalls
import scalaz.effect.IO
import scalaz.syntax.equal._
import shipreq.webapp.base.data._, DataImplicits._
import shipreq.webapp.base.protocol.RemoteFns._
import shipreq.webapp.client.{ChangeListener, ClientData}
import shipreq.webapp.client.lib.ui._
import shipreq.webapp.client.protocol.ClientProtocol
import shipreq.webapp.client.util.On
import ReqType.Mnemonic

private[issues] object ReqTypeImplication {

  case class Props(cp: ClientProtocol, remote: ReqTypeImplicationMod.Instance, clientData: ClientData) {
    @inline def component = Component(this)
  }

  val rowStore = SavedRowStore.data[CustomReqType](_.imp)
  import rowStore.{State => S}
  val  ST = ReactS.FixT[IO, S]
  type ST = ST.T[Unit]

  val changeListener = ChangeListener.store(rowStore)(_.customReqTypes, _.config.customReqTypes.data.get)

  val Component = ReactComponentB[Props]("ReqTypeImplication")
    .getInitialState(initialState)
    .backend(new Backend(_))
    .render(_.backend.render)
    .configure(changeListener.install(_.clientData))
    .build

  private def initialState(p: Props): S =
    rowStore.initStateIM(p.clientData.project.config.customReqTypes.data)

  private def label(r: ReqType): String =
    s"${r.mnemonic.value}: ${r.name}"

  final class Backend($: BackendScope[Props, S]) extends OnUnmount {

    def save(id: CustomReqTypeId): ST = {
      val p = $.props
      Persistence.simpleAsyncUpdate(rowStore)(p.remote, p.clientData, p.cp, $ runState _, id)
    }

    val genEditor =
      Editors.checkboxEditor.imap(On <=> ImplicationRequired)
        .strengthR[ReqType].labelSuffix(a => label(a._2))

    val editor =
      genEditor.cmapA[(ImplicationRequired, CustomReqType)](a => a)
        .zoomU[S].applyRowUpdate(rowStore)(_._2.id)
        .paddSTA(a => { case OnEditFinished(_) => save(a._2.id) })

    val editable = editor.editableByRowStatus($)

    def editorI(r: rowStore.Row): editor.Input =
      EditorI((r.i, r.p), "", editable(r.status))

    type Rows = Stream[(Mnemonic, ReactElement)]

    def customRows: Rows =
      rowStore.getAll($.state).filter(_.p.live ≟ Live).map(r => {
        val re: ReactElement =
          <.tr(^.key := r.p.id.value,
            <.td(
              editor render editorI(r),
              UI.rowStatusCtrls(r.status, EmptyTag)))
        (r.p.mnemonic, re)
      })

    val staticRows: Rows =
      StaticReqType.values.toStream.map(s => {
        val re: ReactElement =
          <.tr(^.key := s.mnemonic.value,
            <.td(genEditor render EditorI((s.imp, s), "", None)))
        (s.mnemonic, re)
      })

    def renderRows =
      (staticRows #::: customRows).sortBy(_._1).map(_._2).toReactNodeArray

    def render: ReactElement =
      <.table(
        <.thead(<.tr(<.th("Req-Types Requiring Implication"))),
        <.tbody(renderRows))
  }
}
