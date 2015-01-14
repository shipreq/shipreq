package shipreq.webapp.client.app.ui.cfg.issues

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.extra.OnUnmount
import scala.language.reflectiveCalls
import scalaz.effect.IO
import scalaz.syntax.equal._
import shipreq.webapp.base.data._, DataImplicits._
import shipreq.webapp.base.delta.Partition
import shipreq.webapp.base.protocol.Routines._
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.lib.ui._
import shipreq.webapp.client.protocol.ClientProtocol
import ReqType.Mnemonic

private[issues] object OtherCauses {

  case class Props(cp: ClientProtocol, remote: ReqTypeImplicationMod.Remote, clientData: ClientData) {
    @inline def component = Component(this)
  }

  val savedRowStore = SavedRowStore.data[CustomReqType](_.imp)
  import savedRowStore.{State => S}
  val  ST = ReactS.FixT[IO, S]
  type ST = ST.T[Unit]

  val Component = ReactComponentB[Props]("OtherCauses")
    .getInitialState(initialState)
    .backend(new Backend(_))
    .render(_.backend.render)
    .configure(
      RemoteDeltaListener(CustomReqType).installS(savedRowStore, Partition.CustomReqTypes, _.clientData))
    .build

  private def initialState(p: Props): S =
    savedRowStore.initStateIM(p.clientData.project.customReqTypes.data)

  private def label(r: ReqType): String =
    s"${r.mnemonic.value}: ${r.name}"

  final class Backend($: BackendScope[Props, S]) extends OnUnmount {

    def save(p: Props, id: CustomReqType.Id): ST =
      ReactS.liftR[IO, S, Unit](state => {
        val setStatus = savedRowStore.setStatusST[IO](id)
        val saveio = Persistence.retryably[ST](retry => {
          val v = savedRowStore.getI(id)(state)
          val f = Persistence.failureIO(retry)($ runState _, setStatus)
          val io = $.props.cp.call(p.remote)((id, v), p.clientData.update, f)
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
        .paddSTA(a => { case OnEditFinished(_) => save($.props, a._2.id) })

    val editable = editor.editableByRowStatus($)

    def editorI(r: savedRowStore.Row): editor.Input =
      EditorI((r.i, r.p), "", editable(r.status))

    type Rows = Stream[(Mnemonic, ReactElement)]

    def savedRows: Rows =
      savedRowStore.getAll($.state).filter(_.p.alive ≟ Alive).map(r => {
        val re: ReactElement =
          <.tr(^.key := r.p.id.value,
            <.td(
              editor render editorI(r),
              UI.rowStatusCtrls(r.status, EmptyTag)))
        (r.p.mnemonic, re)
      })

    val staticRows: Rows =
      StaticReqType.values.list.toStream.map(s => {
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
