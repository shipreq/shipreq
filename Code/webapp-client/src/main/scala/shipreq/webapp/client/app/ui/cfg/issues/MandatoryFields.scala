package shipreq.webapp.client.app.ui.cfg.issues

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.extra.OnUnmount
import monocle.macros.Lenser
import scala.language.reflectiveCalls
import scalaz.effect.IO
import shipreq.base.util.{Must, Refreshable}
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._, DataImplicits._
import shipreq.webapp.base.delta.Partition
import shipreq.webapp.base.protocol.Routines._
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.lib.ui._
import shipreq.webapp.client.protocol.ClientProtocol

private[issues] object MandatoryFields {

  case class Props(cp: ClientProtocol, remote: FieldMandatorinessMod.Remote, clientData: ClientData) {
    @inline def component = Component(this)
  }

  val rowStore = SavedRowStore.data[CustomField](_.mandatory)

  case class State(rows   : rowStore.State,
                   labelFn: Refreshable[Project, Field => Must[String]])

  type S = State
  val  ST = ReactS.FixT[IO, S]
  type ST = ST.T[Unit]

  object State {
    private[this] def l = Lenser[State]
    val _rows    = l(_.rows)
    val _labelFn = l(_.labelFn)
  }

  val rowStoreS = rowStore.contramap(State._rows)

  val fieldListener =
    DeltaListener.store(rowStoreS).partialHandler(Partition.Fields)(_.foldId(_ => None, _.some), _.field.toOption)

  val Component = ReactComponentB[Props]("MandatoryFields")
    .getInitialState(initialState)
    .backend(new Backend(_))
    .render(_.backend.render)
    .configure(
      DeltaListener.apply   [Props, S, Backend](_.clientData, fieldListener) compose
      DeltaListener.refreshL[Props, S, Backend](_.clientData, _.backend.refreshLabelFn, Field.nameAffectingPartitions)
    )
    .build

  private def initialState(p: Props) =
    State(
      rowStore.initStateIM(p.clientData.project.fields.data.customFields),
      Refreshable(Field.nameP)(p.clientData.project))

  final class Backend($: BackendScope[Props, S]) extends OnUnmount {

    @inline def project = $.props.clientData.project

    def refreshLabelFn: IO[Unit] =
      $ modStateIO State._labelFn.modify(_ refresh project)

    def save(id: CustomField.Id): ST = {
      val p = $.props
      Persistence.simpleAsyncUpdate(rowStoreS)(p.remote, p.clientData, p.cp, $ runState _, id)
    }

    val genEditor =
      Editors.checkboxEditor.imap(Mandatory)
        .strengthR[Field].labelSuffix(a => UI.mustA($.state.labelFn.value(a._2)))

    val editor =
      genEditor.cmapA[(Mandatory, CustomField)](a => a)
        .zoomU[S].applyRowUpdate(rowStoreS)(_._2.id)
        .paddSTA(a => { case OnEditFinished(_) => save(a._2.id) })

    val editable = editor.editableByRowStatus($)

    def editorI(r: rowStore.Row): editor.Input =
      EditorI((r.i, r.p), "", editable(r.status))

    def renderStaticField(f: StaticField) =
      <.tr(
        ^.key := f.name,
        <.td(genEditor render EditorI((f.mandatory, f), "", None)))

    def renderCustomField(f: CustomField) = {
      val r = rowStoreS.get(f.id)($.state)
      <.tr(
        ^.key := f.id.value,
        <.td(
          editor render editorI(r),
          UI.rowStatusCtrls(r.status, EmptyTag)))
    }

    def renderRows =
      project.fields.data.fields.filter(Field.filterAlive).toReactNodeArray(
        _.fold(renderStaticField, renderCustomField))

    def render: ReactElement =
      <.table(
        <.thead(<.tr(<.th("Mandatory Fields"))),
        <.tbody(renderRows))
  }
}
