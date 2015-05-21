package shipreq.webapp.client.app.ui.cfg.issues

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.extra.{Px, OnUnmount}
import scala.language.reflectiveCalls
import scalaz.effect.IO
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._, DataImplicits._
import shipreq.webapp.base.delta.Partition
import shipreq.webapp.base.protocol.Routines._
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.data.DataReusability._
import shipreq.webapp.client.lib.HideDead
import shipreq.webapp.client.lib.ui._
import shipreq.webapp.client.protocol.ClientProtocol
import shipreq.webapp.client.util.On

private[issues] object MandatoryFields {

  case class Props(cp: ClientProtocol, remote: FieldMandatorinessMod.Remote, clientData: ClientData) {
    @inline def component = Component(this)
  }

  val rowStore = SavedRowStore.data[CustomField](_.mandatory)

  type S = rowStore.State
  val  ST = ReactS.FixT[IO, S]
  type ST = ST.T[Unit]

  val fieldListener =
    DeltaListener.store(rowStore).partialHandler(Partition.Fields)(_.foldId(_ => None, _.some), _.field.toOption)

  val Component = ReactComponentB[Props]("MandatoryFields")
    .getInitialState(initialState)
    .backend(new Backend(_))
    .render(_.backend.render)
    .configure(
      DeltaListener.apply  [Props, S, Backend, TopNode](_.clientData, fieldListener) compose
      DeltaListener.refresh[Props, S, Backend, TopNode](_.clientData, Field.nameAffectingPartitions)
    )
    .build

  private def initialState(p: Props) =
    rowStore.initStateIM(p.clientData.project.fields.data.customFields)

  final class Backend($: BackendScope[Props, S]) extends OnUnmount {

    @inline def project = $.props.clientData.project

    val pxProject = Px.thunkM(project)
    val labelFn   = pxProject map Field.nameP

    def save(id: CustomFieldId): ST = {
      val p = $.props
      Persistence.simpleAsyncUpdate(rowStore)(p.remote, p.clientData, p.cp, $ runState _, id)
    }

    val genEditor =
      Editors.checkboxEditor.imap(On <=> Mandatory)
        .strengthR[Field].labelSuffix(a => UI.mustA(labelFn.value()(a._2)))

    val editor =
      genEditor.cmapA[(Mandatory, CustomField)](a => a)
        .zoomU[S].applyRowUpdate(rowStore)(_._2.id)
        .paddSTA(a => { case OnEditFinished(_) => save(a._2.id) })

    val editable = editor.editableByRowStatus($)

    def editorI(r: rowStore.Row): editor.Input =
      EditorI((r.i, r.p), "", editable(r.status))

    def renderStaticField(f: StaticField) =
      <.tr(
        ^.key := f.name,
        <.td(genEditor render EditorI((f.mandatory, f), "", None)))

    def renderCustomField(f: CustomField) = {
      val r = rowStore.get(f.id)($.state)
      <.tr(
        ^.key := f.id.value,
        <.td(
          editor render editorI(r),
          UI.rowStatusCtrls(r.status, EmptyTag)))
    }

    def renderRows: ReactNode =
      UI.must(project.fields.data.fields)(
        HideDead(_)(_.alive).toReactNodeArray(
          _.fold(renderStaticField, renderCustomField)))

    def render: ReactElement = {
      pxProject.refresh()
      <.table(
        <.thead(<.tr(<.th("Mandatory Fields"))),
        <.tbody(renderRows))
    }
  }
}
