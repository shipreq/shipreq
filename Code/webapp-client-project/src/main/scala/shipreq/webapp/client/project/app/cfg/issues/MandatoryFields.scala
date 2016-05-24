package shipreq.webapp.client.project.app.cfg.issues

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.extra.{Px, OnUnmount}
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._, DataImplicits._
import shipreq.webapp.base.protocol.FieldMandatorinessMod
import shipreq.webapp.client.base.data.On
import shipreq.webapp.client.base.protocol.ClientProtocol
import shipreq.webapp.client.project.app.state.{ClientData, ChangeListener}
import shipreq.webapp.client.project.app.cfg.shared._
import shipreq.webapp.client.project.lib.DataReusability._

private[issues] object MandatoryFields {

  case class Props(cp: ClientProtocol, remote: FieldMandatorinessMod.Instance, clientData: ClientData) {
    @inline def component = Component(this)
  }

  val rowStore = SavedRowStore.data[CustomField](_.mandatory)

  type S = rowStore.State
  val  ST = ReactS.FixCB[S]
  type ST = ST.T[Unit]

  val changeListener = ChangeListener.store(rowStore)(_.customFieldTypes, _.config.fields.customFields.get)

  val Component = ReactComponentB[Props]("MandatoryFields")
    .initialState_P(initialState)
    .renderBackend[Backend]
    .configure(
      changeListener.install(_.clientData),
      ChangeListener.refreshWhenFieldNamesChange.install(_.clientData)
    )
    .build

  private def initialState(p: Props) =
    rowStore.initStateIM(p.clientData.project().config.fields.customFields)

  final class Backend($: BackendScope[Props, S]) extends OnUnmount {

    val pxProject = Px.bs($).propsA(_.clientData.project())
    val labelFn   = pxProject map Field.nameP

    def save(id: CustomFieldId): CallbackTo[ST] =
      $.props.map(p =>
        Persistence.simpleAsyncUpdate(rowStore)(p.remote, p.clientData, p.cp, $ runState _, id))

    val genEditor =
      Editors.checkboxEditor.imap(On <=> Mandatory)
        .strengthR[Field].labelSuffix(a => labelFn.value()(a._2))

    val editor =
      genEditor.cmapA[(Mandatory, CustomField)](a => a)
        .zoomU[S].applyRowUpdate(rowStore)(_._2.id)
        .paddSTA(a => { case OnEditFinished(_) => save(a._2.id).runNow() })

    val editable = editor.editableByRowStatus($)

    def editorI(r: rowStore.Row): editor.Input =
      EditorI((r.i, r.p), "", editable(r.status))

    def renderStaticField(f: StaticField) =
      <.tr(
        ^.key := f.name,
        <.td(genEditor render EditorI((f.mandatory, f), "", None)))

    def renderCustomField(f: CustomField, s: S) = {
      val r = rowStore.get(f.id)(s)
      <.tr(
        ^.key := f.id.value,
        <.td(
          editor render editorI(r),
          rowStatusCtrls(r.status, EmptyTag)))
    }

    def renderRows(p: Project, s: S): ReactNode = {
      val fs = p.config.fields.fields
      HideDead(fs)(_ live p.config).toReactNodeArray(
        _.fold(renderStaticField, renderCustomField(_, s)))
    }

    def render(p: Props, s: S): ReactElement =
      <.table(
        <.thead(<.tr(<.th("Mandatory Fields"))),
        <.tbody(renderRows(p.clientData.project(), s)))
  }
}
