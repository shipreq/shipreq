package shipreq.webapp.client.project.app.cfg.issues

import japgolly.scalajs.react._, vdom.html_<^._, ScalazReact._
import japgolly.scalajs.react.extra.{Px, OnUnmount}
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._, DataImplicits._
import shipreq.webapp.base.protocol.ProjectSpaProtocols.FieldMandatorinessMod
import shipreq.webapp.base.data.On
import shipreq.webapp.base.protocol.ClientProtocol
import shipreq.webapp.client.project.app.state.{ClientData, ChangeListener}
import shipreq.webapp.client.project.app.cfg.shared._
import shipreq.webapp.client.project.lib.DataReusability._

private[issues] object MandatoryFields {

  final case class Props(cp: ClientProtocol, remote: FieldMandatorinessMod.Instance, clientData: ClientData) {
    @inline def component = Component(this)
    def proc = clientData.serverSideProcToEvents(cp, remote)
  }

  private val rowStore = SavedRowStore.data[CustomField](_.mandatory)

  private type S = rowStore.State
  private val  ST = ReactS.FixCB[S]
  private type ST = ST.T[Unit]

  private val changeListener = ChangeListener.store(rowStore)(_.customFieldTypes, _.config.fields.customFields.get)

  val Component = ScalaComponent.builder[Props]("MandatoryFields")
    .initialStateFromProps(initialState)
    .renderBackend[Backend]
    .configure(
      changeListener.install(_.clientData),
      ChangeListener.refreshWhenFieldNamesChange.install(_.clientData)
    )
    .build

  private def initialState(p: Props) =
    rowStore.initStateIM(p.clientData.project().config.fields.customFields)

  final class Backend($: BackendScope[Props, S]) extends OnUnmount {

    private val pxProject = Px.props($).map(_.clientData.project()).withReuse.autoRefresh
    private val labelFn   = pxProject map Field.nameFromProject

    private def save(id: CustomFieldId): CallbackTo[ST] =
      $.props.map(p =>
        Persistence.simpleAsyncUpdate(rowStore)(p.proc, $ runState _, id))

    private val genEditor =
      Editors.checkboxEditor.imap(On <=> Mandatory)
        .strengthR[Field].labelSuffix(a => labelFn.value()(a._2))

    private val editor =
      genEditor.cmapA[(Mandatory, CustomField)](a => a)
        .zoomU[S].applyRowUpdate(rowStore)(_._2.id)
        .paddSTA(a => { case OnEditFinished(_) => save(a._2.id).runNow() })

    private val editable = editor.editableByRowStatus($)

    private def renderStaticField(f: StaticField) =
      <.div(
        ^.key := f.name,
        genEditor render EditorI((f.mandatory, f), "", None))

    private def renderCustomField(f: CustomField, s: S) = {
      val r = rowStore.get(f.id)(s)
      <.div(
        ^.key := f.id.value,
        editor render EditorI((r.i, r.p), "", editable(r.status)),
        rowStatusCtrls(r.status, EmptyVdom))
    }

    private def renderRows(p: Project, s: S): VdomNode = {
      val fs = p.config.fields.fields
      HideDead(fs)(_ live p.config).toVdomArray(
        _.fold(renderStaticField, renderCustomField(_, s)))
    }

    def render(p: Props, s: S): VdomElement =
      <.section(
        <.h5("Mandatory Fields"),
        renderRows(p.clientData.project(), s))
  }
}
