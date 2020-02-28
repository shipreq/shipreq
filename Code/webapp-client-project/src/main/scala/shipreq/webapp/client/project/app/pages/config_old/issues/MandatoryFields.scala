package shipreq.webapp.client.project.app.pages.config_old.issues

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.{OnUnmount, Px}
import shipreq.webapp.base.data._
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data.On
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.protocol.ProjectSpaProtocols.WsReqRes.FieldMandatorinessMod
import shipreq.webapp.client.project.app.state.{ChangeListener, Global}
import shipreq.webapp.client.project.app.pages.config_old.shared._
import shipreq.webapp.client.project.lib.DataReusability._
import DataImplicits._

private[issues] object MandatoryFields {

  final case class Props(remote: ServerSideProcInvoker[FieldMandatorinessMod.RequestType, ErrorMsg, VerifiedEvent.Seq],
                         global: Global) {
    @inline def component = Component(this)
  }

  private val rowStore = SavedRowStore.data[CustomField](_.mandatory)

  private type S = rowStore.State
  private val  ST = ReactS.FixCB[S]
  private type ST = ST.T[Unit]

  private val changeListener = ChangeListener.store(rowStore)(_.allCustomFieldTypes, _.config.fields.customFields.get)

  val Component = ScalaComponent.builder[Props]("MandatoryFields")
    .initialStateFromProps(initialState)
    .renderBackend[Backend]
    .configure(changeListener.install(_.global))
    .configure(ChangeListener.refreshWhenFieldNamesChange.install(_.global))
    .build

  private def initialState(p: Props) =
    rowStore.initStateIM(p.global.unsafeProject().config.fields.customFields)

  final class Backend($: BackendScope[Props, S]) extends OnUnmount {

    private val pxProjectConfig = Px.props($).map(_.global.unsafeProject().config).withReuse.autoRefresh
    private val labelFn         = pxProjectConfig.map(Field.nameFromProjectConfig)

    private def save(id: CustomFieldId): CallbackTo[ST] =
      $.props.map(p =>
        Persistence.simpleAsyncUpdate(rowStore)(p.remote, $ runState _, id))

    private val genEditor =
      Editors.checkboxEditor.imap(On <=> Mandatory)
        .strengthR[Field].labelSuffix(a => labelFn.value()(a._2))

    private val editor =
      genEditor.cmapA[(Mandatory, CustomField)](a => a)
        .zoomU[S].applyRowUpdate(rowStore)(_._2.id)
        .paddSTA(a => { case OnEditFinished(_) => save(a._2.id).runNow() })

    private val editable = editor.editableByRowStatus($)

    private val renderTitle = {
      val ed = Editors.checkboxEditor.imap(On <=> Mandatory).labelSuffix(_ => "Title")
      <.div(
        ^.key := "title",
        ed render EditorI(Mandatory, "", None))
    }

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
      val a = VdomArray.empty()
      a += renderTitle
      for (f <- HideDead(fs)(_ live p.config))
        a += f.fold(renderStaticField, renderCustomField(_, s))
      a
    }

    def render(p: Props, s: S): VdomElement =
      <.section(
        <.h5("Mandatory Fields"),
        renderRows(p.global.unsafeProject(), s))
  }
}
