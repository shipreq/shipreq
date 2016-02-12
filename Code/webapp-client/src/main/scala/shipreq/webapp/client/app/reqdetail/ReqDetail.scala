package shipreq.webapp.client.app.reqdetail

import japgolly.scalajs.react._, vdom.prefix_<^._
import japgolly.scalajs.react.experimental.StaticPropComponent
import japgolly.scalajs.react.extra._
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import shipreq.webapp.base.UiText
import shipreq.webapp.base.protocol.UpdateContentFn
import shipreq.webapp.base.text.{TextSearch, PlainText}
import shipreq.webapp.client.app.state.ClientData
import shipreq.webapp.client.protocol.ClientProtocol
import shipreq.webapp.client.widgets.high.ProjectWidgets
import scalacss.ScalaCssReact._
import scalaz.{\/, -\/, \/-}
import shipreq.base.util.NonEmptyVector
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
// import shipreq.webapp.client.app.Style.{reqtable => *}
import shipreq.webapp.client.lib._
import shipreq.webapp.client.widgets.DragToReorder
import DataReusability._
import DomUtil._

object ReqDetail extends StaticPropComponent.Template("ReqDetail") {
  override protected def configureBackend = new Backend(_, _)
  override protected def configureRender  = _.renderBackend
//  override protected def configure = _.configure(
//    Listenable.install(_.static.cd, $ => (c: Changes) => $.props.static.state_$.modState(_ recvChanges c)))

  // TODO All needed?
  case class StaticProps(cd              : ClientData,
                         cp              : ClientProtocol,
                         updateContentFn : UpdateContentFn.Instance,
                         pxPlainText     : Px[PlainText.ForProject],
                         pxTextSearch    : Px[TextSearch],
                         pxProjectWidgets: Px[ProjectWidgets])

  case class DynamicProps(extPubid: ExternalPubid)

  final class Backend(SP: StaticProps, $: BackendScope) {
    import SP._
    import cd.pxProject

    val pxFieldNameFn = pxProject.map(Field.nameP)
    val pxFieldRenderer = pxProjectWidgets.map(new FieldRenderer(_))

    def render(p: DynamicProps): ReactElement = {
      val project = pxProject.value()
      project.findReq(p.extPubid) match {
        case \/-(req)                                 => renderDetail(project, p.extPubid, req)
        case -\/(PubidQueryError.InvalidReqType)      => renderNotFound(s"${UiText.FieldNames.reqType} ${p.extPubid.mnemonic.value} not found.")
        case -\/(PubidQueryError.InvalidPos(rt, len)) => renderNotFound(s"${PlainText pubid p.extPubid} not found.")
      }
    }

    def renderNotFound(failureReason: String): ReactElement =
      <.div(
        <.h2("ERROR"),
        <.h5(failureReason))

    def renderDetail(project: Project, ep: ExternalPubid, req: Req): ReactElement = {
      <.div(
        <.h2(s"${PlainText pubid ep}: [TITLE HERE]"),
        renderFields(project.config.fields.fieldsForReqType(req.reqTypeId), req),
        <.code(<.pre(req.toString)))
    }

    def renderFields(fields: Vector[Field], req: Req): ReactElement = {
      val nameFn = pxFieldNameFn.value()
      val r = pxFieldRenderer.value()

      def row(f: Field) =
        <.tr(
          <.th(nameFn(f)),
          <.td(r.renderField(f)(req)))

      <.table(<.tbody(fields.iterator.map(row)))
    }
  }
}

//object FieldRenderer {
//  sealed trait Type
//
//  case object SingleLineText extends Type
//}

class FieldRenderer(widgets: ProjectWidgets) {
  val empty: ReactElement = <.span

  def renderField(field: Field): Req => ReactElement =
    field match {

      case f: CustomField.Text =>
        val g = widgets.customTextField(f.id)
        req => g(req).fold(empty)(w => w)

      // TODO Add field for tags not in custom fields. (Will also need to extract.)

      case f: CustomField.Tag =>
        // TODO Extract relevant tags
        _ => <.span("TODO: " + f)

      case f: CustomField.Implication =>
        _ => <.span("TODO: " + f)

      case StaticField.NormalAltStepTree =>
        _ => <.span("TODO: NormalAltStepTree")

      case StaticField.ExceptionStepTree =>
        _ => <.span("TODO: ExceptionStepTree")

      case StaticField.StepGraph =>
        _ => <.span("TODO: StepGraph")
    }
}