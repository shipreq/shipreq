package shipreq.webapp.client.app.reqdetail

import japgolly.scalajs.react._
import japgolly.scalajs.react.experimental.StaticPropComponent
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import scalaz.{-\/, \/-}
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.{UpdateContentCmd, UpdateContentFn}
import shipreq.webapp.base.text.{PlainText, TextSearch}
import shipreq.webapp.client.app.state.ClientData
import shipreq.webapp.client.protocol.{ServerCall, ClientProtocol}
import shipreq.webapp.client.widgets.high.ProjectWidgets
import shipreq.webapp.client.feature._
import shipreq.webapp.client.lib.DataReusability._

object ReqDetail extends StaticPropComponent.Template("ReqDetail") {
  override protected def configureBackend = new Backend(_, _)
  override protected def configureRender  = _.renderBackend
//  override protected def configure = _.configure(
//    Listenable.install(_.static.cd, $ => (c: Changes) => $.props.static.state_$.modState(_ recvChanges c)))

  type InitEditor = ContentEditorFeature.D1.InitChild[Cell, Cell]

  // TODO Remove unused
  case class StaticProps(cd              : ClientData,
                         cp              : ClientProtocol,
                         updateContentFn : UpdateContentFn.Instance,
                         pxPlainText     : Px[PlainText.ForProject],
                         pxTextSearch    : Px[TextSearch],
                         pxProjectWidgets: Px[ProjectWidgets])

  case class DynamicProps(extPubid: ExternalPubid,
                          reqState: GenericReqId => ReqState)

  case class ReqState(initEditor      : InitEditor,
                      asyncFeature    : AsyncActionFeature.D1.Feature[Cell, String],
                      edit : ContentEditorFeature.D1.State.ReadOnly[Cell],
                      async: AsyncActionFeature  .D1.State.ReadOnly[Cell, String])

  final class Backend(SP: StaticProps, $: BackendScope) {
    import SP._
    import cd.pxProject

    val pxFieldNameFn = pxProject.map(Field.nameP)

    val updateIO: ServerCall[UpdateContentCmd] =
      ServerCall.to(updateContentFn, cp, cd)

    def renderNotFound(failureReason: String): ReactElement =
      <.div(
        <.h2("ERROR"),
        <.h5(failureReason))

    def render(p: DynamicProps): ReactElement = {
      val project = pxProject.value()

      project.findReq(p.extPubid) match {
        case \/-(req)                                 => renderDetail(p, project, req match { case g: GenericReq => g })
        case -\/(PubidQueryError.InvalidReqType)      => renderNotFound(s"${UiText.FieldNames.reqType} ${p.extPubid.mnemonic.value} not found.")
        case -\/(PubidQueryError.InvalidPos(rt, len)) => renderNotFound(s"${PlainText pubid p.extPubid} not found.")
      }
    }

    def rows(project: Project, req: Req): Vector[Row] = {
      val fields = project.config.fields.fieldsForReqType(req.reqTypeId)
      Row.head ++ fields.iterator.map(Row.fromField)
    }

    def renderDetail(p: DynamicProps, project: Project, req: GenericReq): ReactElement = {
      val pw = pxProjectWidgets.value()
      val s = p.reqState(req.id)
      val fieldName = pxFieldNameFn.value()

      val editFeature = {
        import ContentEditorFeature._
        import s.initEditor

        val static = Static(
          initEditor.parent, initEditor.preview, pxProject, pxPlainText, pxProjectWidgets, pxTextSearch, updateIO)

        val edit: Cell => Option[Editor[Cell]] = cell =>
          Some(cell match {
            case Cell.CustomField(fid: CustomField.Text.Id) =>
              Editor.CustomTextField(req, fid, cell)
          })

        initEditor.feature((cell, el) =>
          D0.Feature(static, s.asyncFeature(cell))(el, edit(cell)))
      }

      def renderTitle: ReactElement =
        s.edit(Cell.Title).flatMap(_.render()) getOrElse
          pw.reqTitle(req)

      def renderRows =
        <.table(
          <.tbody(
            rows(project, req).iterator.map(renderRow)))

      def renderRow(row: Row): ReactElement =
        <.tr(
          ^.key := row.key,
          <.th(renderRowTitle(row)),
          <.td(renderRowData(row)))

      def renderRowTitle(row: Row): ReactNode =
        row match {
          case Row.CustomField(f) => fieldName(f)
          case Row.Code           => UiText.ColumnNames.code
          case Row.ReqType        => UiText.ColumnNames.reqType
          case Row.Tags           => UiText.ColumnNames.tags
          case Row.Implications   => UiText.FieldNames.implications
        }

      // TODO Much much overlap with Table.CellProps
      def renderRowData(row: Row): TagMod =
        row match {

          case Row.CustomField(f: CustomField.Text) =>

            def focus: Callback =
              Callback.empty // TODO

            val cell = Cell.CustomField(f.id)
            def startEdit = editFeature(cell).startEdit(focus)
            def editor = s.edit(cell).flatMap(_.render())
            def roView = pw.customTextField(f.id)(req).fold(emptyRow)(w => w)

            TagMod(
              ^.onDblClick -->? startEdit,
              s.async(cell) match {
                case None    => editor getOrElse[ReactElement] roView
                case Some(s) => s.render
              })


          case _ =>
            "TODO"
        }

      rows(project, req)

      <.div(
        <.h2(
          PlainText.pubid(p.extPubid) + ": ",
          renderTitle),
        renderRows,
        <.code(<.pre(req.toString)))
    }




  } // Backend


  /*

      renderFields(project.config.fields.fieldsForReqType(req.reqTypeId), req),

  def renderFields(fields: Vector[Field], req: Req): ReactElement = {

    val temp = req match {
      case g: GenericReq => g
    }

    def row(f: Field) =
      <.tr(
        <.th(
          nameFn(f)),
        <.td(
          ??? // RowComponent (RowProps(temp))
        ))

    <.table(<.tbody(fields.iterator.map(row)))
  }
  */

  // ===================================================================================================================
  // Row

  val emptyRow: ReactElement = <.span

  /*
  case class RowProps(req: GenericReq,
                      row: Row,
                      editState: ContentEditorFeature.D0.State,
                      widgets: ProjectWidgets)

  implicit val reusabilityRowProps = {
    implicit def reusabilityGenericReq = Reusability.byRef[GenericReq]
    Reusability.caseClass[RowProps]
  }

  val RowComponent =
    ReactComponentB[RowProps]("Row")
      .render_P(renderRow)
      .configure(Reusability.shouldComponentUpdate)
      .build

  def renderRow(p: RowProps): ReactElement =
    p.row match {

      case Row.CustomField(id: CustomField.Text.Id) =>
        p.editState.flatMap(_.render()) getOrElse
          p.widgets.customTextField(id)(p.req).fold(emptyRow)(w => w)

      case _ =>
        <.span("TODO")
    }
  */

}
