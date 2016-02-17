package shipreq.webapp.client.app.reqdetail

import japgolly.scalajs.react._
import japgolly.scalajs.react.experimental.StaticPropComponent
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import shipreq.base.util.MutableArray
import scalaz.{-\/, \/-}
import shipreq.base.util.ScalaExt._
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

        // TODO Imps: Need to filter values
        // Editor.ImplicationsAll(row.req, Column.implicationDirection(col), pubids))

        val edit: Cell => Option[Editor[Cell]] = cell =>
          Some(cell match {
            case Cell.Title                                        => Editor.GenericReqTitle(req, cell)
            case Cell.Code                                         => Editor.ReqCodesForReq(req)
//            case Cell.ImplicationSrc                               =>
//            case Cell.ImplicationTgt                               =>
            case Cell.ReqType                                      => Editor.ReqType(req)
            case Cell.Tags                                         => Editor.Tags(req, None)
            case Cell.CustomField(fid: CustomField.Tag        .Id) => Editor.Tags(req, Some(fid))
            case Cell.CustomField(fid: CustomField.Text       .Id) => Editor.CustomTextField(req, fid, cell)
            case Cell.CustomField(fid: CustomField.Implication.Id) => Editor.ImplicationsCustomField(req, fid)
          })

        initEditor.feature((cell, el) =>
          D0.Feature(static, s.asyncFeature(cell))(el, edit(cell)))
      }

      def renderAsyncEditorOrValue(cell: Cell, view: => TagMod): TagMod = {
        def startEdit = editFeature(cell).startEdit(focus)
        def editor = s.edit(cell).flatMap(_.render())
        TagMod(
          ^.onDblClick -->? startEdit,
          s.async(cell) match {
            case None    => editor.fold(view)(e => e)
            case Some(s) => s.render: TagMod
          })
      }

      def renderTitle: ReactElement =
        <.div(
          renderAsyncEditorOrValue(Cell.Title, pw.reqTitle(req)))

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
          case Row.Code           => UiText.FieldNames.reqCodes
          case Row.ReqType        => UiText.FieldNames.reqType
          case Row.Tags           => UiText.FieldNames.tags
          case Row.Implications   => UiText.FieldNames.implications
        }

      def focus: Callback = Callback.empty // TODO

      // TODO Much much overlap with Table.CellProps
      def renderRowData(row: Row): TagMod =
        row match {

          case Row.CustomField(f: CustomField.Text) =>
            renderAsyncEditorOrValue(
              Cell.CustomField(f.id),
              pw.customTextField(f.id)(req).fold(emptyRow)(w => w))

          case Row.Code =>
            val codeSet = project.reqCodes.activeReqCodesByReqId(req.id)
            val codes = MutableArray(codeSet).sortBy(PlainText.reqCode).to[List]
            // TODO ↑ needn't do each time
            renderAsyncEditorOrValue(
              Cell.Code,
              pw.flatReqCodes(codes))

          // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
//          CustomField(Implication(_, _, _, _, _)), CustomField(Tag(_, _, _, _, _)), Implications, Tags, ReqType
          case _ => "TODO"
          // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
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
