package shipreq.webapp.client.app.reqdetail

import japgolly.scalajs.react._
import japgolly.scalajs.react.experimental.StaticPropComponent
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import scalaz.{-\/, \/-}
import shipreq.base.util.{Direction, MutableArray, Valid}
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.{UpdateContentCmd, UpdateContentFn}
import shipreq.webapp.base.text.{PlainText, TextSearch}
import shipreq.webapp.client.app.state.ClientData
import shipreq.webapp.client.data.{Plain, ShowDead, FilterDead, DataLogic}
import shipreq.webapp.client.feature._
import shipreq.webapp.client.lib.DataReusability._
import shipreq.webapp.client.protocol.{ServerCall, ClientProtocol}
import shipreq.webapp.client.widgets.high.ProjectWidgets

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
                          filterDead: FilterDead,
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

    class Temp(project: Project, req: GenericReq, upstreamFD: FilterDead) {
      val live = req.live(project.config.customReqTypes)
      val filterDead = live match {
        case Live => upstreamFD
        case Dead => ShowDead
      }
      val codeSet         = project.reqCodes.activeReqCodesByReqId(req.id)
      val codes           = MutableArray(codeSet).sortBySchwartzian(PlainText.reqCode).to[List]
      val tagDist         = DataLogic.tagFieldDist(project.config, filterDead, _ => true)
      val tagLookup       = DataLogic.tagLookup(project, filterDead)
      val generalTagSet   = DataLogic.generalTags(tagDist, tagLookup)(req.id)
      val tagOrderByName  = DataLogic.tagOrderByName(project.config.tags)
      val tagOrderByPos   = DataLogic.tagOrderByPos(project.config.tags)
      val generalTags     = MutableArray(generalTagSet).sortBy(tagOrderByName.apply).to[Vector]
      val pubidText       = PlainText.pubid(project, req.pubid)
      val impFilter       = DataLogic.impValueFilter(project.config, filterDead)
      val customImpLookup = DataLogic.customFieldImps(project, impFilter)

      val generalImps: Direction => Vector[Pubid] =
        Direction.memo(dir =>
          project.implications(dir)(req.id)
            .iterator
            .map(project.reqs.req)
            .filter(impFilter)
            .map(_.pubid)
            .to[Vector]
        )
    }

//    def generalImplicationValues(project: Project, req: GenericReq): Direction => Vector[Pubid] =
//      Direction.memo { dir => }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    def renderDetail(p: DynamicProps, project: Project, req: GenericReq): ReactElement = {
      val pw = pxProjectWidgets.value()
      val s = p.reqState(req.id)
      val fieldName = pxFieldNameFn.value()

      val editFeature = {
        import ContentEditorFeature._
        import s.initEditor

        val static = Static(
          initEditor.parent, initEditor.preview, pxProject, pxPlainText, pxProjectWidgets, pxTextSearch, updateIO)

        def generalImps(cell: Cell) = {
          // TODO OMG!
          val temp = new Temp(project, req, p.filterDead)
          Editor.ImplicationsAll(req, Cell.implicationDirection(cell), temp.generalImps(Cell.implicationDirection(cell)))
        }

        val edit: Cell => Option[Editor[Cell]] = cell =>
          Some(cell match {
            case Cell.Title                                        => Editor.GenericReqTitle(req, cell)
            case Cell.Code                                         => Editor.ReqCodesForReq(req)
            case Cell.ImplicationSrc
               | Cell.ImplicationTgt                               => generalImps(cell)
            case Cell.ReqType                                      => Editor.ReqType(req)
            case Cell.Tags                                         => Editor.Tags(req, None)
            case Cell.CustomField(fid: CustomField.Tag        .Id) => Editor.Tags(req, Some(fid))
            case Cell.CustomField(fid: CustomField.Text       .Id) => Editor.CustomTextField(req, fid, cell)
            case Cell.CustomField(fid: CustomField.Implication.Id) => Editor.ImplicationsCustomField(req, fid)
          })

        initEditor.feature((cell, el) =>
          D0.Feature(static, s.asyncFeature(cell))(el, edit(cell)))
      }

      // TODO ↓ needn't do all this each time
      val temp = new Temp(project, req, p.filterDead)
      import temp._

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
        <.span(
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

      def renderImpCell(cell: Cell, value: => TraversableOnce[Pubid]) = {
        def pubids = MutableArray(value)
          .sortBySchwartzian(DataLogic.pubidSortKeyFn(project.config))
          .to[Vector]
        renderAsyncEditorOrValue(
          cell,
          pw.pubidRefList(Plain, Valid)(pubids))
      }

      // TODO Much much overlap with Table.CellProps
      def renderRowData(row: Row): TagMod =
        row match {

          case Row.CustomField(f: CustomField.Text) =>
            renderAsyncEditorOrValue(
              Cell.CustomField(f.id),
              pw.customTextField(f.id)(req).fold(emptyRow)(w => w))

          case Row.Code =>
            renderAsyncEditorOrValue(
              Cell.Code,
              pw.flatReqCodes(codes))

          case Row.ReqType =>
            renderAsyncEditorOrValue(
              Cell.ReqType,
              pw.reqTypeFull(req.reqTypeId)) // ---- Note for refactoring: reqTypeFull differs from how ReqTable does it

          case Row.Tags =>
            renderAsyncEditorOrValue(
              Cell.Tags,
              pw.tagList(generalTags))

          case Row.CustomField(f: CustomField.Tag) =>
            def tagSet = DataLogic.customFieldTags(tagDist, tagLookup, f.id)(req.id)
            def tags = MutableArray(tagSet).sortBy(tagOrderByPos.apply).to[Vector]
            renderAsyncEditorOrValue(
              Cell.CustomField(f.id),
              pw.tagList(tags))

          case Row.Implications =>
            def one(cell: Cell) = {
              def pubids = temp.generalImps(Cell.implicationDirection(cell))
              renderImpCell(cell, pubids)
            }
            <.div(
              <.span(one(Cell.ImplicationSrc)),
              <.span(s"→ $pubidText →"),
              <.span(one(Cell.ImplicationTgt)))

          case Row.CustomField(f: CustomField.Implication) =>
            val pubids = customImpLookup(f)(req.id)
            renderImpCell(Cell.CustomField(f.id), pubids)
        }

      rows(project, req)

      <.div(
        <.h2(
          pubidText + ": ",
          renderTitle),
        renderRows,
        <.code(<.pre(req.toString)))
    }
    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━




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
