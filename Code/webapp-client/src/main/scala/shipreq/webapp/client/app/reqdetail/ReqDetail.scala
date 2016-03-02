package shipreq.webapp.client.app.reqdetail

import japgolly.scalajs.react._
import japgolly.scalajs.react.experimental.StaticPropComponent
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import scalacss.ScalaCssReact._
import scalaz.{\/, -\/, \/-}
import shipreq.base.util._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.{UpdateContentCmd, UpdateContentFn}
import shipreq.webapp.base.text.{Text, PlainText, TextSearch}
import shipreq.webapp.client.app.state.ClientData
import shipreq.webapp.client.app.Style.{reqdetail => *}
import shipreq.webapp.client.data._
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

  case class StaticProps(cd              : ClientData,
                         cp              : ClientProtocol,
                         updateContentFn : UpdateContentFn.Instance,
                         pxPlainText     : Px[PlainText.ForProject],
                         pxTextSearch    : Px[TextSearch],
                         pxProjectWidgets: Px[ProjectWidgets])

  case class DynamicProps(extPubid  : ExternalPubid,
                          filterDead: FilterDead,
                          reqProps  : ReqId => ReqProps)

  case class ReqProps(initEditor  : InitEditor,
                      asyncFeature: AsyncActionFeature  .D1.Feature[Cell, String],
                      edit        : ContentEditorFeature.D1.State.ReadOnly[Cell],
                      async       : AsyncActionFeature  .D1.State.ReadOnly[Cell, String])

  /**
   * All data associated with a requirement required for this screen.
   *
   * Cached by its inputs.
   */
  class Data(val project: Project, val req: Req, upstreamFD: FilterDead) {
    val live = req.live(project.config.customReqTypes)

    val filterDead = live match {
      case Live => upstreamFD
      case Dead => ShowDead
    }

    val pubidText = PlainText.pubid(project, req.pubid)

    val codeSet = project.reqCodes.activeReqCodesByReqId(req.id)
    val codes  = MutableArray(codeSet).sortBySchwartzian(PlainText.reqCode).to[List]

    val tagDist        = DataLogic.tagFieldDist(project.config, filterDead, _ => true)
    val tagLookup      = DataLogic.tagLookup(project, filterDead)
    val generalTagSet  = DataLogic.generalTags(tagDist, tagLookup)(req.id)
    val tagOrderByName = DataLogic.tagOrderByName(project.config.tags)
    val tagOrderByPos  = DataLogic.tagOrderByPos(project.config.tags)
    val generalTags    = MutableArray(generalTagSet).sortBy(tagOrderByName.apply).to[Vector]

    val customTags: CustomField.Tag.Id => Vector[ApplicableTagId] =
      Memo { fid =>
        def tagSet = DataLogic.customFieldTags(tagDist, tagLookup, fid)(req.id)
        MutableArray(tagSet).sortBy(tagOrderByPos.apply).to[Vector]
      }

    val pubidSortKeyFn  = DataLogic.pubidSortKeyFn(project.config)
    val impFilter       = DataLogic.impValueFilter(project.config, filterDead)
    val customImpLookup = DataLogic.customFieldImps(project, impFilter)

    private def sortPubids(pubids: TraversableOnce[Pubid]): Vector[Pubid] =
      MutableArray(pubids)
        .sortBySchwartzian(pubidSortKeyFn)
        .to[Vector]

    val generalImps: Direction => Vector[Pubid] =
      Direction.memo(dir =>
        sortPubids(
          project.implications(dir)(req.id)
            .iterator
            .map(project.reqs.req)
            .filter(impFilter)
            .map(_.pubid)))

    val customImps: CustomField.Implication => Vector[Pubid] =
      Memo(f =>
        sortPubids(customImpLookup(f)(req.id)))

    final class UseCaseData(val uc: UseCase) {

      private def stepTreeProps(field   : StaticField.UseCaseStepTree,
                                filter  : UseCaseSteps.Tree => Range,
                                defaultFirst: Text.UseCaseTitle.OptionalText,
                                tailStep: Boolean) = {
        val t = field.useCaseStepTree.get(uc)
        val i = filter(t)
        Temp(field, t, i, defaultFirst, tailStep)
      }

      val stepsN = stepTreeProps(
        StaticField.NormalAltStepTree,
        _ => 0 to 0,
        uc.title,
        false)

      val stepsA = stepTreeProps(
        StaticField.NormalAltStepTree,
        1 until _.children.length,
        Vector.empty,
        true)

      val stepsE = stepTreeProps(
        StaticField.ExceptionStepTree,
        _.children.indices,
        Vector.empty,
        true)
    }

    val useCaseData: Option[UseCaseData] =
      req match {
        case uc: UseCase => Some(new UseCaseData(uc))
        case _           => None
      }
  }

  case class Temp(field: StaticField.UseCaseStepTree,
                  tree: UseCaseSteps.Tree,
                  filter: Range,
                  defaultFirst: Text.UseCaseTitle.OptionalText,
                  tailStep: Boolean) {
    val mdt = tree.maxDepthTree
  }

  // TODO Better performance if cells are (components + shouldComponentRender) or cached

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  final class Backend(SP: StaticProps, $: BackendScope) {
    import SP._
    import cd.pxProject

    val pxFieldNameFn = pxProject.map(Field.nameP)
    val pxExtPubid    = Px.bsMP($).propsM(_.extPubid)
    val pxUpstreamFD  = Px.bsMP($).propsM(_.filterDead)

    val pxData: Px[PubidQueryError \/ Data] =
      for {
        p <- pxProject
        e <- pxExtPubid
        f <- pxUpstreamFD
      } yield
        p.findReq(e).map(new Data(p, _, f))

    val updateIO: ServerCall[UpdateContentCmd] =
      ServerCall.to(updateContentFn, cp, cd)

    type EditFeature = ContentEditorFeature.D1.Feature[Cell]

    def createEditFeature(initEditor  : InitEditor,
                          asyncFeature: AsyncActionFeature.D1.Feature[Cell, String],
                          data        : Data): EditFeature = {
      import ContentEditorFeature._
      import data.req

      val static = Static(
        initEditor.parent, initEditor.preview, pxProject, pxPlainText, pxProjectWidgets, pxTextSearch, updateIO)

      def generalImps(cell: Cell) = {
        val dir = cell.implicationDirection
        Editor.ImplicationsAll(req, dir, data.generalImps(dir))
      }

      @inline implicit def autoSome[P](e: Editor[P]): Option[Editor[P]] = Some(e)
      def edit(cell: Cell): Option[Editor[Cell]] =
        cell match {
          case Cell.Title                                        => Editor.ReqTitle(req, cell)
          case Cell.Code                                         => Editor.ReqCodesForReq(req)
          case Cell.ImplicationSrc
             | Cell.ImplicationTgt                               => generalImps(cell)
          case Cell.ReqType                                      => Editor.reqType(req)
          case Cell.Tags                                         => Editor.Tags(req, None)
          case Cell.CustomField(fid: CustomField.Tag        .Id) => Editor.Tags(req, Some(fid))
          case Cell.CustomField(fid: CustomField.Text       .Id) => Editor.CustomTextField(req, fid, cell)
          case Cell.CustomField(fid: CustomField.Implication.Id) => Editor.ImplicationsCustomField(req, fid)
        }

      initEditor.feature((cell, el) =>
        D0.Feature(static, asyncFeature(cell))(el, edit(cell)))
    }

    def renderNotFound(failureReason: String): ReactElement =
      <.div(
        <.h2("ERROR"),
        <.h5(failureReason))

    val emptyRow: ReactElement = <.span

    def render(p: DynamicProps): ReactElement = {
      Px.refresh(pxExtPubid, pxUpstreamFD)
      pxData.value() match {
        case \/-(data)                                => renderDetail(p, data)
        case -\/(PubidQueryError.InvalidReqType)      => renderNotFound(s"${UiText.FieldNames.reqType} ${p.extPubid.mnemonic.value} not found.")
        case -\/(PubidQueryError.InvalidPos(rt, len)) => renderNotFound(s"${PlainText pubid p.extPubid} not found.")
      }
    }

    def rows(project: Project, req: Req): Vector[Row] = {
      val fields = project.config.fields.fieldsForReqType(req.reqTypeId)
      fields.foldLeft(Row.head)(_ ++ Row.fromField(_))
    }

    def focus: Callback = Callback.empty // TODO

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    def renderDetail(props: DynamicProps, data: Data): ReactElement = {
      import data.{project, req, pubidText}

      val pw          = pxProjectWidgets.value()
      val state       = props.reqProps(req.id)
      val fieldName   = pxFieldNameFn.value()
      val editFeature = createEditFeature(state.initEditor, state.asyncFeature, data)

      def renderAsyncEditorOrValue(cell: Cell, view: => TagMod): TagMod = {
        def startEdit = editFeature(cell).startEdit(focus)
        TagMod(
          ^.onDblClick -->? startEdit,
          state.async(cell) renderOr (state.edit(cell) renderOr view))
      }

      def renderHeader: ReactElement =
        <.div(
          *.header,
          <.div(
            *.headerId,
            pubidText + ": "),
          <.div(
            *.headerTitle,
              renderAsyncEditorOrValue(Cell.Title, pw.reqTitle(req))))

      def renderRows =
        <.table(
          *.mainTable,
          <.tbody(
            rows(project, req).iterator.map(renderRow)))

      def renderRow(row: Row): ReactElement =
        <.tr(
          ^.key := row.key,
          <.th(
            *.rowTitle,
            renderRowTitle(row)),
          <.td(
            *.rowValue,
            renderRowData(row)))

      def renderRowTitle(row: Row): ReactNode =
        row match {
          case Row.CustomField(f) => fieldName(f)
          case Row.Code           => UiText.FieldNames.reqCodes
          case Row.ReqType        => UiText.FieldNames.reqType
          case Row.Tags           => UiText.FieldNames.tags
          case Row.Implications   => UiText.FieldNames.implications
          case Row.UseCaseStepsN  => UiText.FieldNames.useCaseStepTreeN
          case Row.UseCaseStepsA  => UiText.FieldNames.useCaseStepTreeA
          case Row.UseCaseStepsE  => UiText.FieldNames.useCaseStepTreeE
        }

      def renderImpCell(cell: Cell, pubids: => Vector[Pubid]) =
        renderAsyncEditorOrValue(
          cell,
          pw.implicationList(pubids))

      // TODO Much much overlap with Table.CellProps
      // TODO Test that this applies applicability
      // TODO Test can't edit dead req
      def renderRowData(row: Row): TagMod =
        row match {

          case Row.CustomField(f: CustomField.Text) =>
            renderAsyncEditorOrValue(
              Cell.CustomField(f.id),
              pw.customTextField(f.id)(req).fold(emptyRow)(w => w))

          case Row.Code =>
            renderAsyncEditorOrValue(
              Cell.Code,
              pw.flatReqCodes(data.codes))

          case Row.ReqType =>
            renderAsyncEditorOrValue(
              Cell.ReqType,
              pw.reqTypeFull(req.reqTypeId)) // ---- Note for refactoring: reqTypeFull differs from how ReqTable does it

          case Row.Tags =>
            renderAsyncEditorOrValue(
              Cell.Tags,
              pw.tagList(data.generalTags))

          case Row.CustomField(f: CustomField.Tag) =>
            renderAsyncEditorOrValue(
              Cell.CustomField(f.id),
              pw.tagList(data.customTags(f.id)))

          case Row.Implications =>
            def one(cell: Cell) =
              renderImpCell(cell, data.generalImps(cell.implicationDirection))
            <.div(
              *.generalImpsCont,
              <.div(
                *.generalImpsSide,
                one(Cell.ImplicationSrc)),
              <.div(
                *.generalImpsMiddle,
                s"→ $pubidText →"),
              <.div(
                *.generalImpsSide,
                one(Cell.ImplicationTgt)))

          case Row.CustomField(f: CustomField.Implication) =>
            renderImpCell(Cell.CustomField(f.id), data.customImps(f))

          case Row.UseCaseStepsN => renderStepTree(data.useCaseData.get.stepsN)
          case Row.UseCaseStepsA => renderStepTree(data.useCaseData.get.stepsA)
          case Row.UseCaseStepsE => renderStepTree(data.useCaseData.get.stepsE)

            /*
            val uc = data.req match {
              case x: UseCase => x
              case x => sys error s"Not a use case: $x"
            }
            // TODO UseCaseSteps isn't needed it seems
            val tree = f.useCaseStepTree.get(uc)
            val rows =
              tree.locAndValueIterator((loc, step) => UseCaseStepEditor.Props(
                             uc.pubid.pos, // pos       : ReqTypePos,
                             loc, // loc       : VectorTree.Location,
                             step, // step      : UseCaseStep,
                             data.project.reqs.useCases.stepFlow, // flow      : UseCases.StepFlow,
                             ReusableFn(_.toString), // stepLabel : UseCaseStepId ~=> String,
                             f, // field     : StaticField.UseCaseStepTree,
                             pw, // widgets   : ProjectWidgets,
                             None, // editState : ContentEditorFeature.D0.State,
                             None, // asyncState: AsyncActionFeature.D0.State[String],
                             Callback.empty, // startEdit : Callback,
                             _ => Callback.empty) // update    : UpdateContentCmd.ForUseCaseStep => Callback
              .render)
            rows.toReactNodeArray
            */
        }

      // TODO Move
      def renderStepTree(temp: Temp) = {
        import shipreq.webapp.client.app.Style.reqdetail.{useCaseStep => *}
        import uce._
        val uc = data.useCaseData.get.uc
        val pos = uc.pubid.pos
        val flow = data.project.reqs.useCases.stepFlow

        var first = temp.defaultFirst.nonEmpty
        val x = temp.tree.subtreeLocAndValueIterator(temp.filter, (loc, step) => {

          def header =
            <.div(
              *.header(loc.length - 1),
              temp.field.stepLabel(pos, loc, mnemonicPrefix = false) + ".")

          def body = {

            // TODO Not like this
            val d = if (first) {
              first = false
              temp.defaultFirst
            } else
              Vector.empty

            val p =
            StepText.Props(step,
                           d,
                           flow,
                           pw,
                           None,           // TODO editState : ContentEditorFeature.D0.State,
                           None,           // TODO asyncState: AsyncActionFeature.D0.State[String],
                           Callback.empty) // TODO startEdit : Callback) {
            p.render
          }

          def ctrls = {
            import temp.{mdt, field => f}
            val onAction: Controls.OnAction = _ => Callback.empty    // TODO
            val p = Controls.Props(delete     = f.canDelete(loc),
                                   shiftLeft  = f.canShiftLeft(loc),
                                   leftIsDown = false, // TODO Boolean
                                   shiftRight = f.canShiftRight(loc, mdt),
                                   add        = f.canAdd(loc),
                                   onAction   = onAction)
            p.render
          }

          <.div(*.container,
            header,
            body,
            ctrls)

        }).toReactNodeArray

        def runAction(cmd: UpdateContentCmd): Callback =
          // TODO UseCaseStep buttons should use AsyncFeature
          updateIO(cmd, TCB.Success.nop, f => TCB.Failure(Callback.alert(f)))

        if (temp.tailStep) {
          def cmd = UpdateContentCmd.AddUseCaseStep(uc.id, temp.field, Vector.empty)
          val cb = runAction(cmd)
          val ctrls = Controls.addTailStep(cb).render
          x push <.div(*.container, ctrls)
        }

        x
      }

      <.div(
        renderHeader,
        renderRows)
    }

  } // Backend
}
