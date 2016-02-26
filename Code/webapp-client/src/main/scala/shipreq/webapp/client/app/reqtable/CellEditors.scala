delme package shipreq.webapp.client.app.reqtable

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.TagMod
import monocle.{Lens, Optional}
import shipreq.base.util.ScalaExt._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.Event.NESD
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.base.protocol.UpdateContentCmd._
import shipreq.webapp.base.text.{PlainText, Text, TextSearch}
import shipreq.webapp.client.data.TCB
import shipreq.webapp.client.lib.KeyHandlers
import shipreq.webapp.client.widgets.ProjectWidgets

// =====================================================================================================================
// Interfaces (i.e. the entire point of this file)
// =====================================================================================================================

trait CellEditors {
  def startEdit(row: Row, col: Column, focus: => Callback): Option[Callback]
}

/**
 * This is effectively mutable because of the usage of Pxs and reading of PreviewFeature state.
 */
trait CellEditor {
  def render(row: Row, col: Column): Option[ReactElement]
}

// =====================================================================================================================
// Implementation
// =====================================================================================================================

final class CellEditorsImpl[S]($               : CompState.Access[S],
                               editLens        : Lens[S, EditState.Table],
                               async           : AsyncState.Feature[S],
                               previewFeature  : Preview.Feature[S],
                               pxProject       : Px[Project],
                               pxPlainText     : Px[PlainText.ForProject],
                               pxProjectWidgets: Px[ProjectWidgets],
                               pxTextSearch    : Px[TextSearch],
                               saveIO          : CallServer[UpdateContentCmd]) extends CellEditors {

  private val pxApplicability = pxProject.map(Applicability.apply)

  private def areEditPreConditionsSatisfied(row: Row, col: Column): Boolean =
    row.live match {
      case Live => pxApplicability.value().apply(col).choose(row, na = false)(ok = true)
      case Dead => false
    }

  private type CellLens = Lens[S, Option[CellEditor]]

  private def abort(lens: CellLens): Callback =
    $.modState(lens set None)

  private def commit(lens: CellLens, rowId: Row.SourceId, col: Column, cmd: UpdateContentCmd): Callback =
    async(rowId)(col).wrapAsync((s, f) => saveIO(cmd, s >> abort(lens), f))

  private def commitOrIgnore[A, B](lens: CellLens, rowId: Row.SourceId, col: Column, a: A)(filterIgnorable: A => Option[B])(cmd: B => UpdateContentCmd): Callback =
    filterIgnorable(a) match {
      case Some(b) => commit(lens, rowId, col, cmd(b))
      case None    => abort(lens)
    }

  private def commitAndAbort[A, B](lens: CellLens, singleLine: Boolean, o: Option[A])(commitFn: A => Callback) =
    KeyHandlers.commit(o map commitFn, singleLine) + KeyHandlers.abort(abort(lens))

  private trait CellEditorImpl[State] extends CellEditor { this: State =>
    val renderCB: CallbackTo[Some[ReactElement]]

    protected def renderStatic[A](a: A)(implicit e: A => ReactElement): CallbackTo[Some[ReactElement]] =
      CallbackTo pure Some(e(a))

    protected def renderDynamic[A](a: => A)(implicit e: A => ReactElement): CallbackTo[Some[ReactElement]] =
      CallbackTo(Some(e(a)))

    final override def render(row: Row, col: Column) =
      if (areEditPreConditionsSatisfied(row, col))
        renderCB.runNow()
      else
        None
  }

  private def ignoreEqual[A: UnivEq](initial: A): A => Option[A] =
    value =>
      if (value ==* initial)
        None
      else
        value.some

  private def ignoreEmptySetDiff[A: UnivEq](initial: Set[A]): Set[A] => Option[NESD[A]] =
    value =>
      NonEmpty(SetDiff.compare(before = initial, after = value))

  private def ignoreEmptySetDiff[A, B: UnivEq](initial: Set[B], f: A => Set[B]): A => Option[NESD[B]] =
    ignoreEmptySetDiff(initial) compose f

  private type StartEditFn = S => S

  def startEditFn(lens: CellLens, init: CellEditor): StartEditFn =
    lens set init.some

  private def rvarToCellEditor[A: Reusability, B <: CellEditor](lens: CellLens, f: ReusableVar[A] => B): A => B = {
    lazy val update: A ~=> Callback =
      ReusableFn(a => $.modState(lens set f(ReusableVar(a)(update)).some))
    a => f(ReusableVar(a)(update))
  }

  private def rvarStrToStartEditFn[B <: CellEditor](lens: CellLens, f: ReusableVar[String] => B, initial: String): StartEditFn =
    startEditFn(lens, rvarToCellEditor(lens, f) apply initial)

  override def startEdit(row: Row, col: Column, focus: => Callback): Option[Callback] =
    if (areEditPreConditionsSatisfied(row, col))
      startEditWithoutChecks(row, col).map(f => $.modState(f, focus))
    else
      None

  private def startEditWithoutChecks(row: Row, col: Column): Option[StartEditFn] = {
    @inline def noEditor = None
    @inline implicit def autoSome(f: StartEditFn) = f.some
    row match {

      case r: ReqRow =>
        col match {
          case Column.Code                                           => ForReqCodes.forReq(r)
          case Column.Title                                          => ForRichText.reqTitle(r)
          case Column.Tags                                           => ForTags(r, None)
          case Column.ReqType                                        => ForReqType(r)
          case Column.ImplicationSrc                                 => ForImplications(r, col, Row.implicationSrc)
          case Column.ImplicationTgt                                 => ForImplications(r, col, Row.implicationTgt)
          case Column.Pubid
             | Column.DeletionReason                                 => noEditor
          case Column.CustomField(id: CustomField.Text       .Id, _) => ForRichText.CustomTextField(r, id)
          case Column.CustomField(id: CustomField.Tag        .Id, _) => ForTags(r, id.some)
          case Column.CustomField(id: CustomField.Implication.Id, _) => ForImplications(r, col, id)
        }

      case r: ReqCodeGroupRow =>
        col match {
          case Column.Code              => ForReqCodes.forGroup(r)
          case Column.Title             => ForRichText.ReqCodeGroupTitle(r)
          case Column.Pubid
             | Column.ReqType
             | Column.Tags
             | Column.ImplicationSrc
             | Column.ImplicationTgt
             | Column.DeletionReason
             | Column.CustomField(_, _) => noEditor
        }
    }
  }

  // ===================================================================================================================
  object ForReqCodes {
    import shipreq.webapp.client.widgets.ReqCodeEditor

    def trie() = pxProject.value().reqCodes.trie

    @inline def col = Column.Code

    def forReq(row: ReqRow): StartEditFn = {
      val rowId         = row.sourceId
      val id            = row.req.id
      val lens          = editLens ^|-> EditState.atCell(rowId, col)
      val initialValues = pxProject.value().reqCodes.activeReqCodesByReqId(id)
      val initialText   = ReqCodeEditor.Multiple.seqFmt merge initialValues.toVector.map(PlainText.reqCode).sorted

      val extra: ReqCodeEditor.Multiple.Extra =
        ReusableFn(
          commitAndAbort(lens, false, _)(
            commitOrIgnore(lens, rowId, col, _)(
              ignoreEmptySetDiff(initialValues))(PatchReqCodes(id, _))))

      rvarStrToStartEditFn(lens, new StateMultiple(_, Some(initialValues), extra), initialText)
    }

    private class StateMultiple(rvar   : ReusableVar[String],
                                initial: Some[Set[ReqCode.Value]],
                                extra  : ReqCodeEditor.Multiple.Extra) extends CellEditorImpl[StateMultiple] {
      def props = ReqCodeEditor.Multiple.Props(rvar, initial, trie(), extra)
      override val renderCB = renderDynamic(props.render)
    }

    def forGroup(row: ReqCodeGroupRow): StartEditFn = {
      val rowId        = row.sourceId
      val id           = row.reqCodeId
      val lens         = editLens ^|-> EditState.atCell(rowId, col)
      val initialValue = row.reqCode
      val initialText  = PlainText reqCode initialValue

      val extra: ReqCodeEditor.Single.Extra =
        ReusableFn(
          commitAndAbort(lens, true, _)(
            commitOrIgnore(lens, rowId, col, _)(
              ignoreEqual(initialValue))(SetReqCodeGroupCode(id, _))))

      rvarStrToStartEditFn(lens, new StateSingle(_, Some(initialValue), extra), initialText)
    }

    private class StateSingle(rvar   : ReusableVar[String],
                              initial: Some[ReqCode.Value],
                              extra  : ReqCodeEditor.Single.Extra) extends CellEditorImpl[StateSingle] {
      def props = ReqCodeEditor.Single.Props(rvar, initial, trie(), extra)
      override val renderCB = renderDynamic(props.render)
    }
  }

  // ===================================================================================================================
  object ForReqType {
    import shipreq.webapp.client.widgets.ReqTypeSelector
    import ReqTypeSelector.A

    val pxCustomReqTypes = ReqTypeSelector.pxCustomReqTypes(pxProject)

    @inline def col = Column.ReqType

    def apply(row: ReqRow): Option[StartEditFn] =
      row.req match {
        case r: GenericReq => Some(apply(row.sourceId, r))
        case _: UseCase    => None
      }

    def apply(rowId: Row.ReqRowSourceId, req: GenericReq): StartEditFn = {
      val lens      = editLens ^|-> EditState.atCell(rowId, col)
      val id        = req.id
      val initial   = pxProject.value().config.reqTypeC(req.reqTypeId)
      val pxChoices = ReqTypeSelector.pxChoices(initial, pxCustomReqTypes)

      val is = new State(ignoreEqual(initial), initial, pxChoices, t => SetGenericReqType(id, t.id), lens, rowId)
      startEditFn(lens, is)
    }

    private case class State(ignoreInitial: A => Option[A],
                             edit         : A,
                             pxChoices    : Px[NonEmptySet[A]],
                             cmd          : A => SetGenericReqType,
                             lens         : CellLens,
                             rowId        : Row.SourceId) extends CellEditorImpl[State] {

      def evar = ExternalVar(edit)(e => $.modState(lens set copy(edit = e).some))

      def commitCB: Option[TCB.Commit] =
        ignoreInitial(edit).map(a => TCB.Commit(commit(lens, rowId, col, cmd(a))))

      def props = ReqTypeSelector.Props(evar, Some(TCB Abort abort(lens)), commitCB, pxChoices.value())

      override val renderCB = renderDynamic(ReqTypeSelector.Component(props))
    }
  }

  // ===================================================================================================================

  object ForImplications {
    import shipreq.webapp.client.widgets.ImplicationEditor
    import ImplicationEditor.{Lookup, ValidationFn}

    val pxLookupAll = Px.apply2(pxProject, pxPlainText)(ImplicationEditor.Lookup.all)

    def apply(row: ReqRow, col: Column, rowLens: Optional[Row, Vector[Pubid]]): Option[StartEditFn] =
      rowLens.getOption(row).map(pubids =>
        startEdit(row, col, pxLookupAll, pubids))

    def apply(row: ReqRow, col: Column, fid: CustomField.Implication.Id): Option[StartEditFn] = {
      val lookup = Px.apply2(pxProject, pxLookupAll)(ImplicationEditor.Lookup.forCustomColumn(_, _, fid))
      Row.cfImp(fid).getOption(row).map { _ =>
        val pubids = ImplicationEditor.initialValueForCustomColumn(pxProject.value(), fid, row.req.id)
        startEdit(row, col, lookup, pubids)
      }
    }

    private def startEdit(row: ReqRow, col: Column, pxLookup: Px[Lookup], pubids: Seq[Pubid]): StartEditFn = {
      val rowId     = row.sourceId
      val lens      = editLens ^|-> EditState.atCell(rowId, col)
      val subjectId = row.req.id
      val declFwd   = ImplicationEditor.isDeclFwd(col)

      val (initialValues, initialText) = ImplicationEditor.initialValueAndText(
        (subjectId, pubids).some, pxProject.value(), pxLookup.value())

      val pxValFn =
        pxProject.map(p =>
          ImplicationEditor.validationFn(p, subjectId.some, initialValues, declFwd))

      val cmd: NESD[ReqId] => UpdateContentCmd =
        if (declFwd)
          PatchImplicationTgt(subjectId, _)
        else
          PatchImplicationSrc(subjectId, _)

      val extra: ImplicationEditor.Extra =
        ReusableFn(
          commitAndAbort(lens, true, _)(
            commitOrIgnore(lens, rowId, col, _)(
              NonEmpty(_))(cmd)))

      rvarStrToStartEditFn(lens, new State(_, pxLookup, pxValFn, extra), initialText)
    }

    private class State(rvar  : ReusableVar[String],
                        lookup: Px[Lookup],
                        valFn : Px[ValidationFn],
                        extra : ImplicationEditor.Extra) extends CellEditorImpl[State] {
      def props = ImplicationEditor.Props(rvar, lookup.value(), valFn.value(), pxTextSearch.value(), extra)
      override val renderCB = renderDynamic(props.render)
    }
  }

  // ===================================================================================================================

  object ForTags {
    import shipreq.webapp.client.widgets.TagEditor
    import TagEditor.Lookup

    def apply(row: ReqRow, fid: Option[CustomField.Tag.Id]): StartEditFn = {
      val rowId    = row.sourceId
      val col      = fid.fold[Column](Column.Tags)(Column.CustomField(_, Live))
      val lens     = editLens ^|-> EditState.atCell(rowId, col)
      val id       = row.req.id
      val lookupFn = fid.fold[Project => Lookup](Lookup.notUsedInTagFields)(Lookup.forTagField)
      val pxLookup = pxProject map lookupFn

      val (initialValues, initialText) = {
        val p = pxProject.value()
        TagEditor.initialValues(p.reqTags(id), p.config, pxLookup.value())
      }

      val extra: TagEditor.Extra =
        ReusableFn(
          commitAndAbort(lens, true, _)(
            commitOrIgnore(lens, rowId, col, _)(
              ignoreEmptySetDiff(initialValues, _.map(_.id).toSet))(PatchReqTags(id, _))))

      rvarStrToStartEditFn(lens, new State(_, pxLookup, extra), initialText)
    }

    private class State(rvar  : ReusableVar[String],
                        lookup: Px[Lookup],
                        extra : TagEditor.Extra) extends CellEditorImpl[State] {
      def props = TagEditor.Props(rvar, lookup.value(), extra)
      override val renderCB = renderDynamic(props.render)
    }
  }

  // ===================================================================================================================

  object ForRichText {
    import shipreq.webapp.client.widgets.RichTextEditor

    abstract class Base[T <: Text.Generic](val editor: RichTextEditor[T]) {
      val T: editor.text.type = editor.text

      def startEdit(rowId       : Row.SourceId,
                    col         : Column,
                    cmd         : T.OptionalText => UpdateContentCmd,
                    initialValue: T.OptionalText): StartEditFn = {

        val lens: CellLens =
          editLens ^|-> EditState.atCell(rowId, col)

        val focusId = FocusId.AtCell(rowId, col)

        val extra: editor.Extra =
          ReusableFn(
            commitAndAbort(lens, T.singleLine, _)(t =>
              commit(lens, rowId, col, cmd(t))))

        val initialText: String =
          pxPlainText.value().format(editor.hardcodedLive, initialValue)

        rvarStrToStartEditFn(lens, new State(_, focusId, Some(initialValue), extra), initialText)
      }

      private class State(rvar   : ReusableVar[String],
                          focusId: FocusId.AtCell,
                          initial: Some[T.OptionalText],
                          extra  : editor.Extra) extends CellEditorImpl[State] {

        override val renderCB =
          $.state.map { s =>
            import Px.AutoValue._
            val props = editor.Props(
              pxProject, pxPlainText, pxTextSearch, pxProjectWidgets,
              rvar, previewFeature.forChild(focusId, s), initial, extra)
            Some(props.render: ReactElement)
          }
      }
    }

    def reqTitle(reqRow: ReqRow) =
      reqRow.req match {
        case gr: GenericReq => GenericReqTitle(reqRow.sourceId, gr)
        case uc: UseCase    => UseCaseTitle(reqRow.sourceId, uc)
      }

    object GenericReqTitle extends Base(RichTextEditor.GenericReqTitle) {
      def apply(rowId: Row.ReqRowSourceId, gr: GenericReq): StartEditFn =
        startEdit(
          rowId        = rowId,
          col          = Column.Title,
          cmd          = SetGenericReqTitle(gr.id, _),
          initialValue = gr.title)
    }

    object UseCaseTitle extends Base(RichTextEditor.UseCaseTitle) {
      def apply(rowId: Row.ReqRowSourceId, uc: UseCase): StartEditFn =
        startEdit(
          rowId        = rowId,
          col          = Column.Title,
          cmd          = SetUseCaseTitle(uc.id, _),
          initialValue = uc.title)
    }

    object ReqCodeGroupTitle extends Base(RichTextEditor.ReqCodeGroupTitle) {
      def apply(r: ReqCodeGroupRow): StartEditFn =
        startEdit(
          rowId        = r.sourceId,
          col          = Column.Title,
          cmd          = SetReqCodeGroupTitle(r.reqCodeId, _),
          initialValue = r.group.title)
    }

    object CustomTextField extends Base(RichTextEditor.CustomTextField) {
      def apply(r: ReqRow, id: CustomField.Text.Id): StartEditFn =
        startEdit(
          rowId        = r.sourceId,
          col          = Column.CustomField(id, Live),
          cmd          = SetCustomTextField(r.req.id, id, _),
          initialValue = ReqData.textAt(id, r.req.id).get(pxProject.value().reqText))
    }
  }
}
