package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.{ExternalVar, Px}
import japgolly.scalajs.react.vdom.TagMod
import monocle.{Lens, Optional}
import shipreq.base.util.ScalaExt._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.Event.NESD
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.base.protocol.UpdateContentCmd._
import shipreq.webapp.base.text.{PlainText, Text, TextSearch}
import shipreq.webapp.client.app.ui.ProjectWidgets
import shipreq.webapp.client.lib.TCB
import shipreq.webapp.client.lib.ui.KeyHandlers

trait CellEditors {
  def startEdit(row: Row, col: Column, focus: => Callback): Option[Callback]
}

trait CellEditor {
  def render(row: Row, col: Column): Option[ReactElement]
}

final class CellEditorsImpl[S]($               : CompState.Access[S],
                               editLens        : Lens[S, EditState.Table],
                               asyncLens       : Lens[S, AsyncState.TableState],
                               pxProject       : Px[Project],
                               pxPlainText     : Px[PlainText.ForProject],
                               pxProjectWidgets: Px[ProjectWidgets],
                               pxTextSearch    : Px[TextSearch],
                               saveIO          : CallServer[UpdateContentCmd]) extends CellEditors {

  private val async = AsyncState.Feature($)(asyncLens)

  private val pxApplicability = pxProject.map(Applicability.apply)

  private def areEditPreConditionsSatisfied(row: Row, col: Column): Boolean =
    row.live match {
      case Live => pxApplicability.value().apply(col).choose(row, na = false)(ok = true)
      case Dead => false
    }

  private type CellLens = Lens[S, Option[CellEditor]]

  private trait CellEditorImpl[State] extends CellEditor { this: State =>
    val rowId: Row.SourceId
    val lens: CellLens

    final def abort: Callback =
      $.modState(lens set None)

    def commit(cmd: UpdateContentCmd): Callback =
      async.wrapAsync(rowId, (s, f) => saveIO(cmd, s >> abort, f))

    def commitOrIgnore[A, B](a: A)(filterIgnorable: A => Option[B])(cmd: B => UpdateContentCmd): Callback =
      filterIgnorable(a) match {
        case Some(b) => commit(cmd(b))
        case None    => abort
      }

    def commitAndAbort[A, B](o: Option[A])(commitFn: A => Callback) =
      KeyHandlers.commit(o map commitFn, true) + KeyHandlers.abort(abort)

    val rendered: Some[ReactElement]

    final override def render(row: Row, col: Column) =
      if (areEditPreConditionsSatisfied(row, col))
        rendered
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

  private type StartEdit = S => S

  override def startEdit(row: Row, col: Column, focus: => Callback): Option[Callback] =
    if (areEditPreConditionsSatisfied(row, col))
      startEditWithoutChecks(row, col).map(f => $.modState(f, focus))
    else
      None

  private def startEditWithoutChecks(row: Row, col: Column): Option[StartEdit] = {
    @inline def noEditor = None
    @inline implicit def autoSome(f: StartEdit) = f.some
    row match {

      case r: GenericReqRow =>
        col match {
          case Column.Code           => ReqCodeCellEditor.forReq(r)
          case Column.Title          => RichTextCellEditor.GenericReqTitle(r)
          case Column.Tags           => TagCellEditor(r, None)
          case Column.ReqType        => ReqTypeCellEditor(r)
          case Column.ImplicationSrc => ImpCellEditor(r, col, Row.implicationSrc)
          case Column.ImplicationTgt => ImpCellEditor(r, col, Row.implicationTgt)
          case Column.Pubid
             | Column.DeletionReason => noEditor
          case Column.CustomField(f, _) =>
            f match {
              case id: CustomField.Text       .Id => RichTextCellEditor.CustomTextField(r, id)
              case id: CustomField.Tag        .Id => TagCellEditor(r, id.some)
              case id: CustomField.Implication.Id => ImpCellEditor(r, col, id)
            }
        }

      case r: ReqCodeGroupRow =>
        col match {
          case Column.Code           => ReqCodeCellEditor.forGroup(r)
          case Column.Title          => RichTextCellEditor.ReqCodeGroupTitle(r)
          case Column.Pubid
             | Column.ReqType
             | Column.Tags
             | Column.ImplicationSrc
             | Column.ImplicationTgt
             | Column.DeletionReason
             | _: Column.CustomField => noEditor
        }
    }
  }

  // ===================================================================================================================

  object ReqCodeCellEditor {
    import shipreq.webapp.client.app.ui.newui.ReqCodeEditor

    def forReq(row: GenericReqRow): StartEdit = {
      val id      = row.req.id
      val initial = pxProject.value().reqCodes.activeReqCodesByReqId(id)
      val text    = ReqCodeEditor.Multiple.seqFmt merge initial.toVector.map(PlainText.reqCode).sorted

      val lens: CellLens =
        editLens ^|-> EditState.atCell(row.sourceId, Column.Code)

      val is = new StateMultiple(text, row.sourceId, initial, PatchReqCodes(id, _), lens)
      lens set is.some
    }

    def forGroup(row: ReqCodeGroupRow): StartEdit = {
      val id      = row.reqCodeId
      val initial = row.reqCode
      val text    = PlainText reqCode initial

      val lens: CellLens =
        editLens ^|-> EditState.atCell(row.sourceId, Column.Code)

      val is = new StateSingle(text, row.sourceId, initial, SetReqCodeGroupCode(id, _), lens)
      lens set is.some
    }

    private case class StateSingle(text: String,
                                   rowId: Row.SourceId,
                                   initial: ReqCode.Value,
                                   cmd: ReqCode.Value => UpdateContentCmd,
                                   lens: CellLens) extends CellEditorImpl[StateSingle] {

      def evar = ExternalVar(text)(s => $.modState(lens set copy(text = s).some))

      def tagMod: Option[ReqCode.Value] => TagMod =
        commitAndAbort(_)(commitOrIgnore(_)(ignoreEqual(initial))(cmd))

      def props = ReqCodeEditor.Single.Props(evar, initial.some, pxProject.value().reqCodes.trie, tagMod)

      override val rendered =
        Some(ReqCodeEditor.Single.Component(props))
    }

    private case class StateMultiple(text: String,
                                   rowId: Row.SourceId,
                                   initial: Set[ReqCode.Value],
                                   cmd: NESD[ReqCode.Value] => UpdateContentCmd,
                                   lens: CellLens) extends CellEditorImpl[StateMultiple] {

      def evar = ExternalVar(text)(s => $.modState(lens set copy(text = s).some))

      def tagMod: Option[Set[ReqCode.Value]] => TagMod =
        commitAndAbort(_)(commitOrIgnore(_)(ignoreEmptySetDiff(initial))(cmd))

      def props = ReqCodeEditor.Multiple.Props(evar, initial.some, pxProject.value().reqCodes.trie, tagMod)

      override val rendered =
        Some(ReqCodeEditor.Multiple.Component(props))
    }
  }

  // ===================================================================================================================

  object ReqTypeCellEditor {
    import shipreq.webapp.client.app.ui.newui.ReqTypeSelector
    import ReqTypeSelector.A

    val pxCustomReqTypes = ReqTypeSelector.pxCustomReqTypes(pxProject)

    def apply(row: GenericReqRow): StartEdit = {
      val id = row.req.id
      val initial = pxProject.value().config.reqTypeC(row.req.reqTypeId)
      val pxChoices = ReqTypeSelector.pxChoices(initial, pxCustomReqTypes)

      val lens: CellLens =
        editLens ^|-> EditState.atCell(row.sourceId, Column.ReqType)

      val is = new State(ignoreEqual(initial), initial, pxChoices, t => SetGenericReqType(id, t.id), row.sourceId, lens)
      lens set is.some
    }

    private case class State(ignoreInitial: A => Option[A],
                             edit: A,
                             pxChoices: Px[NonEmptySet[A]],
                             cmd: A => SetGenericReqType,
                             rowId: Row.SourceId,
                             lens: CellLens) extends CellEditorImpl[State] {

      def evar = ExternalVar(edit)(e => $.modState(lens set copy(edit = e).some))

      def commit: Option[TCB.Commit] =
        ignoreInitial(edit).map(a => TCB.Commit(commit(cmd(a))))

      def props = ReqTypeSelector.Props(evar, Some(TCB Abort abort), commit, pxChoices.value())

      override val rendered =
        Some(ReqTypeSelector.Component(props))
    }
  }

  // ===================================================================================================================

  object ImpCellEditor {
    import shipreq.webapp.client.app.ui.newui.ImplicationEditor
    import ImplicationEditor.{Lookup, ValidationFn}

    val pxLookupAll = Px.apply2(pxProject, pxPlainText)(ImplicationEditor.Lookup.all)

    def apply(row: GenericReqRow, col: Column, rowLens: Optional[Row, Vector[Pubid]]): Option[StartEdit] =
      rowLens.getOption(row).map(pubids =>
        startEdit(row, col, pxLookupAll, pubids))

    def apply(row: GenericReqRow, col: Column, fid: CustomField.Implication.Id): Option[StartEdit] = {
      val lookup = Px.apply2(pxProject, pxLookupAll)(ImplicationEditor.Lookup.forCustomColumn(_, _, fid))
      Row.cfImp(fid).getOption(row).map { _ =>
        val pubids = ImplicationEditor.initialValueForCustomColumn(pxProject.value(), fid, row.req.id)
        startEdit(row, col, lookup, pubids)
      }
    }

    private def startEdit(row: GenericReqRow, col: Column, pxLookup: Px[Lookup], pubids: Seq[Pubid]): StartEdit = {
      val subjectId = row.req.id

      val declFwd = ImplicationEditor.isDeclFwd(col)

      val p = pxProject.value()

      val (initialValues, text) = ImplicationEditor.initialValueAndText(
        (subjectId, pubids).some, p, pxLookup.value())

      val valFn =
        pxProject.map(p =>
          ImplicationEditor.validationFn(p, subjectId.some, initialValues, declFwd))

      val cmd: NESD[ReqId] => UpdateContentCmd =
        if (declFwd)
          PatchImplicationTgt(subjectId, _)
        else
          PatchImplicationSrc(subjectId, _)

      val lens: CellLens =
        editLens ^|-> EditState.atCell(row.sourceId, col)

      val is = new State(text, cmd, row.sourceId, pxLookup, valFn, lens)
      lens set is.some
    }

    private case class State(text: String,
                             cmd: NESD[ReqId] => UpdateContentCmd,
                             rowId: Row.SourceId,
                             lookup: Px[Lookup],
                             valFn: Px[ValidationFn],
                             lens: CellLens) extends CellEditorImpl[State] {

      def evar = ExternalVar(text)(s => $.modState(lens set copy(text = s).some))

      def tagMod: Option[SetDiff[ReqId]] => TagMod =
        commitAndAbort(_)(commitOrIgnore(_)(NonEmpty(_))(cmd))

      import Px.AutoValue._

      def props = ImplicationEditor.Props(evar, lookup, valFn, pxTextSearch, tagMod)

      override val rendered =
        Some(ImplicationEditor.Component(props))
    }
  }

  // ===================================================================================================================

  object TagCellEditor {
    import shipreq.webapp.client.app.ui.newui.TagEditor
    import TagEditor.Lookup

    def apply(row: GenericReqRow, fid: Option[CustomField.Tag.Id]): StartEdit = {

      val lookupFn = fid.fold[Project => Lookup](Lookup.notUsedInTagFields)(Lookup.forTagField)

      val p = pxProject.value()
      val lookup = lookupFn(p)
      val id = row.req.id
      val (initial, text) = TagEditor.initialValues(p.reqTags(id), p.config, lookup)

      val lens: CellLens =
        editLens ^|-> EditState.atCell(row.sourceId, fid.fold[Column](Column.Tags)(Column.CustomField(_, Live)))

      val is = new State(text, PatchReqTags(id, _), row.sourceId, initial, lookup, lens)
      lens set is.some
    }

    private case class State(text: String,
                             cmd: NESD[ApplicableTagId] => PatchReqTags,
                             rowId: Row.SourceId,
                             initial: Set[ApplicableTagId],
                             lookup: Lookup, // TODO Should make dynamic
                             lens: CellLens) extends CellEditorImpl[State] {

      def evar = ExternalVar(text)(s => $.modState(lens set copy(text = s).some))

      def tagMod: Option[Stream[ApplicableTag]] => TagMod =
        commitAndAbort(_)(commitOrIgnore(_)(ignoreEmptySetDiff(initial, _.map(_.id).toSet))(cmd))

      def props = TagEditor.Props(evar, lookup, tagMod)

      override val rendered =
        Some(TagEditor.Component(props))
    }
  }

  // ===================================================================================================================

  import shipreq.webapp.client.app.ui.newui.RichTextEditor

  abstract class RichTextCellEditor[T <: Text.Generic](val editor: RichTextEditor[T]) {
    val T: editor.text.type = editor.text

    def startEdit(rowId: Row.SourceId,
                  col: Column,
                  cmd: T.OptionalText => UpdateContentCmd,
                  initialValue: T.OptionalText): StartEdit = {

      def it = pxPlainText.value().format(editor.hardcodedLive, initialValue)

      val lens: CellLens =
        editLens ^|-> EditState.atCell(rowId, col)

      val is = new State(it, rowId, cmd, Some(initialValue), lens)
      lens set is.some
    }

    private case class State(text: String,
                             rowId: Row.SourceId,
                             cmd: T.OptionalText => UpdateContentCmd,
                             initial: Some[T.OptionalText],
                             lens: CellLens) extends CellEditorImpl[State] {

      def evar = ExternalVar(text)(s => $.modState(lens set copy(text = s).some))

      def tagMod: Option[T.OptionalText] => TagMod =
        commitAndAbort(_)(t => commit(cmd(t)))

      def preview = ??? // TODO ///////////////////////////////////////////////////////////////////////////////////

      import Px.AutoValue._

      def props = editor.Props(pxProject, pxPlainText, pxTextSearch, pxProjectWidgets,
        evar, preview, initial, tagMod)

      override val rendered =
        Some(editor.Component(props))
    }
  }

  object RichTextCellEditor {
    object GenericReqTitle extends RichTextCellEditor(RichTextEditor.GenericReqTitle) {
      def apply(r: GenericReqRow): StartEdit =
        startEdit(
          r.sourceId,
          Column.Title,
          SetGenericReqTitle(r.req.id, _),
          r.req.title)
    }

    object ReqCodeGroupTitle extends RichTextCellEditor(RichTextEditor.ReqCodeGroupTitle) {
      def apply(r: ReqCodeGroupRow): StartEdit =
        startEdit(
          r.sourceId,
          Column.Title,
          SetReqCodeGroupTitle(r.reqCodeId, _),
          r.group.title)
    }

    object CustomTextField extends RichTextCellEditor(RichTextEditor.CustomTextField) {
      def apply(r: GenericReqRow, id: CustomField.Text.Id): StartEdit =
        startEdit(
          r.sourceId,
          Column.CustomField(id, Live),
          SetCustomTextField(r.req.id, id, _),
          ReqData.textAt(id, r.req.id).get(pxProject.value().reqText))
    }
  }
}
