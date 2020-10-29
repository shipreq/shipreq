package shipreq.webapp.client.project.feature.create

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import scala.reflect.ClassTag
import scalaz.~~>
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature._
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.text._
import shipreq.webapp.base.util.LastValueMemo
import shipreq.webapp.client.project.feature.create.Feature.{AsyncState, Editor, PreviewId, State}

object NewEditor {

  final case class Ctx[Args, Value](stateAccess: StateAccessPure[State.ForEditor[Args, Value]]) extends AnyVal

  type ForFields[-FK <: FieldKey] = FieldKey.FoldBase[FK, ForEditor]

  type ForEditor[Args, Value] = Ctx[Args, Value] => Editor[Args, Value]

  def forRow(rowKey: RowKey): ForFields[rowKey.FieldKey] =
    Internal.perRow(rowKey)

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  private object Internal {

    /** Initialises an editor.
      *
      * Unlike the `Init` used in `EditorFeature`, here we have no need for initial data because all editors are for
      * new content. Therefore, success is assured, total, and pure.
      */
    type Init[Args, Value] = Editor[Args, Value]

    trait EditorImpl[Args, Value] extends Editor[Args, Value] {
      protected type Props
      protected val props: (Args, AsyncState) => Props
      protected def renderImpl: Props => VdomElement
      protected def valueImpl: Props => Editor.Value[Value]

      final override def render(as: AsyncState, args: Args): VdomElement =
        renderImpl(props(args, as))

      final override def value(args: Args) =
        valueImpl(props(args, None))
    }

    final val ShowInstructions = true

    val editorStyle = EditControlsFeature.Style(
      PreviewFeature.Position.Under,
      EditControlsFeature.OpenPreview.WhenWanted,
      EditControlsFeature.WhenInTransit.DisableEditor, // else buttons move up & down jarringly
    )

    val perRow: RowKey.Fold[ForFields] = {
      type LogicPerField[Args, Value] = InternalCtx[Args, Value] => Internal.Init[Args, Value]

      val logicToPerField: LogicPerField ~~> ForEditor =
        new (LogicPerField ~~> ForEditor) {
          override def apply[A, V](init: LogicPerField[A, V]): ForEditor[A, V] =
            ctx => init(new InternalCtx[A, V](ctx))
        }

      def prepareCG(@nowarn("cat=unused") r: RowKey.CodeGroup.type) = FieldKey.FoldForCodeGroup[LogicPerField](
        _ => EditReqCodes.Single.apply,
        f => EditRichText.CodeGroupTitle(PreviewId(RowKey.CodeGroup, f), None))

      def prepareGR(r: RowKey.GenericReq) = FieldKey.FoldForGenericReq[LogicPerField](
        codes           = _ => EditReqCodes.Multiple.apply,
        customTextField = f => EditRichText.CustomTextField(PreviewId(r, f), Some(r.reqTypeId)),
        implications    = f => EditImplications(f.scope),
        otherTags       = _ => EditTags.otherTags(r.reqTypeId),
        allTags         = _ => EditTags.allTags(r.reqTypeId),
        customFieldTags = f => EditTags.customField(r.reqTypeId, f.field),
        title           = f => EditRichText.GenericReqTitle(PreviewId(r, f), Some(r.reqTypeId)))

      def prepareUC(r: RowKey.UseCase.type) = FieldKey.FoldForUseCase[LogicPerField](
        codes           = _ => EditReqCodes.Multiple.apply,
        customTextField = f => EditRichText.CustomTextField(PreviewId(r, f), Some(StaticReqType.UseCase)),
        implications    = f => EditImplications(f.scope),
        otherTags       = _ => EditTags.otherTags(r.reqTypeId),
        allTags         = _ => EditTags.allTags(r.reqTypeId),
        customFieldTags = f => EditTags.customField(r.reqTypeId, f.field),
        title           = f => EditRichText.UseCaseTitle(PreviewId(r, f), Some(StaticReqType.UseCase)))

      def prepareMI(r: RowKey.ManualIssue.type) = FieldKey.FoldForManualIssue[LogicPerField](
        f => EditRichTextNonEmpty.ManualIssue(PreviewId(r, f), None))

      RowKey.Fold[ForFields](
        codeGroup   = prepareCG(_).map(logicToPerField),
        genericReq  = prepareGR(_).map(logicToPerField),
        useCase     = prepareUC(_).map(logicToPerField),
        manualIssue = prepareMI(_).map(logicToPerField),
      )
    }

    final class InternalCtx[A, V](val ctx: Ctx[A, V]) {
      import ctx._

      def startWithStateSnapshot[B: Reusability, E <: Editor[A, V]](initialValue: B)
                                                                   (editor: StateSnapshot[B] => E): E = {
        lazy val update: Reusable[SetStateFnPure[B]] =
          Reusable.byRef(stateAccess.toSetStateFn.contramap(b => Some(newEditor(b))))

        def newEditor: B => E =
          a => editor(StateSnapshot.withReuse(a)(update))

        newEditor(initialValue)
      }
    }

    trait ForValueType {
      type Args
      type Value
      final type EditorImpl = Internal.EditorImpl[Args, Value]
      final type Init       = Internal.Init[Args, Value]
      final type InitFn     = InternalCtx[Args, Value] => Init
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditReqCodes {
      import shipreq.webapp.client.project.widgets.editors_with_controls.ReqCodeEditor

      object Multiple extends ForValueType {
        import ReqCodeEditor.{Multiple => RCE}

        override type Args = EditorArgs.ForReqCodeEditor
        override type Value = FieldKey.Codes.Value

        def apply: InitFn =
          _.startWithStateSnapshot("")(new EditorAndState(_))

        private class EditorAndState(ss: StateSnapshot[String]) extends EditorImpl {
          override type Props = RCE.Props
          override def renderImpl = _.render
          override def valueImpl = _.parseResult
          override val props = (args, asyncState) =>
            RCE.Props(
              edit             = ss,
              initialValue     = None,
              trie             = args.trie,
              asyncStatus      = EditorStatus.async(asyncState),
              abort            = args.abort,
              abortVerb        = args.abortVerb,
              autoFocus        = args.autoFocus,
              commitFn         = args.commitFn,
              commitVerb       = args.commitVerb,
              extraControls    = args.extraControls,
              showInstructions = ShowInstructions)

          override type State              = String
          override val stateType           = implicitly[ClassTag[String]]
          override val state               = ss.value
          override def withState(s: State) = new EditorAndState(ss.withValue(s))
        }
      }

      object Single extends ForValueType {
        import ReqCodeEditor.{Single => RCE}

        override type Args = EditorArgs.ForReqCodeEditor
        override type Value = FieldKey.Code.Value

        def apply: InitFn =
          _.startWithStateSnapshot("")(new EditorAndState(_))

        private class EditorAndState(ss: StateSnapshot[String]) extends EditorImpl {
          override type Props = RCE.Props
          override def renderImpl = _.render
          override def valueImpl = _.parseResult
          override val props = (args, asyncState) =>
            RCE.Props(
              edit             = ss,
              initialValue     = None,
              trie             = args.trie,
              asyncStatus      = EditorStatus.async(asyncState),
              abort            = args.abort,
              abortVerb        = args.abortVerb,
              autoFocus        = args.autoFocus,
              commitFn         = args.commitFn,
              commitVerb       = args.commitVerb,
              extraControls    = args.extraControls,
              showInstructions = ShowInstructions)

          override type State              = String
          override val stateType           = implicitly[ClassTag[String]]
          override val state               = ss.value
          override def withState(s: State) = new EditorAndState(ss.withValue(s))
        }
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditImplications extends ForValueType {
      import shipreq.webapp.client.project.widgets.editors_with_controls.ImplicationEditor
      import ImplicationEditor.{Lookup, ValidationFn}

      type LookupFn = (Project, PlainText.ForProject.AnyCtx) => Lookup

      override type Args = EditorArgs.ForImplicationEditor
      override type Value = FieldKey.Implications#Value

      def apply(scope: ImplicationScope): InitFn =
        scope.fold(customField, all)

      def all(dir: Direction): InitFn =
        start(dir, ImplicationEditor.Lookup.all)

      def customField(fid: CustomField.Implication.Id): InitFn = {
        val dir = CustomField.Implication.dir
        val lookup: LookupFn =
          (p, pt) => {
            val all = ImplicationEditor.Lookup.all(p, pt)
            ImplicationEditor.Lookup.forCustomColumn(p, all, fid)
          }
        start(dir, lookup)
      }

      private def start(dir: Direction, lookupFn: LookupFn): InitFn = ictx => {
        import ictx._

        val lookupFnMemo: LookupFn =
          LastValueMemo(lookupFn.tupled).toFn2

        val valFnMemo: Project => ValidationFn =
          LastValueMemo(ImplicationEditor.ValidationFn(_, None, Set.empty, dir))

        startWithStateSnapshot("")(new EditorAndState(_, lookupFnMemo, valFnMemo))
      }

      private class EditorAndState(ss      : StateSnapshot[String],
                                   lookupFn: LookupFn,
                                   valFn   : Project => ValidationFn) extends EditorImpl {
        override type Props = ImplicationEditor.Props
        override def renderImpl = _.render
        override def valueImpl = _.parseResult.map(_.added)
        override val props = (args, asyncState) =>
          ImplicationEditor.Props(
            edit             = ss,
            lookup           = lookupFn(args.project, args.plainText),
            validationFn     = valFn(args.project),
            asyncStatus      = EditorStatus.async(asyncState),
            abort            = args.abort,
            abortVerb        = args.abortVerb,
            autoFocus        = args.autoFocus,
            commitFn         = args.commitFn,
            commitVerb       = args.commitVerb,
            textSearch       = args.textSearch,
            extraControls    = args.extraControls,
            showInstructions = ShowInstructions)

        override type State              = String
        override val stateType           = implicitly[ClassTag[String]]
        override val state               = ss.value
        override def withState(s: State) = new EditorAndState(ss.withValue(s), lookupFn, valFn)
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditTags extends ForValueType {
      import shipreq.webapp.client.project.widgets.editors_with_controls.TagEditor
      import TagEditor.Lookup

      override type Args = EditorArgs.ForTagEditor
      override type Value = Set[ApplicableTagId]

      def allTags(reqTypeId: ReqTypeId): InitFn =
        apply(reqTypeId, Lookup.all)

      def otherTags(reqTypeId: ReqTypeId): InitFn =
        apply(reqTypeId, Lookup.notUsedInTagFields)

      def customField(reqTypeId: ReqTypeId, fid: CustomField.Tag.Id): InitFn =
        apply(reqTypeId, Lookup.forTagField(fid))

      def apply(reqTypeId: ReqTypeId, lookupFn: Project => Lookup): InitFn = ictx => {
        import ictx._

        val lookupFnMemo: Project => Lookup =
          LastValueMemo(lookupFn)

        startWithStateSnapshot("")(new EditorAndState(_, reqTypeId, lookupFnMemo))
      }

      private class EditorAndState(ss       : StateSnapshot[String],
                                   reqTypeId: ReqTypeId,
                                   lookupFn : Project => Lookup) extends EditorImpl {

        override type Props = TagEditor.Props
        override def renderImpl = _.render
        override def valueImpl = _.parseResultSet
        override val props = (args, asyncState) =>
          TagEditor.Props(
            preEditValue     = None,
            naTags           = args.project.config.naTags(reqTypeId),
            edit             = ss,
            lookup           = lookupFn(args.project),
            asyncStatus      = EditorStatus.async(asyncState),
            abort            = args.abort,
            abortVerb        = args.abortVerb,
            autoFocus        = args.autoFocus,
            commitFn         = args.commitFn,
            commitVerb       = args.commitVerb,
            extraControls    = args.extraControls,
            showInstructions = ShowInstructions)

        override type State              = String
        override val stateType           = implicitly[ClassTag[String]]
        override val state               = ss.value
        override def withState(s: State) = new EditorAndState(ss.withValue(s), reqTypeId, lookupFn)
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditRichText {
      import shipreq.webapp.base.text._
      import shipreq.webapp.client.project.widgets.editors_with_controls.RichTextEditor

      abstract class Base[T <: Text.Generic](val editor: RichTextEditor[T]) extends ForValueType {
        val T: editor.text.type = editor.text

        override type Args = EditorArgs.ForTextEditor
        override type Value = T.OptionalText

        def apply(pid: PreviewId, reqTypeId: Option[ReqTypeId]): InitFn =
          _.startWithStateSnapshot("")(new EditorAndState(_, pid, reqTypeId))

        private class EditorAndState(ss       : StateSnapshot[String],
                                     pid      : PreviewId,
                                     reqTypeId: Option[ReqTypeId]) extends EditorImpl {

          override type Props = editor.Optional
          override def renderImpl = _.render
          override def valueImpl = _.parseResult
          override val props = (args, asyncState) =>
            editor.Optional(
              project            = args.project,
              naTags             = args.project.config.naTags(reqTypeId),
              plainTextNoCtx     = args.projectWidgets.plainText,
              textSearch         = args.textSearch,
              projectWidgets     = args.projectWidgets,
              edit               = ss,
              asyncStatus        = EditorStatus.async(asyncState),
              abort              = args.abort,
              abortVerb          = args.abortVerb,
              abortConfirmation  = None,
              autoFocus          = args.autoFocus,
              commitFn           = args.commitFn,
              commitVerb         = args.commitVerb,
              editorStyle        = editorStyle,
              preview            = args.previewRW(pid),
              preEditValue       = None,
              extraControls      = args.extraControls,
              showInstructions   = ShowInstructions,
              optionalFullscreen = None)

          override type State              = String
          override val stateType           = implicitly[ClassTag[String]]
          override val state               = ss.value
          override def withState(s: State) = new EditorAndState(ss.withValue(s), pid, reqTypeId)
        }
      }

      object CodeGroupTitle  extends Base(RichTextEditor.CodeGroupTitle)
      object CustomTextField extends Base(RichTextEditor.CustomTextField)
      object GenericReqTitle extends Base(RichTextEditor.GenericReqTitle)
      object UseCaseTitle    extends Base(RichTextEditor.UseCaseTitle)
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditRichTextNonEmpty {
      import shipreq.webapp.base.text._
      import shipreq.webapp.client.project.widgets.editors_with_controls.RichTextEditor

      abstract class Base[T <: Text.Generic](val editor: RichTextEditor[T]) extends ForValueType {
        val T: editor.text.type = editor.text

        override type Args = EditorArgs.ForTextEditor
        override type Value = T.NonEmptyText

        def apply(pid: PreviewId, reqTypeId: Option[ReqTypeId]): InitFn =
          _.startWithStateSnapshot("")(new EditorAndState(_, pid, reqTypeId))

        private class EditorAndState(ss       : StateSnapshot[String],
                                     pid      : PreviewId,
                                     reqTypeId: Option[ReqTypeId]) extends EditorImpl {

          override type Props = editor.NonEmpty
          override def renderImpl = _.render
          override def valueImpl = _.parseResult
          override val props = (args, asyncState) =>
            editor.NonEmpty(
              project            = args.project,
              naTags             = args.project.config.naTags(reqTypeId),
              plainTextNoCtx     = args.projectWidgets.plainText,
              textSearch         = args.textSearch,
              projectWidgets     = args.projectWidgets,
              edit               = ss,
              asyncStatus        = EditorStatus.async(asyncState),
              abort              = args.abort,
              abortVerb          = args.abortVerb,
              abortConfirmation  = None,
              autoFocus          = args.autoFocus,
              commitFn           = args.commitFn,
              commitVerb         = args.commitVerb,
              editorStyle        = editorStyle,
              preview            = args.previewRW(pid),
              preEditValue       = None,
              extraControls      = args.extraControls,
              showInstructions   = ShowInstructions,
              optionalFullscreen = None)

          override type State              = String
          override val stateType           = implicitly[ClassTag[String]]
          override val state               = ss.value
          override def withState(s: State) = new EditorAndState(ss.withValue(s), pid, reqTypeId)
        }
      }

      object ManualIssue extends Base(RichTextEditor.ManualIssue)
    }

  }
}
