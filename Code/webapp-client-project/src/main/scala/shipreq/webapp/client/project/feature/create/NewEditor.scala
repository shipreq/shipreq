package shipreq.webapp.client.project.feature.create

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import scalaz.~>
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text._
import shipreq.webapp.base.feature._
import shipreq.webapp.client.project.widgets.ProjectWidgets
import Feature.{AsyncState, Editor, PreviewId, State}

object NewEditor {

  final case class Static(previewW        : PreviewFeature.Write.Composite[PreviewId],
                          pxProject       : Px[Project],
                          pxPlainText     : Px[PlainText.ForProject],
                          pxProjectWidgets: Px[ProjectWidgets],
                          pxTextSearch    : Px[TextSearch]) {

    private[NewEditor] val internal = new Internal(this)
  }

  final case class Ctx[Value](stateAccess: StateAccessPure[State.ForEditor[Value]]) extends AnyVal

  type ForFields[-FK <: FieldKey] = FieldKey.Fold[FK, ForEditor]

  type ForEditor[Value] = Ctx[Value] ⇒ Editor[Value]

  def forRow(static: Static, rowKey: RowKey): ForFields[rowKey.FieldKey] =
    static.internal.perRow(rowKey)

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  private object Internal {

    /** Initialises an editor.
      *
      * Unlike the `Init` used in `EditorFeature`, here we have no need for initial data because all editors are for
      * new content. Therefore, success is assured, total, and pure.
      */
    type Init[Value] = Editor[Value]

    trait EditorImpl[Value] extends Editor[Value] {
      protected type Props
      protected val props: AsyncState => CallbackTo[Props]
      protected def renderImpl: Props => VdomElement
      protected def valueImpl: Props => Editor.Value[Value]

      final override def render(a: AsyncState)(): VdomElement =
        renderImpl(props(a).runNow())

      final override def value() =
        valueImpl(props(None).runNow())
    }

    final val ShowInstructions = true
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private final class Internal(static: Static) {
    import Internal.ShowInstructions
    import static._

    val perRow: RowKey.Fold[ForFields] = {
      type EditorLogic[Value] = InternalCtx[Value] => Internal.Init[Value]

      val forEditor: EditorLogic ~> ForEditor =
        λ[EditorLogic ~> ForEditor](f => ctx => f(new InternalCtx(ctx)))

      def prepareCG(r: RowKey.CodeGroup.type) = FieldKey.FoldForCodeGroup[EditorLogic](
        _ => EditReqCodes.Single.apply,
        f => EditRichText.CodeGroupTitle(PreviewId(RowKey.CodeGroup, f)))

      def prepareGR(r: RowKey.GenericReq) = FieldKey.FoldForGenericReq[EditorLogic](
        _ => EditReqCodes.Multiple.apply,
        f => EditRichText.CustomTextField(PreviewId(r, f)),
        f => EditImplications(f.scope),
        f => EditTags(f.field),
        f => EditRichText.GenericReqTitle(PreviewId(r, f)))

      def prepareUC(r: RowKey.UseCase.type) = FieldKey.FoldForUseCase[EditorLogic](
        _ => EditReqCodes.Multiple.apply,
        f => EditRichText.CustomTextField(PreviewId(r, f)),
        f => EditImplications(f.scope),
        f => EditTags(f.field),
        f => EditRichText.UseCaseTitle(PreviewId(r, f)))

      RowKey.Fold[ForFields](
        codeGroup    = prepareCG(_).map(forEditor),
        genericReq   = prepareGR(_).map(forEditor),
        useCase      = prepareUC(_).map(forEditor))
    }

    final class InternalCtx[C](val ctx: Ctx[C]) {
      import ctx._

      def startWithStateSnapshot[A: Reusability, E <: Editor[C]](initialValue: A)
                                                                (editor: StateSnapshot[A] => E): E = {
        lazy val update: A ~=> Callback =
          Reusable.fn(b => stateAccess.setState(Some(newEditor(b))))

        def newEditor: A => E =
          a => editor(StateSnapshot.withReuse(a)(update))

        newEditor(initialValue)
      }
    }

    trait ForValueType {
      type Value
      final type EditorImpl = Internal.EditorImpl[Value]
      final type Init       = Internal.Init[Value]
      final type InitFn     = InternalCtx[Value] => Init
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditReqCodes {
      import shipreq.webapp.client.project.widgets.ReqCodeEditor

      val trieCB: CallbackTo[ReqCode.Trie] =
        pxProject.toCallback.map(_.reqCodes.trie)

      object Multiple extends ForValueType {
        import ReqCodeEditor.{Multiple => RCE}

        override type Value = FieldKey.Codes.Value

        def apply: InitFn =
          _.startWithStateSnapshot("")(new State(_))

        private class State(ss: StateSnapshot[String]) extends EditorImpl {
          override type Props = RCE.Props
          override def renderImpl = _.render
          override def valueImpl = _.parseResult
          override val props = as =>
            for {
              trie <- trieCB
            } yield RCE.Props(
              ss,
              None,
              trie,
              EditorStatus.async(as),
              None,
              showInstructions = ShowInstructions)
        }
      }

      object Single extends ForValueType {
        import ReqCodeEditor.{Single => RCE}

        override type Value = FieldKey.Code.Value

        def apply: InitFn =
          _.startWithStateSnapshot("")(new State(_))

        private class State(ss: StateSnapshot[String]) extends EditorImpl {
          override type Props = RCE.Props
          override def renderImpl = _.render
          override def valueImpl = _.parseResult
          override val props = as =>
            for {
              trie <- trieCB
            } yield RCE.Props(
              ss,
              None,
              trie,
              EditorStatus.async(as),
              None,
              showInstructions = ShowInstructions)
        }
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditImplications extends ForValueType {
      import shipreq.webapp.client.project.widgets.ImplicationEditor
      import ImplicationEditor.{Lookup, ValidationFn}

      val pxLookupAll = Px.apply2(pxProject, pxPlainText)(ImplicationEditor.Lookup.all)

      override type Value = FieldKey.Implications#Value

      def apply(scope: ImplicationScope): InitFn =
        scope.fold(customField, all)

      def all(dir: Direction): InitFn =
        start(dir, pxLookupAll)

      def customField(fid: CustomField.Implication.Id): InitFn = {
        val dir = CustomField.Implication.dir
        val lookup = Px.apply2(pxProject, pxLookupAll)(ImplicationEditor.Lookup.forCustomColumn(_, _, fid))
        start(dir, lookup)
      }

      private def start(dir: Direction, pxLookup: Px[Lookup]): InitFn = ictx => {
        import ictx._
        val pxValFn: Px[ValidationFn] = pxProject.map(ImplicationEditor.validationFn(_, None, Set.empty, dir))
        startWithStateSnapshot("")(new State(_, pxLookup, pxValFn))
      }

      private class State(ss: StateSnapshot[String], pxLookup: Px[Lookup], pxValFn: Px[ValidationFn]) extends EditorImpl {
        override type Props = ImplicationEditor.Props
        override def renderImpl = _.render
        override def valueImpl = _.parseResult.map(_.added)
        override val props = as =>
          for {
            lookup     <- pxLookup.toCallback
            valFn      <- pxValFn.toCallback
            textSearch <- pxTextSearch.toCallback
          } yield ImplicationEditor.Props(
            ss,
            lookup,
            valFn,
            EditorStatus.async(as),
            None,
            textSearch,
            showInstructions = ShowInstructions)
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditTags extends ForValueType {
      import shipreq.webapp.client.project.widgets.TagEditor
      import TagEditor.Lookup

      override type Value = FieldKey.Tags#Value

      def apply(fid: Option[CustomField.Tag.Id]): InitFn = ictx => {
        import ictx._
        val lookupFn = fid.fold[Project => Lookup](Lookup.notUsedInTagFields)(Lookup.forTagField)
        val pxLookup = pxProject map lookupFn
        startWithStateSnapshot("")(new State(_, pxLookup))
      }

      private class State(ss: StateSnapshot[String], pxLookup: Px[Lookup]) extends EditorImpl {
        override type Props = TagEditor.Props
        override def renderImpl = _.render
        override def valueImpl = _.parseResultSet
        override val props = as =>
          for {
            lookup <- pxLookup.toCallback
          } yield TagEditor.Props(
            None,
            ss,
            lookup,
            EditorStatus.async(as),
            None,
            showInstructions = ShowInstructions)
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditRichText {
      import shipreq.webapp.base.text._
      import shipreq.webapp.client.project.widgets.RichTextEditor

      abstract class Base[T <: Text.Generic](val editor: RichTextEditor[T]) extends ForValueType {
        val T: editor.text.type = editor.text

        override type Value = T.OptionalText

        def apply(pid: PreviewId): InitFn =
          _.startWithStateSnapshot("")(new State(_, pid))

        private class State(ss: StateSnapshot[String], pid: PreviewId) extends EditorImpl {
          override type Props = editor.Props
          override def renderImpl = _.render
          override def valueImpl = _.parseResult
          override val props = as =>
            for {
              previewRW      <- previewW.toReadWriteCB
              project        <- pxProject.toCallback
              plainText      <- pxPlainText.toCallback
              textSearch     <- pxTextSearch.toCallback
              projectWidgets <- pxProjectWidgets.toCallback
            } yield editor.Props(
              project,
              plainText,
              textSearch,
              projectWidgets,
              ss,
              EditorStatus.async(as),
              None,
              previewRW(pid),
              None,
              showInstructions = ShowInstructions)
        }
      }

      object CodeGroupTitle  extends Base(RichTextEditor.CodeGroupTitle)
      object CustomTextField extends Base(RichTextEditor.CustomTextField)
      object GenericReqTitle extends Base(RichTextEditor.GenericReqTitle)
      object UseCaseTitle    extends Base(RichTextEditor.UseCaseTitle)
    }

  }
}
