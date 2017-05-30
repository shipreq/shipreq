package shipreq.webapp.client.project.feature.editor

import japgolly.microlibs.nonempty._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import scalaz.~>
import shipreq.base.util._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.base.text._
import shipreq.webapp.client.base.feature._
import shipreq.webapp.client.base.lib.AbortCommit
import shipreq.webapp.client.project.feature.PreviewFeature
import shipreq.webapp.client.project.protocol.ServerCall
import shipreq.webapp.client.project.widgets.ProjectWidgets
import Feature.{AsyncError, AsyncState, Editor, PreviewId, State}
import shipreq.webapp.base.event.UseCaseStepGD

/** Interface to start a new editor (if possible).
  * If not all required data is available then the execution of this Callback could result in a no-op.
  *
  * The input to [[create]] is a Callback to invoke after the editor opens.
  *
  * Doesn't perform ANY applicability checks. That's performed by the higher-level Feature API.
  */
final case class NewEditor(create: Callback => Callback) extends AnyVal

object NewEditor {

  final case class Static(previewFeature  : PreviewFeature.Write.Composite[PreviewId],
                          pxProject       : Px[Project],
                          pxPlainText     : Px[PlainText.ForProject],
                          pxProjectWidgets: Px[ProjectWidgets],
                          pxTextSearch    : Px[TextSearch],
                          saveIO          : ServerCall[UpdateContentCmd]) {

    private[NewEditor] val internal = new Internal(this)
  }

  final case class Ctx[Change](stateAccess : StateAccessPure[State.ForEditor[Change]],
                               asyncFeature: AsyncFeature.Write.D0[AsyncError])

  type ForFields[FK <: FieldKey] = FieldKey.Fold[FK, ForEditor]

  type ForEditor[Change] = Ctx[Change] ⇒ NewEditor

  def forRow(static: Static, rowKey: RowKey): ForFields[rowKey.FieldKey] =
    static.internal.perRow(rowKey)

  def doNothing: NewEditor =
    NewEditor(_ => Callback.empty)

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  private object Internal {

    /** Initialises an editor.
      *
      * `Callback` because the initial value may depend on project context (accessed via `Px`).
      *
      * `CallbackOption` because the initial data query might fail. In practice this would probably never happen but
      *   1. I can't prove it won't (id lookup is partial)
      *   2. This might become likely when collaborative features are edited.
      *      (eg. Alice renders start-edit button, Bob deletes req, Alice attempts to start editor)
      */
    type Init[Change] = CallbackOption[Editor[Change]]

    trait EditorImpl[Change] extends Editor[Change] {
      protected type Props
      protected val props: AsyncState => CallbackTo[Props]
      protected def renderImpl: Props => VdomElement
      protected def changeImpl: Props => Editor.Change[Change]

      final override def render(p: Permission, a: AsyncState): Option[VdomElement] =
        // Looks like this could block async but not so. Can't go from edit → async → notAllowed.
        // Unsafety is allowed here because EditorInstance is never Reusable
        p match {
          case Allow => Some(renderImpl(props(a).runNow()))
          case Deny  => None
        }

      final override def change() =
        changeImpl(props(None).runNow())
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private final class Internal(static: Static) {
    import static._

    val perRow: RowKey.Fold[ForFields] = {
      type LogicPerField[Change] = InternalCtx[Change] => Internal.Init[Change]

      val logicToPerField: LogicPerField ~> ForEditor =
        λ[LogicPerField ~> ForEditor] { init =>ctx =>
          val ictx = new InternalCtx(ctx)
          ictx.newEditor(init(ictx))
        }

      def prepareCG(r: RowKey.CodeGroup) = FieldKey.FoldForCodeGroup[LogicPerField](
        _ => EditReqCodes.Single(r.id),
        f => EditRichText.CodeGroupTitle(r.id, PreviewId(r, f)))

      def prepareGR(r: RowKey.GenericReq) = FieldKey.FoldForGenericReq[LogicPerField](
        _ => EditReqCodes.Multiple(r.id),
        f => EditRichText.CustomTextField(r.id, f.field, PreviewId(r, f)),
        f => EditImplications(r.id, f.scope),
        _ => EditReqType(r.id),
        f => EditTags(r.id, f.field),
        f => EditRichText.GenericReqTitle(r.id, PreviewId(r, f)))

      def prepareUC(r: RowKey.UseCase) = FieldKey.FoldForUseCase[LogicPerField](
        _ => EditReqCodes.Multiple(r.id),
        f => EditRichText.CustomTextField(r.id, f.field, PreviewId(r, f)),
        f => EditImplications(r.id, f.scope),
        f => EditTags(r.id, f.field),
        f => EditRichText.UseCaseTitle(r.id, PreviewId(r, f)))

      lazy val forUseCaseSteps = FieldKey.FoldForUseCaseSteps[ForEditor](
        f => logicToPerField(EditUseCaseStep(f.id, PreviewId(RowKey.UseCaseSteps, f))))

      RowKey.Fold[ForFields](
        codeGroup    = prepareCG(_).map(logicToPerField),
        genericReq   = prepareGR(_).map(logicToPerField),
        useCase      = prepareUC(_).map(logicToPerField),
        useCaseSteps = () => forUseCaseSteps)
    }

    final class InternalCtx[C](val ctx: Ctx[C]) {
      import ctx._

      def abort: Callback =
        stateAccess.setState(None)

      def commit(cmd: UpdateContentCmd): Callback =
        asyncFeature((s, f) => saveIO(cmd, s >> abort, f))

      def makeAbortCommit[A](cmd: A => UpdateContentCmd): Some[AbortCommit[Callback, A ~=> Callback]] =
        Some(AbortCommit(abort, Reusable.fn(v => commit(cmd(v)))))

      /** Creates a Callback that when invoked, will initialise and start an editor.
        *
        * @tparam S Initial data. Data captured before starting the editor.
        * @tparam A The initial value of the editor.
        * @tparam E The editor
        */
      def startWithStateSnapshot[S, A: Reusability, E <: Editor[C]](initialData: CallbackOption[S])
                                                                   (initialValue: S => A)
                                                                   (editor: S => StateSnapshot[A] => E): CallbackOption[E] =
        initialData.flatMap { s =>
          val editorCtor = editor(s)

          lazy val update: A ~=> Callback =
            Reusable.fn(b => stateAccess.setState(newEditor(b)))

          def newEditor: A => Some[E] =
            b => Some(editorCtor(StateSnapshot.withReuse(b)(update)))

          CallbackOption.liftOption(newEditor(initialValue(s)))
        }

      def newEditor(init: => Internal.Init[C]): NewEditor =
        NewEditor(cb => init.get.flatMap(stateAccess.setState(_, cb)))
    }

    def getGenericReq(id: GenericReqId): CallbackOption[GenericReq] =
      pxProject.toCallback.map(_.reqs.genericReqs.get(id)).asCBO

    def getUseCase(id: UseCaseId): CallbackOption[UseCase] =
      pxProject.toCallback.map(_.reqs.useCases.imap.get(id)).asCBO

    def getCodeGroup(id: ReqCodeId): CallbackOption[LiveCodeGroup] =
      pxProject.toCallback.map(_.reqCodes.getById(id).flatMap {
        case d: ReqCode.ActiveGroup => d.group.some
        case _: ReqCode.ActiveReq
             | _: ReqCode.Inactive    => None
      }).asCBO

    def getCustomReqTypeCB(id: CustomReqTypeId): CallbackOption[CustomReqType] =
      pxProject.toCallback.map(_.config.reqTypes.custom.get(id)).asCBO

    trait ForChangeType {
      type Change
      final type EditorImpl = Internal.EditorImpl[Change]
      final type Init       = Internal.Init[Change]
      final type InitFn     = InternalCtx[Change] => Init
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditReqType extends ForChangeType {
      import shipreq.webapp.client.project.widgets.ReqTypeSelector
      import ReqTypeSelector.RT

      override type Change = CustomReqType

      val pxCustomReqTypes = ReqTypeSelector.pxCustomReqTypes(pxProject)

      def apply(id: GenericReqId): InitFn = ictx => {
        import ictx._, ctx._

        case class State(initialValue: Some[RT],
                         editValue   : RT,
                         pxChoices   : Px[NonEmptySet[RT]],
                         abortCommit : ReqTypeSelector.AbortCommit) extends EditorImpl {

          def ss = StateSnapshot(editValue)(e => stateAccess.setState(copy(editValue = e).some))

          override type Props = ReqTypeSelector.Props
          override def renderImpl = _.render
          override def changeImpl = _.change
          override val props = as =>
            for {
              choices <- pxChoices.toCallback
            } yield ReqTypeSelector.Props(
              initialValue,
              ss,
              choices,
              EditorStatus.async(as),
              abortCommit)
        }

        val abortCommit: ReqTypeSelector.AbortCommit =
          Some(makeAbortCommit[RT](t => UpdateContentCmd.SetGenericReqType(id, t.id)).value)

        for {
          req     <- getGenericReq(id)
          initial <- getCustomReqTypeCB(req.reqTypeId)
        } yield {
          val pxChoices = ReqTypeSelector.pxChoices(initial, pxCustomReqTypes)
          State(Some(initial), initial, pxChoices, abortCommit)
        }
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditReqCodes {
      import shipreq.webapp.client.project.widgets.ReqCodeEditor

      val trieCB: CallbackTo[ReqCode.Trie] =
        pxProject.toCallback.map(_.reqCodes.trie)

      object Multiple extends ForChangeType {
        import ReqCodeEditor.{Multiple => RCE}

        override type Change = RCE.Output

        def apply(id: ReqId): InitFn = ictx => {
          import ictx._

          val initialValuesCB: CallbackTo[Set[ReqCode.Value]] =
            pxProject.toCallback.map(_.reqCodes.activeReqCodesByReqId(id))

          val abortCommit: ReqCodeEditor.Multiple.AbortCommit =
            makeAbortCommit(UpdateContentCmd.PatchReqCodes(id, _))

          startWithStateSnapshot(
            initialValuesCB.toCBO)(
            ReqCodeEditor.Multiple.seqFmt merge _.toVector.map(PlainText.reqCode).sorted)(
            initialValues => new State(_, Some(initialValues), abortCommit))
        }

        private class State(ss         : StateSnapshot[String],
                            initial    : Some[Set[ReqCode.Value]],
                            abortCommit: RCE.AbortCommit) extends EditorImpl {

          override type Props = RCE.Props
          override def renderImpl = _.render
          override def changeImpl = _.validated
          override val props = as =>
            for {
              trie <- trieCB
            } yield RCE.Props(
              ss,
              initial,
              trie,
              EditorStatus.async(as),
              abortCommit,
              showInstructions = true)
        }
      }

      object Single extends ForChangeType {
        import ReqCodeEditor.{Single => RCE}

        override type Change = RCE.Output

        def apply(id: ReqCodeId): InitFn = ictx => {
          import ictx._

          val initialValueCB: CallbackOption[ReqCode.Value] =
            pxProject.toCallback.map(_.reqCodes.reqCode(id)).toCBO

          val abortCommit: ReqCodeEditor.Single.AbortCommit =
            makeAbortCommit(UpdateContentCmd.SetCodeGroupCode(id, _))

          startWithStateSnapshot(initialValueCB)(PlainText.reqCode)(
            i => new State(_, Some(i), abortCommit))
        }

        private class State(ss         : StateSnapshot[String],
                            initial    : Some[ReqCode.Value],
                            abortCommit: RCE.AbortCommit) extends EditorImpl {

          override type Props = RCE.Props
          override def renderImpl = _.render
          override def changeImpl = _.validated
          override val props = as =>
            for {
              trie <- trieCB
            } yield RCE.Props(
              ss,
              initial,
              trie,
              EditorStatus.async(as),
              abortCommit,
              showInstructions = true)
        }
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditImplications extends ForChangeType {
      import shipreq.webapp.client.project.widgets.ImplicationEditor
      import ImplicationEditor.{Lookup, ValidationFn}

      val pxLookupAll = Px.apply2(pxProject, pxPlainText)(ImplicationEditor.Lookup.all)

      override type Change = ImplicationEditor.Output

      def apply(id: ReqId, scope: ImplicationScope): InitFn =
        scope.fold(customField(id, _), all(id, _))

      def all(id: ReqId, dir: Direction): InitFn =
        start(id, dir, pxLookupAll)

      def customField(id: ReqId, fid: CustomField.Implication.Id): InitFn = {
        val dir = CustomField.Implication.dir
        val lookup = Px.apply2(pxProject, pxLookupAll)(ImplicationEditor.Lookup.forCustomColumn(_, _, fid))
        start(id, dir, lookup)
      }

      private def start(id: ReqId, dir: Direction, pxLookup: Px[Lookup]): InitFn = ictx => {
        import ictx._

        val pxPubids = pxProject.map(p => ImplicationEditor.initialValue(p, dir, id))

        val pxInit: Px[(Set[ReqId], String)] =
          for {
            project <- pxProject
            lookup  <- pxLookup
            pubids  <- pxPubids
          } yield ImplicationEditor.initialValueAndText((id, pubids).some, project, lookup)

        val pxValFn: Px[ValidationFn] =
          for {
            project <- pxProject
            init    <- pxInit
          } yield ImplicationEditor.validationFn(project, id.some, init._1, dir)

        val abortCommit: ImplicationEditor.AbortCommit =
          makeAbortCommit(UpdateContentCmd.PatchImplications(id, dir, _))

        startWithStateSnapshot(pxInit.toCallback.toCBO)(_._2)(
          _ => new State(_, pxLookup, pxValFn, abortCommit))
      }

      private class State(ss         : StateSnapshot[String],
                          pxLookup   : Px[Lookup],
                          pxValFn    : Px[ValidationFn],
                          abortCommit: ImplicationEditor.AbortCommit) extends EditorImpl {

        override type Props = ImplicationEditor.Props
        override def renderImpl = _.render
        override def changeImpl = _.validated
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
            abortCommit,
            textSearch,
            showInstructions = true)
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditTags extends ForChangeType {
      import shipreq.webapp.client.project.widgets.TagEditor
      import TagEditor.Lookup

      override type Change = TagEditor.Output

      def apply(id: ReqId, fid: Option[CustomField.Tag.Id]): InitFn = ictx => {
        import ictx._

        val lookupFn = fid.fold[Project => Lookup](Lookup.notUsedInTagFields)(Lookup.forTagField)
        val pxLookup = pxProject map lookupFn

        val pxInit: Px[(Set[ApplicableTagId], String)] =
          for {
            project <- pxProject
            lookup <- pxLookup
          } yield TagEditor.initialValues(project.reqTags(id), project.config, lookup)

        val abortCommit: TagEditor.AbortCommit =
          makeAbortCommit(UpdateContentCmd.PatchReqTags(id, _))

        startWithStateSnapshot(pxInit.toCallback.toCBO)(_._2)(
          init => new State(_, Some(init._1), pxLookup, abortCommit))
      }

      private class State(ss           : StateSnapshot[String],
                          initialValues: Some[Set[ApplicableTagId]],
                          pxLookup     : Px[Lookup],
                          abortCommit  : TagEditor.AbortCommit) extends EditorImpl {

        override type Props = TagEditor.Props
        override def renderImpl = _.render
        override def changeImpl = _.validated
        override val props = as =>
          for {
            lookup <- pxLookup.toCallback
          } yield TagEditor.Props(
            initialValues,
            ss,
            lookup,
            EditorStatus.async(as),
            abortCommit,
            showInstructions = true)
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditRichText {
      import shipreq.webapp.base.text._
      import shipreq.webapp.client.project.widgets.RichTextEditor

      abstract class Base[T <: Text.Generic](val editor: RichTextEditor[T]) extends ForChangeType {
        val T: editor.text.type = editor.text

        override type Change = T.OptionalText

        protected def start(cmd           : T.OptionalText => UpdateContentCmd,
                            initialValueCB: CallbackOption[T.OptionalText],
                            pid           : PreviewId): InitFn = ictx => {
          import ictx._

          val abortCommit: editor.AbortCommit =
            makeAbortCommit(cmd)

          val initCB =
            for {
              initialValue <- initialValueCB
              plainText    <- pxPlainText.toCallback.toCBO
            } yield {
              val initialText = plainText.format(RichTextEditor.hardcodedLive, initialValue)
              (initialValue, initialText)
            }

          startWithStateSnapshot(initCB)(_._2)(
            i => new State(_, Some(i._1), pid, abortCommit))
        }

        private class State(ss         : StateSnapshot[String],
                            initial    : Some[T.OptionalText],
                            pid        : PreviewId,
                            abortCommit: editor.AbortCommit) extends EditorImpl {

          override type Props = editor.Props
          override def renderImpl = _.render
          override def changeImpl = _.validated
          override val props = as =>
            for {
              previewState   <- previewFeature.stateCB
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
              abortCommit,
              previewFeature(pid, previewState),
              initial,
              showInstructions = true)
        }
      }

      object CodeGroupTitle extends Base(RichTextEditor.CodeGroupTitle) {
        def apply(id: ReqCodeId, pid: PreviewId): InitFn = start(
          UpdateContentCmd.SetCodeGroupTitle(id, _),
          getCodeGroup(id).map(_.title).widen,
          pid)
      }

      object CustomTextField extends Base(RichTextEditor.CustomTextField) {
        def apply(id: ReqId, fid: CustomField.Text.Id, pid: PreviewId): InitFn = start(
          UpdateContentCmd.SetCustomTextField(id, fid, _),
          pxProject.toCallback.map(p => ReqData.textAt(fid, id).get(p.reqText)).toCBO,
          pid)
      }

      object GenericReqTitle extends Base(RichTextEditor.GenericReqTitle) {
        def apply(id: GenericReqId, pid: PreviewId): InitFn = start(
          UpdateContentCmd.SetGenericReqTitle(id, _),
          getGenericReq(id).map(_.title),
          pid)
      }

      object UseCaseTitle extends Base(RichTextEditor.UseCaseTitle) {
        def apply(id: UseCaseId, pid: PreviewId): InitFn = start(
          UpdateContentCmd.SetUseCaseTitle(id, _),
          getUseCase(id).map(_.title),
          pid)
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditUseCaseStep extends ForChangeType {
      import shipreq.webapp.client.project.widgets.RichTextEditor.hardcodedLive
      import shipreq.webapp.client.project.widgets.UseCaseStepEditor
      import UseCaseStepFlowText.TextAndFlow

      override type Change = UseCaseStepGD.NonEmptyValues

      def apply(id: UseCaseStepId, pid: PreviewId): InitFn = ictx => {
        import ictx._

        val commitFn: UseCaseStepEditor.CommitFn =
          Reusable.fn(v => commit(UpdateContentCmd.UpdateUseCaseStep(id, v)))

        val pxStepFocus: Px[UseCaseStep.Focus] =
          pxProject.map(_.reqs.useCases.focusStep(id))

        val pxInit: Px[(UseCaseStepEditor.InitialValue, String)] =
          for {
            stepFocus <- pxStepFocus
            plainText <- pxPlainText
          } yield {
            val initialValue = TextAndFlow(stepFocus.step.titleExplicitly, stepFocus.flow)
            val initialText = plainText.useCaseStep(hardcodedLive, initialValue)
            (initialValue, initialText)
          }

        startWithStateSnapshot(pxInit.toCallback.toCBO)(_._2)(
          i => new State(_, Some(i._1), pid, abort, commitFn))
      }

      private class State(ss     : StateSnapshot[String],
                          initial: Some[UseCaseStepEditor.InitialValue],
                          pid    : PreviewId,
                          abort  : Callback,
                          commit : UseCaseStepEditor.CommitFn) extends EditorImpl {

        override type Props = UseCaseStepEditor.Props
        override def renderImpl = _.render
        override def changeImpl = _.validatedChanges
        override val props = as =>
          for {
            previewState   <- previewFeature.stateCB
            project        <- pxProject.toCallback
            plainText      <- pxPlainText.toCallback
            textSearch     <- pxTextSearch.toCallback
            projectWidgets <- pxProjectWidgets.toCallback
          } yield UseCaseStepEditor.Props(
            project,
            plainText,
            textSearch,
            projectWidgets,
            ss,
            EditorStatus.async(as),
            abort,
            commit,
            previewFeature(pid, previewState),
            initial)
      }
    }

  }
}
