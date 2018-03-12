package shipreq.webapp.client.project.feature.editor

import japgolly.microlibs.nonempty._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import scalaz.~~>
import shipreq.base.util.ScalaExt._
import shipreq.base.util.VectorTree.LocationOps
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.UseCaseStepGD
import shipreq.webapp.base.feature._
import shipreq.webapp.base.lib.KeyboardTheme
import shipreq.webapp.base.protocol.{ServerSideProcInvoker, UpdateContentCmd}
import shipreq.webapp.base.text._
import shipreq.webapp.client.project.widgets.ProjectWidgets
import Feature.{AsyncError, AsyncState, Editor, PreviewId, State}

/** Interface to start a new editor (if possible).
  * If not all required data is available then the execution of this Callback could result in a no-op.
  *
  * The input to [[create]] is a Callback to invoke after the editor opens.
  *
  * Doesn't perform ANY applicability checks. That's performed by the higher-level Feature API.
  */
final case class NewEditor(create: NewEditor.CreationArgs => Callback) extends AnyVal

object NewEditor {

  @Lenses
  final case class CreationArgs(pxProjectWidgets: Reusable[Px[ProjectWidgets.AnyCtx]], hooks: Hooks) {
    val cbProjectWidgets: CallbackTo[ProjectWidgets.AnyCtx] =
      pxProjectWidgets.toCallback
  }

  object CreationArgs {
    val onClose = hooks ^|-> Hooks.onClose
    val onStart = hooks ^|-> Hooks.onStart

    implicit val reusability: Reusability[CreationArgs] =
      Reusability.byRef || Reusability.derive
  }

  @Lenses
  final case class Hooks(onStart: Callback, onClose: Callback)

  object Hooks {
    val empty: Hooks =
      Hooks(Callback.empty, Callback.empty)

    implicit val reusability: Reusability[Hooks] = {
      implicit val x: Reusability[Callback] = Reusability.by((_: Callback).toScalaFn)(Reusability.byRef) // TODO Use Reusability.callbackByRef
      Reusability.byRef || Reusability.derive
    }
  }

  final case class Static(previewW        : PreviewFeature.Write.Composite[PreviewId],
                          pxProject       : Px[Project],
                          pxPlainTextNoCtx: Px[PlainText.ForProject.NoCtx],
                          pxTextSearch    : Px[TextSearch],
                          saveIO          : ServerSideProcInvoker[UpdateContentCmd, ErrorMsg, Any]) {

    private[NewEditor] val internal = new Internal(this)
  }

  final case class Ctx[A, Change](stateAccess : StateAccessPure[State.ForEditor[A, Change]],
                                  asyncFeature: AsyncFeature.Write.D0[AsyncError])

  type ForFields[FK <: FieldKey] = FieldKey.Fold[FK, ForEditor]

  type ForEditor[A, Change] = Ctx[A, Change] ⇒ NewEditor

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
    type Init[FieldArgs, Change] = CreationArgs => CallbackOption[Editor[FieldArgs, Change]]

    trait EditorImpl[Args, Change] extends Editor[Args, Change] {
      protected type Props
      protected val props: (Args, AsyncState) => CallbackTo[Props]
      protected def renderImpl: Props => VdomElement
      protected def changeImpl: Props => Editor.Change[Change]

      final override def render(p: Permission, as: AsyncState, args: Args): Option[VdomElement] =
        // Looks like this could block async but not so. Can't go from edit → async → notAllowed.
        // Unsafety is allowed here because EditorInstance is never Reusable
        p match {
          case Allow => Some(renderImpl(props(args, as).runNow()))
          case Deny  => None
        }

      final override def change(args: Args) =
        changeImpl(props(args, None).runNow())
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private final class Internal(static: Static) {
    import static._

    val perRow: RowKey.Fold[ForFields] = {
      type LogicPerField[Args, Change] = InternalCtx[Args, Change] => Internal.Init[Args, Change]

      val logicToPerField: LogicPerField ~~> ForEditor =
        new (LogicPerField ~~> ForEditor) {
          override def apply[A, C](init: LogicPerField[A, C]): ForEditor[A, C] =
            ctx => {
              val ictx = new InternalCtx[A, C](ctx)
              ictx.newEditor(init(ictx))
            }
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

    final class InternalCtx[A, C](val ctx: Ctx[A, C]) {
      import ctx._

      def abort(hooks: Hooks): Callback =
        stateAccess.setState(None, hooks.onClose)

      def commit(cmd: UpdateContentCmd, hooks: Hooks): Callback =
        asyncFeature((s, f) => saveIO(cmd, _ => s >> abort(hooks), f))

      def makeAbortCommitFn[B](cmd: B => UpdateContentCmd, hooks: Hooks): (Some[Callback], Some[B ~=> Callback]) =
        (Some(abort(hooks)), Some(Reusable.fn(v => commit(cmd(v), hooks))))

      /** Creates a Callback that when invoked, will initialise and start an editor.
        *
        * @tparam S Initial data. Data captured before starting the editor.
        * @tparam B The initial value of the editor.
        * @tparam E The editor
        */
      def startWithStateSnapshot[S, B: Reusability, E <: Editor[A, C]](initialData: CallbackOption[S])
                                                                      (initialValue: S => B)
                                                                      (editor: S => StateSnapshot[B] => E): CallbackOption[E] =
        initialData.flatMap { s =>
          val editorCtor = editor(s)

          lazy val update: Reusable[SetStateFnPure[B]] =
            Reusable.byRef(stateAccess.toSetStateFn.contramap(newEditor))

          def newEditor: B => Some[E] =
            b => Some(editorCtor(StateSnapshot.withReuse(b)(update)))

          CallbackOption.liftOption(newEditor(initialValue(s)))
        }

      def newEditor(init: => Internal.Init[A, C]): NewEditor =
        NewEditor(args => init(args).asCallback.flatMap(stateAccess.setState(_, args.hooks.onStart)))
    }

    def getGenericReq(id: GenericReqId): CallbackOption[GenericReq] =
      pxProject.toCallback.map(_.content.reqs.genericReqs.get(id)).asCBO

    def getUseCase(id: UseCaseId): CallbackOption[UseCase] =
      pxProject.toCallback.map(_.content.reqs.useCases.imap.get(id)).asCBO

    def getCodeGroup(id: ReqCodeId): CallbackOption[LiveCodeGroup] =
      pxProject.toCallback.map(_.content.reqCodes.getById(id).flatMap {
        case d: ReqCode.ActiveGroup => d.group.some
        case _: ReqCode.ActiveReq
             | _: ReqCode.Inactive    => None
      }).asCBO

    def getCustomReqTypeCB(id: CustomReqTypeId): CallbackOption[CustomReqType] =
      pxProject.toCallback.map(_.config.reqTypes.custom.get(id)).asCBO

    def commitVerb = KeyboardTheme.Instructions.defaultCommitVerb

    trait ForChangeType {
      type Args
      type Change
      final type EditorImpl = Internal.EditorImpl[Args, Change]
      final type Init       = Internal.Init[Args, Change]
      final type InitFn     = InternalCtx[Args, Change] => Init
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditReqType extends ForChangeType {
      import shipreq.webapp.client.project.widgets.ReqTypeSelector
      import ReqTypeSelector.RT

      override type Args   = Unit
      override type Change = CustomReqType

      val pxCustomReqTypes = ReqTypeSelector.pxCustomReqTypes(pxProject)

      def apply(id: GenericReqId): InitFn = ictx => args => {
        import ictx._, ctx._

        case class State(initialValue: Some[RT],
                         editValue   : RT,
                         pxChoices   : Px[NonEmptySet[RT]],
                         abort       : Some[Callback],
                         commitFn    : Some[RT ~=> Callback]) extends EditorImpl {

          def ss = StateSnapshot(editValue)(stateAccess.toSetStateFn.contramap(e => copy(editValue = e).some))

          override type Props = ReqTypeSelector.Props
          override def renderImpl = _.render
          override def changeImpl = _.change
          override val props = (_, asyncState) =>
            for {
              choices <- pxChoices.toCallback
            } yield ReqTypeSelector.Props(
              initialValue = initialValue,
              edit         = ss,
              choices      = choices,
              asyncStatus  = EditorStatus.async(asyncState),
              abort        = abort,
              commitFn     = commitFn)
        }

        val (abort, commitFn) =
          makeAbortCommitFn[RT](t => UpdateContentCmd.SetGenericReqType(id, t.id), args.hooks)

        for {
          req     <- getGenericReq(id)
          initial <- getCustomReqTypeCB(req.reqTypeId)
        } yield {
          val pxChoices = ReqTypeSelector.pxChoices(initial, pxCustomReqTypes)
          State(Some(initial), initial, pxChoices, abort, commitFn)
        }
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditReqCodes {
      import shipreq.webapp.client.project.widgets.ReqCodeEditor

      val trieCB: CallbackTo[ReqCode.Trie] =
        pxProject.toCallback.map(_.content.reqCodes.trie)

      object Multiple extends ForChangeType {
        import ReqCodeEditor.{Multiple => RCE}

        override type Args   = Unit
        override type Change = RCE.Output

        def apply(id: ReqId): InitFn = ictx => args => {
          import ictx._

          val initialValuesCB: CallbackTo[Set[ReqCode.Value]] =
            pxProject.toCallback.map(_.content.reqCodes.activeReqCodesByReqId(id))

          val (abort, commitFn) =
            makeAbortCommitFn[RCE.Output](UpdateContentCmd.PatchReqCodes(id, _), args.hooks)

          startWithStateSnapshot(
            initialValuesCB.toCBO)(
            ReqCodeEditor.Multiple.seqFmt merge _.toVector.map(PlainText.reqCode).sorted)(
            initialValues => new State(_, Some(initialValues), abort, commitFn))
        }

        private class State(ss      : StateSnapshot[String],
                            initial : Some[Set[ReqCode.Value]],
                            abort   : Some[Callback],
                            commitFn: Some[RCE.CommitFn]) extends EditorImpl {

          override type Props = RCE.Props
          override def renderImpl = _.render
          override def changeImpl = _.validated
          override val props = (_, asyncState) =>
            for {
              trie <- trieCB
            } yield RCE.Props(
              edit             = ss,
              initialValue     = initial,
              trie             = trie,
              asyncStatus      = EditorStatus.async(asyncState),
              abort            = abort,
              commitFn         = commitFn,
              commitVerb       = commitVerb,
              extraKbShortcuts = KeyboardTheme.Shortcuts.empty,
              showInstructions = true)
        }
      }

      object Single extends ForChangeType {
        import ReqCodeEditor.{Single => RCE}

        override type Args   = Unit
        override type Change = RCE.Output

        def apply(id: ReqCodeId): InitFn = ictx => args => {
          import ictx._

          val initialValueCB: CallbackOption[ReqCode.Value] =
            pxProject.toCallback.map(_.content.reqCodes.reqCode(id)).toCBO

          val (abort, commitFn) =
            makeAbortCommitFn[RCE.Output](UpdateContentCmd.SetCodeGroupCode(id, _), args.hooks)

          startWithStateSnapshot(initialValueCB)(PlainText.reqCode)(
            i => new State(_, Some(i), abort, commitFn))
        }

        private class State(ss      : StateSnapshot[String],
                            initial : Some[ReqCode.Value],
                            abort   : Some[Callback],
                            commitFn: Some[RCE.CommitFn]) extends EditorImpl {

          override type Props = RCE.Props
          override def renderImpl = _.render
          override def changeImpl = _.validated
          override val props = (_, asyncState) =>
            for {
              trie <- trieCB
            } yield RCE.Props(
              edit             = ss,
              initialValue     = initial,
              trie             = trie,
              asyncStatus      = EditorStatus.async(asyncState),
              abort            = abort,
              commitFn         = commitFn,
              commitVerb       = commitVerb,
              extraKbShortcuts = KeyboardTheme.Shortcuts.empty,
              showInstructions = true)
        }
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditImplications extends ForChangeType {
      import shipreq.webapp.client.project.widgets.ImplicationEditor
      import ImplicationEditor.{CommitFn, Lookup, ValidationFn}

      val pxLookupAll = Px.apply2(pxProject, pxPlainTextNoCtx)(ImplicationEditor.Lookup.all)

      override type Args   = Unit
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

      private def start(id: ReqId, dir: Direction, pxLookup: Px[Lookup]): InitFn = ictx => args => {
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

        val (abort, commitFn) =
          makeAbortCommitFn[ImplicationEditor.Output](UpdateContentCmd.PatchImplications(id, dir, _), args.hooks)

        startWithStateSnapshot(pxInit.toCallback.toCBO)(_._2)(
          _ => new State(_, pxLookup, pxValFn, abort, commitFn))
      }

      private class State(ss         : StateSnapshot[String],
                          pxLookup   : Px[Lookup],
                          pxValFn    : Px[ValidationFn],
                          abort      : Some[Callback],
                          commitFn   : Some[CommitFn]) extends EditorImpl {

        override type Props = ImplicationEditor.Props
        override def renderImpl = _.render
        override def changeImpl = _.validated
        override val props = (_, asyncState) =>
          for {
            lookup     <- pxLookup.toCallback
            valFn      <- pxValFn.toCallback
            textSearch <- pxTextSearch.toCallback
          } yield ImplicationEditor.Props(
            edit             = ss,
            lookup           = lookup,
            validationFn     = valFn,
            asyncStatus      = EditorStatus.async(asyncState),
            abort            = abort,
            commitFn         = commitFn,
            commitVerb       = commitVerb,
            textSearch       = textSearch,
            extraKbShortcuts = KeyboardTheme.Shortcuts.empty,
            showInstructions = true)
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditTags extends ForChangeType {
      import shipreq.webapp.client.project.widgets.TagEditor
      import TagEditor.{CommitFn, Lookup}

      override type Args   = Unit
      override type Change = TagEditor.Output

      def apply(id: ReqId, fid: Option[CustomField.Tag.Id]): InitFn = ictx => args => {
        import ictx._

        val lookupFn = fid.fold[Project => Lookup](Lookup.notUsedInTagFields)(Lookup.forTagField)
        val pxLookup = pxProject map lookupFn

        val pxInit: Px[(Set[ApplicableTagId], String)] =
          for {
            project <- pxProject
            lookup <- pxLookup
          } yield TagEditor.initialValues(project.content.reqTags(id), project.config, lookup)

        val (abort, commitFn) =
          makeAbortCommitFn[TagEditor.Output](UpdateContentCmd.PatchReqTags(id, _), args.hooks)

        startWithStateSnapshot(pxInit.toCallback.toCBO)(_._2)(
          init => new State(_, Some(init._1), pxLookup, abort, commitFn))
      }

      private class State(ss           : StateSnapshot[String],
                          initialValues: Some[Set[ApplicableTagId]],
                          pxLookup     : Px[Lookup],
                          abort        : Some[Callback],
                          commitFn     : Some[CommitFn]) extends EditorImpl {

        override type Props = TagEditor.Props
        override def renderImpl = _.render
        override def changeImpl = _.validated
        override val props = (_, asyncState) =>
          for {
            lookup <- pxLookup.toCallback
          } yield TagEditor.Props(
            preEditValue     = initialValues,
            edit             = ss,
            lookup           = lookup,
            asyncStatus      = EditorStatus.async(asyncState),
            abort            = abort,
            commitFn         = commitFn,
            commitVerb       = commitVerb,
            extraKbShortcuts = KeyboardTheme.Shortcuts.empty,
            showInstructions = true)
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditRichText {
      import shipreq.webapp.base.text._
      import shipreq.webapp.client.project.widgets.RichTextEditor

      abstract class Base[T <: Text.Generic](val editor: RichTextEditor[T]) extends ForChangeType {
        val T: editor.text.type = editor.text

        override type Args   = Unit
        override type Change = T.OptionalText

        protected def start(cmd           : T.OptionalText => UpdateContentCmd,
                            initialValueCB: CallbackOption[T.OptionalText],
                            pid           : PreviewId): InitFn = ictx => args => {
          import ictx._

          val (abort, commitFn) =
            makeAbortCommitFn(cmd, args.hooks)

          val initCB =
            for {
              initialValue   <- initialValueCB
              projectWidgets <- args.cbProjectWidgets.toCBO
            } yield {
              val initialText = projectWidgets.plainText.text(initialValue, RichTextEditor.hardcodedLive)
              (initialValue, initialText)
            }

          startWithStateSnapshot(initCB)(_._2)(
            i => new State(_, Some(i._1), args.cbProjectWidgets, pid, abort, commitFn))
        }

        private class State(ss              : StateSnapshot[String],
                            initial         : Some[T.OptionalText],
                            projectWidgetsCB: CallbackTo[ProjectWidgets.AnyCtx],
                            pid             : PreviewId,
                            abort           : Some[Callback],
                            commitFn        : Some[editor.CommitFn]) extends EditorImpl {

          override type Props = editor.Props
          override def renderImpl = _.render
          override def changeImpl = _.validated
          override val props = (_, asyncState) =>
            for {
              previewRW      <- previewW.toReadWriteCB
              project        <- pxProject.toCallback
              plainTextNoCtx <- pxPlainTextNoCtx.toCallback
              textSearch     <- pxTextSearch.toCallback
              projectWidgets <- projectWidgetsCB
            } yield editor.Props(
              project          = project,
              plainTextNoCtx   = plainTextNoCtx,
              textSearch       = textSearch,
              projectWidgets   = projectWidgets,
              edit             = ss,
              asyncStatus      = EditorStatus.async(asyncState),
              abort            = abort,
              commitFn         = commitFn,
              commitVerb       = commitVerb,
              preview          = previewRW(pid),
              preEditValue     = initial,
              extraKbShortcuts = KeyboardTheme.Shortcuts.empty,
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
          pxProject.toCallback.map(p => ReqData.textAt(fid, id).get(p.content.reqText)).toCBO,
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

      override type Args = FieldKey.UseCaseStep.Args
      override type Change = UseCaseStepGD.NonEmptyValues

      def apply(id: UseCaseStepId, pid: PreviewId): InitFn = ictx => args => {
        import ictx._

        val commitFn: UseCaseStepEditor.CommitFn =
          Reusable.fn(v => commit(UpdateContentCmd.UpdateUseCaseStep(id, v), args.hooks))

        val pxStepFocus: Px[UseCaseStep.Focus] =
          pxProject.map(_.content.reqs.useCases.focusStep(id))

        val pxInit: Px[(UseCaseStepEditor.InitialValue, String)] =
          for {
            stepFocus      <- pxStepFocus
            projectWidgets <- args.pxProjectWidgets.value
          } yield {
            val initialValue = TextAndFlow(stepFocus.step.titleExplicitly, Direction.Values(stepFocus.flow))
            val initialText = projectWidgets.plainText.useCaseStepTextAndFlow(initialValue, hardcodedLive)
            (initialValue, initialText)
          }

        startWithStateSnapshot(pxInit.toCallback.toCBO)(_._2)(
          i => new State(_, Some(i._1), args.cbProjectWidgets, pxStepFocus.toCallback, pid, abort(args.hooks), commitFn))
      }

      private class State(ss              : StateSnapshot[String],
                          initial         : Some[UseCaseStepEditor.InitialValue],
                          projectWidgetsCB: CallbackTo[ProjectWidgets.AnyCtx],
                          stepFocusCB     : CallbackTo[UseCaseStep.Focus],
                          pid             : PreviewId,
                          abort           : Callback,
                          commitFn        : UseCaseStepEditor.CommitFn) extends EditorImpl {

        override type Props = UseCaseStepEditor.Props
        override def renderImpl = _.render
        override def changeImpl = _.validatedChanges
        override val props = (args: Args, asyncState) =>
          for {
            previewRW      <- previewW.toReadWriteCB
            project        <- pxProject.toCallback
            plainTextNoCtx <- pxPlainTextNoCtx.toCallback
            textSearch     <- pxTextSearch.toCallback
            projectWidgets <- projectWidgetsCB
            step           <- stepFocusCB
          } yield {

            val shiftRunner: AsyncFeature.Runner.D0O[LeftRight, Any] =
              args.shiftRunner.mapRunOption(run =>
                Reusable.never(d =>
                  step.canShift(d).option(
                    run(UpdateContentCmd.ShiftUseCaseStep(step.id, d)))))

            val addStepRunner: AsyncFeature.Runner.D0O[Unit, Any] =
              args.addStepRunner.mapRunOption(run =>
                Reusable.never(_ =>
                  step.field.canInsertAfter(step.loc).option(
                    run(UpdateContentCmd.AddUseCaseStep(step.useCaseId, step.field, step.loc.asParentLoc)))))

            UseCaseStepEditor.Props(
              project        = project,
              plainTextNoCtx = plainTextNoCtx,
              textSearch     = textSearch,
              projectWidgets = projectWidgets,
              edit           = ss,
              asyncStatus    = EditorStatus.async(asyncState),
              abort          = abort,
              commit         = commitFn,
              shiftRunner    = shiftRunner,
              addStepRunner  = addStepRunner,
              preview        = previewRW(pid),
              preEditValue   = initial)
          }
      }
    }

  }
}
