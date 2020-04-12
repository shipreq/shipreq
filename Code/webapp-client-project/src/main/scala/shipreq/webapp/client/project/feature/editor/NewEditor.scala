package shipreq.webapp.client.project.feature.editor

import japgolly.microlibs.nonempty._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import scala.annotation.elidable
import scalaz.~~>
import shipreq.base.util.ScalaExt._
import shipreq.base.util.VectorTree.LocationOps
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.UseCaseStepGD
import shipreq.webapp.base.feature._
import shipreq.webapp.base.feature.clipboard.ClipboardData
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.lib.KeyboardTheme
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.protocol.websocket.{ManualIssueCmd, UpdateContentCmd}
import shipreq.webapp.base.text._
import shipreq.webapp.base.util.CallbackHelpers._
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
  final case class CreationArgs(pxProjectWidgets: Reusable[Px[ProjectWidgets.AnyCtx]],
                                filterDead      : FilterDead,
                                potentialValue  : Option[PotentialValue],
                                hooks           : Hooks) {
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
                          sspUpdateContent: ServerSideProcInvoker[UpdateContentCmd, ErrorMsg, Any],
                          sspManualIssue  : ServerSideProcInvoker[ManualIssueCmd, ErrorMsg, Any],
                         ) {

    private[NewEditor] val internal = new Internal(this)
  }

  final case class Ctx[A, Change](stateAccess : StateAccessPure[State.ForEditor[A, Change]],
                                  asyncFeature: AsyncFeature.Write.D0[AsyncError])

  type ForFields[FK <: FieldKey] = FieldKey.Fold[FK, ForEditor]

  type ForEditor[A, Change] = Ctx[A, Change] => NewEditor

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

      /** Currently all the editor types calculate their changes in the props. Some props need additional args for their
        * creation. Combine the two and we're now in a state where we need an Args to generate a Props to get a Change.
        *
        * At the moment, we're able to provide instances of Args for all types such that they don't affect the Changes.
        *
        * In future this might not be possible in which case, we'll need to split Props into two so that Changes can be
        * generated without Args, and this cheaty method should be removed.
        */
      protected def changeArgs: Args

      final override def render(p: Permission, as: AsyncState, args: Args): Option[VdomElement] =
        // Looks like this could block async but not so. Can't go from edit -> async -> notAllowed.
        // Unsafety is allowed here because EditorInstance is never Reusable
        p match {
          case Allow => Some(renderImpl(props(args, as).runNow()))
          case Deny  => None
        }

      private[this] lazy val _change = props(changeArgs, None).map(changeImpl)

      final override def change[C >: Change]: CallbackTo[Editor.Change[C]] =
        _change.widen
    }

    def init[FieldArgs, Change, A](pva: PotentialValueAcceptor[A])
                                  (userInit: Option[A] => Init[FieldArgs, Change]): Init[FieldArgs, Change] = args => {

      args.potentialValue match {

        case None =>
          userInit(None)(args)

        case Some(pv) =>
          for {
            a <- CallbackOption.liftOption(pva.accept(pv)) // halt here if PotentialValueAcceptor rejects value
            e <- userInit(Some(a))(args)
          } yield e
      }
    }

  } // Internal

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
        codes           = _ => EditReqCodes.Multiple(r.id),
        customTextField = f => EditRichText.CustomTextField(r.id, f.field, PreviewId(r, f)),
        implications    = f => EditImplications(r.id, f.scope),
        reqType         = _ => EditReqType(r.id),
        allTags         = f => EditTags.allTags(r.id),
        otherTags       = f => EditTags.otherTags(r.id),
        customFieldTags = f => EditTags.customField(r.id, f.field),
        title           = f => EditRichText.GenericReqTitle(r.id, PreviewId(r, f)))

      def prepareUC(r: RowKey.UseCase) = FieldKey.FoldForUseCase[LogicPerField](
        codes           = _ => EditReqCodes.Multiple(r.id),
        customTextField = f => EditRichText.CustomTextField(r.id, f.field, PreviewId(r, f)),
        implications    = f => EditImplications(r.id, f.scope),
        allTags         = f => EditTags.allTags(r.id),
        otherTags       = f => EditTags.otherTags(r.id),
        customFieldTags = f => EditTags.customField(r.id, f.field),
        title           = f => EditRichText.UseCaseTitle(r.id, PreviewId(r, f)))

      lazy val forUseCaseSteps = FieldKey.FoldForUseCaseSteps[ForEditor](
        f => logicToPerField(EditUseCaseStep(f.id, PreviewId(RowKey.UseCaseSteps, f))))

      lazy val forManualIssues = FieldKey.FoldForManualIssues[ForEditor](
        f => logicToPerField(EditRichTextNonEmpty.ManualIssue(f.id, PreviewId(RowKey.ManualIssues, f))))

      RowKey.Fold[ForFields](
        codeGroup    = prepareCG(_).map(logicToPerField),
        genericReq   = prepareGR(_).map(logicToPerField),
        useCase      = prepareUC(_).map(logicToPerField),
        useCaseSteps = () => forUseCaseSteps,
        manualIssues = () => forManualIssues,
      )
    }

    final class InternalCtx[A, C](val ctx: Ctx[A, C]) {
      import ctx._

      def abort(hooks: Hooks): Callback =
        stateAccess.setState(None, asyncFeature.clearAsyncStatus >> hooks.onClose)

      def commit[Cmd](ssp: ServerSideProcInvoker[Cmd, ErrorMsg, Any])
                     (cmd: Cmd, hooks: Hooks): Callback =
        asyncFeature(
          ssp(cmd)
            .rightFlatTap(_ => abort(hooks).asAsyncCallback)
        )

      def makeAbortCommitFn[Cmd, B](ssp: ServerSideProcInvoker[Cmd, ErrorMsg, Any])
                                   (cmd: B => Cmd, hooks: Hooks): (Some[Callback], Some[B ~=> Callback]) =
        (Some(abort(hooks)), Some(Reusable.fn(v => commit(ssp)(cmd(v), hooks))))

      /** Creates a Callback that when invoked, will initialise and start an editor.
        *
        * @tparam S Initial data. Data captured before starting the editor.
        * @tparam B The initial value of the editor.
        * @tparam E The editor
        */
      def startWithStateSnapshot[S, B: Reusability, E <: Editor[A, C]](initialData : CallbackOption[S])
                                                                      (initialValue: S => B)
                                                                      (editor      : S => StateSnapshot[B] => E): CallbackOption[E] =
        initialData.flatMap { s =>
          val editorCtor = editor(s)

          lazy val update: Reusable[SetStateFnPure[B]] =
            Reusable.byRef(stateAccess.toSetStateFn.contramap(newEditor))

          def newEditor: B => Some[E] =
            b => Some(editorCtor(StateSnapshot.withReuse(b)(update)))

          CallbackOption.liftOption(newEditor(initialValue(s)))
        }

      def startWithStateSnapshot[S, B: Reusability, E <: Editor[A, C]](initialData      : CallbackOption[S],
                                                                       initalValueOption: Option[B])
                                                                      (initialValueFn   : S => B)
                                                                      (editor           : S => StateSnapshot[B] => E): CallbackOption[E] =
        startWithStateSnapshot(initialData)(s => initalValueOption.getOrElse(initialValueFn(s)))(editor)

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

          @elidable(elidable.FINER)
          override def toString = s"EditReqType(${ss.value})"

          def ss = StateSnapshot(editValue)(stateAccess.toSetStateFn.contramap(e => copy(editValue = e).some))

          override type Props = ReqTypeSelector.Props
          override def renderImpl = _.render
          override def changeArgs = ()
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

          override def clipboardData: Option[ClipboardData] =
            Some(ClipboardData(PlainText.reqTypeFull(ss.value)))

          override def setPotentialValue(p: PotentialValue): Option[Callback] =
            Some {
              for {
                choices <- pxChoices.toCallback
                pva      = ReqTypeSelector.potentialValueAcceptor(choices.whole)
                _       <- pva.accept(p).map(ss.setState).getOrEmpty
              } yield ()
            }
        }

        val (abort, commitFn) =
          makeAbortCommitFn(sspUpdateContent)((t: RT) => UpdateContentCmd.SetGenericReqType(id, t.id), args.hooks)

        for {
          req      <- getGenericReq(id)
          current  <- getCustomReqTypeCB(req.reqTypeId)
          pxChoices = ReqTypeSelector.pxChoices(current, pxCustomReqTypes)
          initial  <- args.potentialValue match {
                        case None     => CallbackOption.pure(current)
                        case Some(pv) =>
                          for {
                            choices <- pxChoices.toCallback.toCBO
                            pva      = ReqTypeSelector.potentialValueAcceptor(choices.whole)
                            i       <- CallbackOption.liftOption(pva.accept(pv))
                          } yield i
                      }
        } yield State(Some(current), initial, pxChoices, abort, commitFn)
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditReqCodes {
      import shipreq.webapp.client.project.widgets.ReqCodeEditor

      val trieCB: CallbackTo[ReqCode.Trie] =
        pxProject.toCallback.map(_.content.reqCodes.trie)

      object Multiple extends ForChangeType {
        import ReqCodeEditor.{Multiple => RCE}
        import RCE.potentialValueAcceptor

        override type Args   = Unit
        override type Change = RCE.Output

        def apply(id: ReqId): InitFn = ictx => Internal.init(potentialValueAcceptor) { ivo => args =>
          import ictx._

          val initialValuesCB: CallbackTo[Set[ReqCode.Value]] =
            pxProject.toCallback.map(_.content.reqCodes.activeReqCodesByReqId(id))

          val (abort, commitFn) =
            makeAbortCommitFn(sspUpdateContent)(UpdateContentCmd.PatchReqCodes(id, _), args.hooks)

        startWithStateSnapshot(
          initialData       = initialValuesCB.toCBO,
          initalValueOption = ivo)(
          initialValueFn    = ReqCodeEditor.Multiple.seqFmt merge _.toVector.map(PlainText.reqCode).sorted)(
          editor            = initialValues => new State(_, Some(initialValues), abort, commitFn))
        }

        private class State(ss      : StateSnapshot[String],
                            initial : Some[Set[ReqCode.Value]],
                            abort   : Some[Callback],
                            commitFn: Some[RCE.CommitFn]) extends EditorImpl {

          override type Props = RCE.Props
          override def renderImpl = _.render
          override def changeArgs = ()
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
              autoFocus        = true,
              commitFn         = commitFn,
              commitVerb       = commitVerb,
              extraKbShortcuts = KeyboardTheme.Shortcuts.empty,
              showInstructions = true)

          override def clipboardData: Option[ClipboardData] =
            Some(ClipboardData(ss.value))

          override def setPotentialValue(p: PotentialValue): Option[Callback] =
            potentialValueAcceptor.accept(p).map(ss.setState)
        }
      }

      object Single extends ForChangeType {
        import ReqCodeEditor.{Single => RCE}
        import RCE.potentialValueAcceptor

        override type Args   = Unit
        override type Change = RCE.Output

        def apply(id: ReqCodeGroupId): InitFn = ictx => Internal.init(potentialValueAcceptor) { ivo => args =>
          import ictx._

          val initialValueCB: CallbackOption[ReqCode.Value] =
            pxProject.toCallback.map(_.content.reqCodes.reqCode(id)).toCBO

          val (abort, commitFn) =
            makeAbortCommitFn(sspUpdateContent)(UpdateContentCmd.SetCodeGroupCode(id, _), args.hooks)

        startWithStateSnapshot(
          initialData       = initialValueCB,
          initalValueOption = ivo)(
          initialValueFn    = PlainText.reqCode)(
          editor            = i => new State(_, Some(i), abort, commitFn))
        }

        private class State(ss      : StateSnapshot[String],
                            initial : Some[ReqCode.Value],
                            abort   : Some[Callback],
                            commitFn: Some[RCE.CommitFn]) extends EditorImpl {

          @elidable(elidable.FINER)
          override def toString = s"EditReqCodes(${ss.value})"

          override type Props = RCE.Props
          override def renderImpl = _.render
          override def changeArgs = ()
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
              autoFocus        = true,
              commitFn         = commitFn,
              commitVerb       = commitVerb,
              extraKbShortcuts = KeyboardTheme.Shortcuts.empty,
              showInstructions = true)

          override def clipboardData: Option[ClipboardData] =
            Some(ClipboardData(ss.value))

          override def setPotentialValue(p: PotentialValue): Option[Callback] =
            potentialValueAcceptor.accept(p).map(ss.setState)
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

        val pxCorrector: Px[String => String] =
          for {
            lookup <- pxLookup
            valFn  <- pxValFn
          } yield valFn(lookup).corrector.live

        val potentialValueAcceptor = PotentialValueAcceptor.correct(pxCorrector.value())

        Internal.init(potentialValueAcceptor) { ivo => args =>

          val (abort, commitFn) =
            makeAbortCommitFn(sspUpdateContent)(UpdateContentCmd.PatchImplications(id, dir, _), args.hooks)

        startWithStateSnapshot(
          initialData       = pxInit.toCallback.toCBO,
          initalValueOption = ivo)(
          initialValueFn    = _._2)(
          editor            = _ => new State(_, pxLookup, pxValFn, pxCorrector, abort, commitFn))
        }
      }

      private class State(ss         : StateSnapshot[String],
                          pxLookup   : Px[Lookup],
                          pxValFn    : Px[ValidationFn],
                          pxCorrector: Px[String => String],
                          abort      : Some[Callback],
                          commitFn   : Some[CommitFn]) extends EditorImpl {

        @elidable(elidable.FINER)
        override def toString = s"EditImplications(${ss.value})"

        private val pxPotentialValueAcceptor =
          pxCorrector.map(PotentialValueAcceptor.correct)

        override type Props = ImplicationEditor.Props
        override def renderImpl = _.render
        override def changeArgs = ()
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
            autoFocus        = true,
            commitFn         = commitFn,
            commitVerb       = commitVerb,
            textSearch       = textSearch,
            extraKbShortcuts = KeyboardTheme.Shortcuts.empty,
            showInstructions = true)

        override def clipboardData: Option[ClipboardData] =
          Some(ClipboardData(ss.value))

        override def setPotentialValue(p: PotentialValue): Option[Callback] =
          Some {
            for {
              pva <- pxPotentialValueAcceptor.toCallback
              _   <- pva.accept(p).fold(Callback.empty)(ss.setState)
            } yield ()
          }
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditTags extends ForChangeType {
      import shipreq.webapp.client.project.widgets.TagEditor
      import TagEditor.{CommitFn, Lookup, potentialValueAcceptor}

      override type Args   = Unit
      override type Change = TagEditor.Output

      def allTags(id: ReqId): InitFn =
        apply(id, Lookup.all)

      def otherTags(id: ReqId): InitFn =
        apply(id, Lookup.notUsedInTagFields)

      def customField(id: ReqId, fid: CustomField.Tag.Id): InitFn =
        apply(id, Lookup.forTagField(fid))

      def apply(id: ReqId, lookupFn: Project => Lookup): InitFn = ictx => Internal.init(potentialValueAcceptor) { ivo => args =>
        import ictx._

        val pxLookup = pxProject map lookupFn
        val pxNaTags = pxProject.map(_.naTagsForReq(id))

        val pxInit: Px[(Set[ApplicableTagId], String)] =
          for {
            project <- pxProject
            lookup <- pxLookup
            naTags <- pxNaTags
          } yield TagEditor.initialValues(project.content.reqTags(id), project.config, lookup, naTags)

        val (abort, commitFn) =
          makeAbortCommitFn(sspUpdateContent)(UpdateContentCmd.PatchReqTags(id, _), args.hooks)

        startWithStateSnapshot(
          initialData       = pxInit.toCallback.toCBO,
          initalValueOption = ivo)(
          initialValueFn    = _._2)(
          editor            = i => ss => new State(
            ss            = ss,
            initialValues = Some(i._1),
            pxLookup      = pxLookup,
            pxNaTags      = pxNaTags,
            abort         = abort,
            commitFn      = commitFn))
      }

      private class State(ss           : StateSnapshot[String],
                          initialValues: Some[Set[ApplicableTagId]],
                          pxLookup     : Px[Lookup],
                          pxNaTags     : Px[NaTags],
                          abort        : Some[Callback],
                          commitFn     : Some[CommitFn]) extends EditorImpl {

        @elidable(elidable.FINER)
        override def toString = s"EditTags(${ss.value})"

        override type Props = TagEditor.Props
        override def renderImpl = _.render
        override def changeArgs = ()
        override def changeImpl = _.validated
        override val props = (_, asyncState) =>
          for {
            lookup <- pxLookup.toCallback
            naTags <- pxNaTags.toCallback
          } yield TagEditor.Props(
            preEditValue     = initialValues,
            naTags           = naTags,
            edit             = ss,
            lookup           = lookup,
            asyncStatus      = EditorStatus.async(asyncState),
            abort            = abort,
            autoFocus        = true,
            commitFn         = commitFn,
            commitVerb       = commitVerb,
            extraKbShortcuts = KeyboardTheme.Shortcuts.empty,
            showInstructions = true)

        override def clipboardData: Option[ClipboardData] =
          Some(ClipboardData(ss.value))

        override def setPotentialValue(p: PotentialValue): Option[Callback] =
          potentialValueAcceptor.accept(p).map(ss.setState)
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditRichText {
      import shipreq.webapp.base.text._
      import shipreq.webapp.client.project.widgets.RichTextEditor

      abstract class Base[T <: Text.Generic](val editor: RichTextEditor[T]) extends ForChangeType {
        val T: editor.text.type = editor.text

        import editor.potentialValueAcceptor

        override type Args   = Unit
        override type Change = T.OptionalText

        protected def start(cmd           : T.OptionalText => UpdateContentCmd,
                            initialValueCB: CallbackOption[T.OptionalText],
                            pid           : PreviewId,
                            reqId         : Option[ReqId]): InitFn = ictx => Internal.init(potentialValueAcceptor) { ivo => args =>
          import ictx._

          val (abort, commitFn) =
            makeAbortCommitFn(sspUpdateContent)(cmd, args.hooks)

          val initCB =
            for {
              initialValue   <- initialValueCB
              projectWidgets <- args.cbProjectWidgets.toCBO
            } yield {
              val initialText = projectWidgets.plainText.text(initialValue, RichTextEditor.hardcodedLive, Optional)
              (initialValue, initialText)
            }

        startWithStateSnapshot(
          initialData       = initCB,
          initalValueOption = ivo)(
          initialValueFn    = _._2)(
          editor            = i => new State(_, Some(i._1), args.cbProjectWidgets, pid, reqId, abort, commitFn))
        }

        private class State(ss              : StateSnapshot[String],
                            initial         : Some[T.OptionalText],
                            projectWidgetsCB: CallbackTo[ProjectWidgets.AnyCtx],
                            pid             : PreviewId,
                            reqId           : Option[ReqId],
                            abort           : Some[Callback],
                            commitFn        : Some[editor.Optional.CommitFn]) extends EditorImpl {

          @elidable(elidable.FINER)
          override def toString = s"EditRichText(${ss.value})"

          override type Props = editor.Optional
          override def renderImpl = _.render
          override def changeArgs = ()
          override def changeImpl = _.validated
          override val props = (_, asyncState) =>
            for {
              previewRW      <- previewW.toReadWriteCB
              project        <- pxProject.toCallback
              plainTextNoCtx <- pxPlainTextNoCtx.toCallback
              textSearch     <- pxTextSearch.toCallback
              projectWidgets <- projectWidgetsCB
            } yield editor.Optional(
              project          = project,
              naTags           = project.naTagsForReq(reqId),
              plainTextNoCtx   = plainTextNoCtx,
              textSearch       = textSearch,
              projectWidgets   = projectWidgets,
              edit             = ss,
              asyncStatus      = EditorStatus.async(asyncState),
              abort            = abort,
              autoFocus        = true,
              commitFn         = commitFn,
              commitVerb       = commitVerb,
              preview          = previewRW(pid),
              preEditValue     = initial,
              extraKbShortcuts = KeyboardTheme.Shortcuts.empty,
              showInstructions = true)

          override def clipboardData: Option[ClipboardData] =
            Some(ClipboardData(ss.value))

          override def setPotentialValue(p: PotentialValue): Option[Callback] =
            potentialValueAcceptor.accept(p).map(ss.setState)
        }
      }

      object CodeGroupTitle extends Base(RichTextEditor.CodeGroupTitle) {
        def apply(id: ReqCodeGroupId, pid: PreviewId): InitFn = start(
          cmd            = UpdateContentCmd.SetCodeGroupTitle(id, _),
          initialValueCB = getCodeGroup(id).map(_.title).widen,
          pid            = pid,
          reqId          = None)
      }

      object CustomTextField extends Base(RichTextEditor.CustomTextField) {
        def apply(id: ReqId, fid: CustomField.Text.Id, pid: PreviewId): InitFn = start(
          cmd            = UpdateContentCmd.SetCustomTextField(id, fid, _),
          initialValueCB = pxProject.toCallback.map(p => ReqData.textAt(fid, id).get(p.content.reqText)).toCBO,
          pid            = pid,
          reqId          = Some(id))
      }

      object GenericReqTitle extends Base(RichTextEditor.GenericReqTitle) {
        def apply(id: GenericReqId, pid: PreviewId): InitFn = start(
          cmd            = UpdateContentCmd.SetGenericReqTitle(id, _),
          initialValueCB = getGenericReq(id).map(_.title),
          pid            = pid,
          reqId          = Some(id))
      }

      object UseCaseTitle extends Base(RichTextEditor.UseCaseTitle) {
        def apply(id: UseCaseId, pid: PreviewId): InitFn = start(
          cmd            = UpdateContentCmd.SetUseCaseTitle(id, _),
          initialValueCB = getUseCase(id).map(_.title),
          pid            = pid,
          reqId          = Some(id))
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditRichTextNonEmpty {
      import shipreq.webapp.base.text._
      import shipreq.webapp.client.project.widgets.RichTextEditor

      abstract class Base[T <: Text.Generic, Cmd](val editor: RichTextEditor[T],
                                                  ssp: ServerSideProcInvoker[Cmd, ErrorMsg, Any]) extends ForChangeType {
        val T: editor.text.type = editor.text

        import editor.potentialValueAcceptor

        override type Args   = Unit
        override type Change = T.NonEmptyText

        protected def start(cmd           : T.NonEmptyText => Cmd,
                            initialValueCB: CallbackOption[T.NonEmptyText],
                            pid           : PreviewId,
                            reqId         : Option[ReqId]): InitFn = ictx => Internal.init(potentialValueAcceptor) { ivo => args =>
          import ictx._

          val (abort, commitFn) =
            makeAbortCommitFn(ssp)(cmd, args.hooks)

          val initCB =
            for {
              initialValue   <- initialValueCB
              projectWidgets <- args.cbProjectWidgets.toCBO
            } yield {
              val initialText = projectWidgets.plainText.text(initialValue.whole, RichTextEditor.hardcodedLive, Mandatory)
              (initialValue, initialText)
            }

          startWithStateSnapshot(
            initialData       = initCB,
            initalValueOption = ivo)(
            initialValueFn    = _._2)(
            editor            = i => new State(_, Some(i._1), args.cbProjectWidgets, pid, reqId, abort, commitFn))
        }

        private class State(ss              : StateSnapshot[String],
                            initial         : Some[T.NonEmptyText],
                            projectWidgetsCB: CallbackTo[ProjectWidgets.AnyCtx],
                            pid             : PreviewId,
                            reqId           : Option[ReqId],
                            abort           : Some[Callback],
                            commitFn        : Some[editor.NonEmpty.CommitFn]) extends EditorImpl {

          @elidable(elidable.FINER)
          override def toString = s"EditRichTextNonEmpty(${ss.value})"

          override type Props = editor.NonEmpty
          override def renderImpl = _.render
          override def changeArgs = ()
          override def changeImpl = _.validated
          override val props = (_, asyncState) =>
            for {
              previewRW      <- previewW.toReadWriteCB
              project        <- pxProject.toCallback
              plainTextNoCtx <- pxPlainTextNoCtx.toCallback
              textSearch     <- pxTextSearch.toCallback
              projectWidgets <- projectWidgetsCB
            } yield editor.NonEmpty(
              project          = project,
              naTags           = project.naTagsForReq(reqId),
              plainTextNoCtx   = plainTextNoCtx,
              textSearch       = textSearch,
              projectWidgets   = projectWidgets,
              edit             = ss,
              asyncStatus      = EditorStatus.async(asyncState),
              abort            = abort,
              autoFocus        = true,
              commitFn         = commitFn,
              commitVerb       = commitVerb,
              preview          = previewRW(pid),
              preEditValue     = initial,
              extraKbShortcuts = KeyboardTheme.Shortcuts.empty,
              showInstructions = true)

          override def clipboardData: Option[ClipboardData] =
            Some(ClipboardData(ss.value))

          override def setPotentialValue(p: PotentialValue): Option[Callback] =
            potentialValueAcceptor.accept(p).map(ss.setState)
        }
      }

       object ManualIssue extends Base(RichTextEditor.ManualIssue, sspManualIssue) {
         def apply(id: ManualIssueId, pid: PreviewId): InitFn = start(
           cmd            = ManualIssueCmd.Update(id, _),
           initialValueCB = pxProject.toCallback.map(_.manualIssues.imap.need(id).text).toCBO,
           pid            = pid,
           reqId          = None)
       }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditUseCaseStep extends ForChangeType {
      import shipreq.webapp.client.project.widgets.RichTextEditor.hardcodedLive
      import shipreq.webapp.client.project.widgets.UseCaseStepEditor
      import shipreq.webapp.client.project.widgets.UseCaseStepEditor.potentialValueAcceptor
      import UseCaseStepFlowText.TextAndFlow

      override type Args = FieldKey.UseCaseStep.Args
      override type Change = UseCaseStepGD.NonEmptyValues

      def apply(id: UseCaseStepId, pid: PreviewId): InitFn = ictx => Internal.init(potentialValueAcceptor) { ivo => args =>
        import ictx._

        val commitFn: UseCaseStepEditor.CommitFn =
          // Below you'll see that we're filtering flow to create visibleFlow
          // There's no need to re-insert invisibleFlow here because flow is passed as a SetDiff
          // meaning that regardless of what the actual flow is, we only send what the user changes.
          Reusable.fn(v => commit(sspUpdateContent)(UpdateContentCmd.UpdateUseCaseStep(id, v), args.hooks))

        val pxStepFocus: Px[UseCaseStep.Focus] =
          pxProject.map(_.content.reqs.useCases.focusStep(id))

        val pxInit: Px[(UseCaseStepEditor.InitialValue, String)] =
          for {
            stepFocus      <- pxStepFocus
            projectWidgets <- args.pxProjectWidgets.value
          } yield {
            val visibleFlow = args.filterDead match {
              case HideDead => Direction.Values(stepFocus.flow(_, Live))
              case ShowDead => Direction.Values(stepFocus.flow)
            }
            val initialValue = TextAndFlow(stepFocus.step.titleExplicitly, visibleFlow)
            val initialText = projectWidgets.plainText.useCaseStepTextAndFlow(initialValue, hardcodedLive)
            (initialValue, initialText)
          }

        startWithStateSnapshot(
          initialData       = pxInit.toCallback.toCBO,
          initalValueOption = ivo)(
          initialValueFn    = _._2)(
          editor            = i => new State(_, Some(i._1), args.cbProjectWidgets, pxStepFocus.toCallback, pid, abort(args.hooks), commitFn))
      }

      private class State(ss              : StateSnapshot[String],
                          initial         : Some[UseCaseStepEditor.InitialValue],
                          projectWidgetsCB: CallbackTo[ProjectWidgets.AnyCtx],
                          stepFocusCB     : CallbackTo[UseCaseStep.Focus],
                          pid             : PreviewId,
                          abort           : Callback,
                          commitFn        : UseCaseStepEditor.CommitFn) extends EditorImpl {

        @elidable(elidable.FINER)
        override def toString = s"EditUseCaseStep(${ss.value})"

        override type Props = UseCaseStepEditor.Props
        override def renderImpl = _.render
        override def changeArgs = FieldKey.UseCaseStep.Args.empty
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

            val shiftRunner: Option[AsyncFeature.Runner.D0O[LeftRight, Any]] =
              args.shiftRunner.map(
                _.mapRunOption(run =>
                  Reusable.never(d =>
                    step.canShift(d).option(
                      run(UpdateContentCmd.ShiftUseCaseStep(step.id, d))))))

            val addStepRunner: Option[AsyncFeature.Runner.D0O[Unit, Any]] =
              args.addStepRunner.map(
                _.mapRunOption(run =>
                  Reusable.never(_ =>
                    UpdateContentCmd.addUseCaseStepAfter(step).map(run))))

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

        override def clipboardData: Option[ClipboardData] =
          Some(ClipboardData(ss.value))

        override def setPotentialValue(p: PotentialValue): Option[Callback] =
          potentialValueAcceptor.accept(p).map(ss.setState)
      }
    }

  } // Internal
}
