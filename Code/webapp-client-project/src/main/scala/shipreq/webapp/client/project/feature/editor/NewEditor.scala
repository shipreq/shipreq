package shipreq.webapp.client.project.feature.editor

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import scalaz.~~>
import shipreq.base.util.ScalaExt._
import shipreq.base.util._
import shipreq.webapp.base.feature._
import shipreq.webapp.base.feature.clipboard.ClipboardData
import shipreq.webapp.base.lib.ConfirmJs
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.util.CallbackHelpers._
import shipreq.webapp.base.util.{LastValueMemo, LruMemo}
import shipreq.webapp.client.project.feature.editor.Feature.{AsyncError, AsyncState, Editor, PreviewId, State}
import shipreq.webapp.client.project.widgets.ProjectWidgets
import shipreq.webapp.member.feature._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event.UseCaseStepGD
import shipreq.webapp.member.project.protocol.websocket.{ManualIssueCmd, UpdateContentCmd}
import shipreq.webapp.member.project.text._
import shipreq.webapp.member.project.util.DataReusability._
import shipreq.webapp.member.ui.OptionalFullscreen

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
      @nowarn("cat=unused")
      implicit val x: Reusability[Callback] = Reusability.callbackByRef
      Reusability.byRef || Reusability.derive
    }
  }

  final case class Static(previewW          : PreviewFeature.Write.Composite[PreviewId],
                          pxProject         : Px[Project],
                          pxPlainTextNoCtx  : Px[PlainText.ForProject.NoCtx],
                          pxTextSearch      : Px[TextSearch],
                          confirmJs         : ConfirmJs,
                          sspUpdateContent  : ServerSideProcInvoker[UpdateContentCmd, ErrorMsg, Any],
                          sspManualIssue    : ServerSideProcInvoker[ManualIssueCmd, ErrorMsg, Any],
                          optionalFullscreen: OptionalFullscreen,
                         ) {

    val someConfirmJs =
      Some(confirmJs)

    val someOptionalFullscreen =
      Some(optionalFullscreen)

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
      protected val props: (Args, AsyncState) => Props
      protected def renderImpl: Props => VdomNode
      protected def changeImpl: Props => Editor.Change[Change]

      final override def render(p: Permission, as: AsyncState, args: Args): Option[VdomNode] =
        // Looks like this could block async but not so. Can't go from edit -> async -> notAllowed.
        // Unsafety is allowed here because EditorInstance is never Reusable
        p match {
          case Allow => Some(renderImpl(props(args, as)))
          case Deny  => None
        }

      final override def change[C >: Change](args: Args): Editor.Change[C] =
        changeImpl(props(args, None))
    }

    def init[FieldArgs, Change, A](pvaCB: CallbackTo[PotentialValueAcceptor[A]])
                                  (userInit: Option[A] => Init[FieldArgs, Change]): Init[FieldArgs, Change] = args => {

      args.potentialValue match {

        case None =>
          userInit(None)(args)

        case Some(pv) =>
          for {
            pva <- pvaCB.toCBO
            a   <- CallbackOption.liftOption(pva.accept(pv)) // halt here if PotentialValueAcceptor rejects value
            e   <- userInit(Some(a))(args)
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
        allTags         = _ => EditTags.allTags(r.id),
        otherTags       = _ => EditTags.otherTags(r.id),
        customFieldTags = f => EditTags.customField(r.id, f.field),
        title           = f => EditRichText.GenericReqTitle(r.id, PreviewId(r, f)))

      def prepareUC(r: RowKey.UseCase) = FieldKey.FoldForUseCase[LogicPerField](
        codes           = _ => EditReqCodes.Multiple(r.id),
        customTextField = f => EditRichText.CustomTextField(r.id, f.field, PreviewId(r, f)),
        implications    = f => EditImplications(r.id, f.scope),
        allTags         = _ => EditTags.allTags(r.id),
        otherTags       = _ => EditTags.otherTags(r.id),
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

      def abort(hooks: Hooks, previewId: Option[PreviewId]): Callback = {
        val onClose: Callback =
          Callback.traverseOption(previewId)(previewW(_).clear) >> asyncFeature.clearAsyncStatus >> hooks.onClose
        stateAccess.setState(None, onClose)
      }

      def commit[Cmd](ssp: ServerSideProcInvoker[Cmd, ErrorMsg, Any])
                     (cmd: Cmd, hooks: Hooks, previewId: Option[PreviewId]): Callback =
        asyncFeature(
          ssp(cmd).rightFlatTap(_ => abort(hooks, previewId).asAsyncCallback)
        )

      def makeAbortCommitFn[Cmd, B](ssp: ServerSideProcInvoker[Cmd, ErrorMsg, Any])
                                   (cmd: B => Cmd, hooks: Hooks, previewId: Option[PreviewId]): (Some[Callback], Some[B => Callback]) =
        (
          Some(abort(hooks, previewId)),
          Some(v => commit(ssp)(cmd(v), hooks, previewId))
        )

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
      pxProject.toCallback.map(_.content.reqs.genericReqs.imap.get(id)).asCBO

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

    final val abortVerb = EditControlsFeature.defaultAbortVerb
    final val commitVerb = EditControlsFeature.defaultCommitVerb

    trait ForChangeType {
      type Args
      type Change
      final type EditorImpl = Internal.EditorImpl[Args, Change]
      final type Init       = Internal.Init[Args, Change]
      final type InitFn     = InternalCtx[Args, Change] => Init
      final type SetStateFn = japgolly.scalajs.react.SetStateFn[CallbackTo, State.ForEditor[Args, Change]]
    }

    def newPropsMemo[I, P](f: I => P)(implicit r: Reusability[I]): I => P =
      LruMemo(f, 4).byReusability

    def setPotentialValueStd[A](pvaCBO: CallbackOption[PotentialValueAcceptor[A]])
                               (p: PotentialValue,
                                ss: StateSnapshot[A]): CallbackOption[Unit] =
      for {
        pva <- pvaCBO
        v   <- CallbackOption.liftOption(pva.accept(p))
        _   <- ss.setState(v).toCBO
      } yield ()

    // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████
    object EditReqType extends ForChangeType { base =>
      import shipreq.webapp.client.project.widgets.editors_with_controls.ReqTypeSelector
      import ReqTypeSelector.RT

      override type Args   = EditorArgs.ForReqTypeEditor
      override type Change = CustomReqType
      type Props           = ReqTypeSelector.Props
      type PropsInputs     = (StateSnapshot[RT], Args, AsyncState)
      type InitialValue    = RT

      def apply(id: GenericReqId): InitFn = ictx => args => {
        import ictx._, ctx._

        val (abort, commitFn) =
          makeAbortCommitFn(sspUpdateContent)((t: RT) => UpdateContentCmd.SetGenericReqType(id, t.id), args.hooks, None)

        def getInitialValue(current: RT): CallbackOption[InitialValue] =
          args.potentialValue match {
            case None =>
              CallbackOption.pure(current)

            case Some(pv) =>
              for {
                p      <- pxProject.toCallback.toCBO
                choices = ReqTypeSelector.choices(current, p.config.reqTypes)
                pva     = ReqTypeSelector.potentialValueAcceptor(choices.whole)
                i      <- CallbackOption.liftOption(pva.accept(pv))
              } yield i
          }

        val initialValueCBO: CallbackOption[RT] =
          for {
            req     <- getGenericReq(id)
            current <- getCustomReqTypeCB(req.reqTypeId)
            initial <- getInitialValue(current)
          } yield initial

        for {
          initialValue <- initialValueCBO
        } yield {

          val someInitialValue = Some(initialValue)

          val propsMemo =
            newPropsMemo[PropsInputs, Props] { in =>
              val (ss, args, asyncState) = in
              ReqTypeSelector.Props(
                initialValue = someInitialValue,
                edit         = ss,
                choices      = ReqTypeSelector.choices(initialValue, args.reqTypes),
                asyncStatus  = EditorStatus.async(asyncState),
                abort        = abort,
                commitFn     = commitFn)
            }

          val pxChoices: Px[NonEmptySet[RT]] =
            pxProject.map(p => ReqTypeSelector.choices(initialValue, p.config.reqTypes))

          State(initialValue, stateAccess.toSetStateFn, propsMemo, pxChoices)
        }
      }

      private case class State(editValue : RT,
                               setStateFn: SetStateFn,
                               propsMemo : PropsInputs => Props,
                               pxChoices : Px[NonEmptySet[RT]]) extends EditorImpl {

        @elidable(elidable.FINER)
        override def toString = s"EditReqType(${ss.value})"

        val ss = StateSnapshot(editValue)(setStateFn.contramap(e => copy(editValue = e).some))

        override type Props     = base.Props
        override def renderImpl = _.render
        override def changeImpl = _.change
        override val props      = (args, asyncState) => propsMemo((ss, args, asyncState))

        override def clipboardData: Option[ClipboardData] =
          Some(ClipboardData(PlainText.reqTypeFull(ss.value)))

        override def setPotentialValue(p: PotentialValue): CallbackOption[Unit] = {
          val pvaCBO = pxChoices.toCallback.toCBO.map(choices => ReqTypeSelector.potentialValueAcceptor(choices.whole))
          setPotentialValueStd(pvaCBO)(p, ss)
        }
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditReqCodes {
      import shipreq.webapp.client.project.widgets.editors_with_controls.ReqCodeEditor

      object Multiple extends ForChangeType { base =>
        import ReqCodeEditor.{Multiple => RCE}

        private val potentialValueAcceptor = CallbackTo(RCE.potentialValueAcceptor)

        override type Args   = EditorArgs.ForReqCodeEditor
        override type Change = RCE.Output
        type Props           = RCE.Props
        type PropsInputs     = (StateSnapshot[String], Args, AsyncState)
        type InitialValues   = Set[ReqCode.Value]

        def apply(id: ReqId): InitFn = ictx => Internal.init(potentialValueAcceptor) { ivo => args =>
          import ictx._

          val initialValuesCB: CallbackTo[InitialValues] =
            pxProject.toCallback.map(_.content.reqCodes.activeReqCodesByReqId(id))

          val (abort, commitFn) =
            makeAbortCommitFn(sspUpdateContent)(UpdateContentCmd.PatchReqCodes(id, _), args.hooks, None)

          val initEditor: InitialValues => StateSnapshot[String] => EditorImpl =
            initialValue => {

              val someInitialValue = Some(initialValue)

              val propsMemo =
                newPropsMemo[PropsInputs, Props] { in =>
                  val (ss, args, asyncState) = in
                  RCE.Props(
                    edit             = ss,
                    initialValue     = someInitialValue,
                    trie             = args.trie,
                    asyncStatus      = EditorStatus.async(asyncState),
                    abort            = abort,
                    abortVerb        = abortVerb,
                    autoFocus        = args.autoFocus,
                    commitFn         = commitFn,
                    commitVerb       = commitVerb,
                    extraControls    = EditControlsFeature.ExtraControls.empty,
                    showInstructions = true)
                }

              new State(_, propsMemo)
            }

          startWithStateSnapshot(
            initialData       = initialValuesCB.toCBO,
            initalValueOption = ivo)(
            initialValueFn    = ReqCodeEditor.Multiple.seqFmt merge _.toVector.map(PlainText.reqCode).sorted)(
            editor            = initEditor)
        }

        private final class State(ss       : StateSnapshot[String],
                                  propsMemo: PropsInputs => Props) extends EditorImpl {

          override type Props     = base.Props
          override def renderImpl = _.render
          override def changeImpl = _.validated
          override val props      = (args, asyncState) => propsMemo((ss, args, asyncState))

          override def clipboardData: Option[ClipboardData] =
            Some(ClipboardData(ss.value))

          override def setPotentialValue(p: PotentialValue): CallbackOption[Unit] =
            setPotentialValueStd(potentialValueAcceptor.toCBO)(p, ss)
        }
      }

      object Single extends ForChangeType { base =>
        import ReqCodeEditor.{Single => RCE}

        private val potentialValueAcceptor = CallbackTo(RCE.potentialValueAcceptor)

        override type Args   = EditorArgs.ForReqCodeEditor
        override type Change = RCE.Output
        type Props           = RCE.Props
        type PropsInputs     = (StateSnapshot[String], Args, AsyncState)
        type InitialValues   = ReqCode.Value

        def apply(id: ReqCodeGroupId): InitFn = ictx => Internal.init(potentialValueAcceptor) { ivo => args =>
          import ictx._

          val initialValueCB: CallbackOption[InitialValues] =
            pxProject.toCallback.map(_.content.reqCodes.reqCode(id)).toCBO

          val (abort, commitFn) =
            makeAbortCommitFn(sspUpdateContent)(UpdateContentCmd.SetCodeGroupCode(id, _), args.hooks, None)

          val initEditor: InitialValues => StateSnapshot[String] => EditorImpl =
            initialValue => {

              val someInitialValue = Some(initialValue)

              val propsMemo =
                newPropsMemo[PropsInputs, Props] { in =>
                  val (ss, args, asyncState) = in
                  RCE.Props(
                    edit             = ss,
                    initialValue     = someInitialValue,
                    trie             = args.trie,
                    asyncStatus      = EditorStatus.async(asyncState),
                    abort            = abort,
                    abortVerb        = abortVerb,
                    autoFocus        = args.autoFocus,
                    commitFn         = commitFn,
                    commitVerb       = commitVerb,
                    extraControls    = EditControlsFeature.ExtraControls.empty,
                    showInstructions = true)
                }

              new State(_, propsMemo)
            }

          startWithStateSnapshot(
            initialData       = initialValueCB,
            initalValueOption = ivo)(
            initialValueFn    = PlainText.reqCode)(
            editor            = initEditor)
        }

        private final class State(ss      : StateSnapshot[String],
                                  propsMemo: PropsInputs => Props) extends EditorImpl {

          @elidable(elidable.FINER)
          override def toString = s"EditReqCodes(${ss.value})"

          override type Props     = base.Props
          override def renderImpl = _.render
          override def changeImpl = _.validated
          override val props      = (args, asyncState) => propsMemo((ss, args, asyncState))

          override def clipboardData: Option[ClipboardData] =
            Some(ClipboardData(ss.value))

          override def setPotentialValue(p: PotentialValue): CallbackOption[Unit] =
            setPotentialValueStd(potentialValueAcceptor.toCBO)(p, ss)
        }
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditImplications extends ForChangeType { base =>
      import shipreq.webapp.client.project.widgets.editors_with_controls.ImplicationEditor
      import ImplicationEditor.{Lookup, ValidationFn}

      override type Args   = EditorArgs.ForImplicationEditor
      override type Change = ImplicationEditor.Output
      type Props           = ImplicationEditor.Props
      type PropsInputs     = (StateSnapshot[String], Args, AsyncState)
      type LookupFn        = (Project, PlainText.ForProject.AnyCtx) => Lookup
      type InitialValues   = (Set[ReqId], String)

      def apply(id: ReqId, scope: ImplicationScope): InitFn =
        scope.fold(customField(id, _), all(id, _))

      def all(id: ReqId, dir: Direction): InitFn =
        start(id, dir, ImplicationEditor.Lookup.allExcept(id))

      def customField(id: ReqId, fid: CustomField.Implication.Id): InitFn = {
        val dir = CustomField.Implication.dir
        val lookupFn: LookupFn =
          (p, pt) => {
            val all = ImplicationEditor.Lookup.allExcept(id)(p, pt)
            ImplicationEditor.Lookup.forCustomColumn(p, all, fid)
          }
        start(id, dir, lookupFn)
      }

      private def start(id: ReqId, dir: Direction, lookupFn: LookupFn): InitFn = ictx => {
        import ictx._

        val lookupFnMemo: LookupFn =
          LastValueMemo(lookupFn.tupled).toFn2

        val valFnMemo: ((Project, InitialValues)) => ValidationFn =
          LastValueMemo(x => ValidationFn(x._1, Some(id), x._2._1, dir))

        val pxInitialValues =
          pxProject.map(p => ImplicationEditor.initialValue(p, dir, id))

        val pxLookup: Px[Lookup] =
          for {
            project       <- pxProject
            plainText     <- pxPlainTextNoCtx
          } yield lookupFnMemo(project, plainText)

        val pxInit: Px[InitialValues] =
          for {
            project       <- pxProject
            lookup        <- pxLookup
            initialValues <- pxInitialValues
          } yield ImplicationEditor.initialValueAndText((id, initialValues).some, project, lookup)

        val pxValFn: Px[ValidationFn] =
          for {
            project <- pxProject
            init    <- pxInit
          } yield valFnMemo((project, init))

        val pxCorrector: Px[String => String] =
          for {
            valFn  <- pxValFn
            lookup <- pxLookup
          } yield valFn(lookup).corrector.live

        val pvaCB: CallbackTo[PotentialValueAcceptor[String]] =
          pxCorrector.toCallback.map(PotentialValueAcceptor.correct)

        Internal.init(pvaCB) { ivo => args =>

          val (abort, commitFn) =
            makeAbortCommitFn(sspUpdateContent)(UpdateContentCmd.PatchImplications(id, dir, _), args.hooks, None)

          val initEditor: InitialValues => StateSnapshot[String] => EditorImpl =
            initialValues => {

              val propsMemo =
                newPropsMemo[PropsInputs, Props] { in =>
                  val (ss, args, asyncState) = in
                  ImplicationEditor.Props(
                    edit             = ss,
                    lookup           = lookupFnMemo(args.project, args.plainText),
                    validationFn     = valFnMemo((args.project, initialValues)),
                    asyncStatus      = EditorStatus.async(asyncState),
                    abort            = abort,
                    abortVerb        = abortVerb,
                    autoFocus        = args.autoFocus,
                    commitFn         = commitFn,
                    commitVerb       = commitVerb,
                    textSearch       = args.textSearch,
                    extraControls    = EditControlsFeature.ExtraControls.empty,
                    showInstructions = true)
                }

              new State(_, propsMemo, pvaCB.toCBO)
            }

          startWithStateSnapshot(
            initialData       = pxInit.toCallback.toCBO,
            initalValueOption = ivo)(
            initialValueFn    = _._2)(
            editor            = initEditor)
        }
      }

      private class State(ss         : StateSnapshot[String],
                          propsMemo  : PropsInputs => Props,
                          pvaCBO     : CallbackOption[PotentialValueAcceptor[String]]) extends EditorImpl {

        @elidable(elidable.FINER)
        override def toString = s"EditImplications(${ss.value})"

        override type Props     = base.Props
        override def renderImpl = _.render
        override def changeImpl = _.validated
        override val props      = (args, asyncState) => propsMemo((ss, args, asyncState))

        override def clipboardData: Option[ClipboardData] =
          Some(ClipboardData(ss.value))

        override def setPotentialValue(p: PotentialValue): CallbackOption[Unit] =
          setPotentialValueStd(pvaCBO)(p, ss)
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditTags extends ForChangeType { base =>
      import shipreq.webapp.client.project.widgets.editors_with_controls.TagEditor
      import TagEditor.Lookup

      override type Args   = EditorArgs.ForTagEditor
      override type Change = TagEditor.Output
      type Props           = TagEditor.Props
      type PropsInputs     = (StateSnapshot[String], Args, AsyncState)
      type LookupFn        = Project => Lookup
      type InitialValues   = (Set[ApplicableTagId], String)

      private val potentialValueAcceptor = CallbackTo.pure(TagEditor.potentialValueAcceptor)

      def allTags(id: ReqId): InitFn =
        apply(id, Lookup.all)

      def otherTags(id: ReqId): InitFn =
        apply(id, Lookup.notUsedInTagFields)

      def customField(id: ReqId, fid: CustomField.Tag.Id): InitFn =
        apply(id, Lookup.forTagField(fid))

      def apply(id: ReqId, lookupFn: LookupFn): InitFn = ictx => {
        import ictx._

        val lookupFnMemo: Project => Lookup =
          LastValueMemo(lookupFn)

        val initialValueCB: CallbackTo[InitialValues] =
          for {
            p <- pxProject.toCallback
          } yield {
            val lookup = lookupFnMemo(p)
            val naTags = p.naTagsForReq(id)
            TagEditor.initialValues(p.content.reqTags(id), p.config, lookup, naTags)
          }

        Internal.init(potentialValueAcceptor) { ivo =>args =>

          val (abort, commitFn) =
            makeAbortCommitFn(sspUpdateContent)(UpdateContentCmd.PatchReqTags(id, _), args.hooks, None)

          val initEditor: InitialValues => StateSnapshot[String] => EditorImpl =
            initialValues => {

              val initialValue = Some(initialValues._1)

              val propsMemo =
                newPropsMemo[PropsInputs, Props] { in =>
                  val (ss, args, asyncState) = in
                  TagEditor.Props(
                    preEditValue     = initialValue,
                    naTags           = args.project.naTagsForReq(id),
                    edit             = ss,
                    lookup           = lookupFnMemo(args.project),
                    asyncStatus      = EditorStatus.async(asyncState),
                    abort            = abort,
                    abortVerb        = abortVerb,
                    autoFocus        = args.autoFocus,
                    commitFn         = commitFn,
                    commitVerb       = commitVerb,
                    extraControls    = EditControlsFeature.ExtraControls.empty,
                    showInstructions = true)
                }

              new State(_, propsMemo)
            }

          startWithStateSnapshot(
            initialData       = initialValueCB.toCBO,
            initalValueOption = ivo)(
            initialValueFn    = _._2)(
            editor            = initEditor)
        }
      }

      private class State(ss       : StateSnapshot[String],
                          propsMemo: PropsInputs => Props) extends EditorImpl {

        @elidable(elidable.FINER)
        override def toString = s"EditTags(${ss.value})"

        override type Props     = base.Props
        override def renderImpl = _.render
        override def changeImpl = _.validated
        override val props      = (args, asyncState) => propsMemo((ss, args, asyncState))

        override def clipboardData: Option[ClipboardData] =
          Some(ClipboardData(ss.value))

        override def setPotentialValue(p: PotentialValue): CallbackOption[Unit] =
          setPotentialValueStd(potentialValueAcceptor.toCBO)(p, ss)
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditRichText {
      import shipreq.webapp.member.project.text._
      import shipreq.webapp.client.project.widgets.editors_with_controls.RichTextEditor

      @inline def defaultStyle = EditControlsFeature.Style.default

      abstract class Base[T <: Text.Generic](val editor: RichTextEditor[T], allowCustomStyle: Boolean = false) extends ForChangeType { base =>
        val T: editor.text.type = editor.text

        val potentialValueAcceptor = CallbackTo.pure(editor.potentialValueAcceptor)

        override type Args   = EditorArgs.ForTextEditor
        override type Change = T.OptionalText
        type Props           = editor.Optional
        type PropsInputs     = (StateSnapshot[String], Args, AsyncState)
        type InitialValues   = (T.OptionalText, String)

        protected def start(cmd           : T.OptionalText => UpdateContentCmd,
                            initialValueCB: CallbackOption[T.OptionalText],
                            pid           : PreviewId,
                            reqId         : Option[ReqId]): InitFn = ictx => {
          import ictx._

          Internal.init(potentialValueAcceptor) { ivo => args =>

            val (abort, commitFn) =
              makeAbortCommitFn(sspUpdateContent)(cmd, args.hooks, Some(pid))

            val initCBO: CallbackOption[InitialValues] =
              for {
                initialValue   <- initialValueCB
                projectWidgets <- args.cbProjectWidgets.toCBO
              } yield {
                val initialText = projectWidgets.plainText.text(initialValue, RichTextEditor.hardcodedLive, Optional)
                (initialValue, initialText)
              }

            val initEditor: InitialValues => StateSnapshot[String] => EditorImpl =
              initialValues => {

                val initialValue = Some(initialValues._1)

                val propsMemo =
                  newPropsMemo[PropsInputs, Props] { in =>
                    val (ss, args, asyncState) = in
                    editor.Optional(
                      project            = args.project,
                      naTags             = args.project.naTagsForReq(reqId),
                      plainTextNoCtx     = args.plainTextNoCtx,
                      textSearch         = args.textSearch,
                      projectWidgets     = args.projectWidgets,
                      edit               = ss,
                      asyncStatus        = EditorStatus.async(asyncState),
                      abort              = abort,
                      abortVerb          = abortVerb,
                      abortConfirmation  = someConfirmJs,
                      autoFocus          = args.autoFocus,
                      commitFn           = commitFn,
                      commitVerb         = commitVerb,
                      editorStyle        = if (allowCustomStyle) args.style else defaultStyle,
                      preview            = args.previewRW(pid),
                      preEditValue       = initialValue,
                      extraControls      = EditControlsFeature.ExtraControls.empty,
                      showInstructions   = true,
                      optionalFullscreen = someOptionalFullscreen)
                  }

                new State(_, propsMemo)
              }

            startWithStateSnapshot(
              initialData       = initCBO,
              initalValueOption = ivo)(
              initialValueFn    = _._2)(
              editor            = initEditor)
          }
        }

        private class State(ss       : StateSnapshot[String],
                            propsMemo: PropsInputs => Props) extends EditorImpl {

          @elidable(elidable.FINER)
          override def toString = s"EditRichText(${ss.value})"

          override type Props     = base.Props
          override def renderImpl = _.render
          override def changeImpl = _.validated
          override val props      = (args, asyncState) => propsMemo((ss, args, asyncState))

          override def clipboardData: Option[ClipboardData] =
            Some(ClipboardData(ss.value))

          override def setPotentialValue(p: PotentialValue): CallbackOption[Unit] =
            setPotentialValueStd(potentialValueAcceptor.toCBO)(p, ss)
        }
      }

      object CodeGroupTitle extends Base(RichTextEditor.CodeGroupTitle) {
        def apply(id: ReqCodeGroupId, pid: PreviewId): InitFn = start(
          cmd            = UpdateContentCmd.SetCodeGroupTitle(id, _),
          initialValueCB = getCodeGroup(id).map(_.title).widen,
          pid            = pid,
          reqId          = None)
      }

      object CustomTextField extends Base(RichTextEditor.CustomTextField, allowCustomStyle = true) {
        def apply(id: ReqId, fid: CustomField.Text.Id, pid: PreviewId): InitFn = start(
          cmd            = UpdateContentCmd.SetCustomTextField(id, fid, _),
          initialValueCB = pxProject.toCallback.map(p => ReqData.Text.at(fid, id).get(p.content.reqText)).toCBO,
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
      import shipreq.webapp.member.project.text._
      import shipreq.webapp.client.project.widgets.editors_with_controls.RichTextEditor

      @inline def defaultStyle = EditControlsFeature.Style.default

      abstract class Base[T <: Text.Generic, Cmd](val editor: RichTextEditor[T],
                                                  ssp: ServerSideProcInvoker[Cmd, ErrorMsg, Any],
                                                  allowCustomStyle: Boolean = false) extends ForChangeType { base =>

        val T: editor.text.type = editor.text

        val potentialValueAcceptor = CallbackTo.pure(editor.potentialValueAcceptor)

        override type Args   = EditorArgs.ForTextEditor
        override type Change = T.NonEmptyText
        type Props           = editor.NonEmpty
        type PropsInputs     = (StateSnapshot[String], Args, AsyncState)
        type InitialValues   = (T.NonEmptyText, String)

        protected def start(cmd           : T.NonEmptyText => Cmd,
                            initialValueCB: CallbackOption[T.NonEmptyText],
                            pid           : PreviewId,
                            reqId         : Option[ReqId]): InitFn = ictx => {
          import ictx._

          Internal.init(potentialValueAcceptor) { ivo => args =>

            val (abort, commitFn) =
              makeAbortCommitFn(ssp)(cmd, args.hooks, Some(pid))

            val initCBO: CallbackOption[InitialValues] =
              for {
                initialValue   <- initialValueCB
                projectWidgets <- args.cbProjectWidgets.toCBO
              } yield {
                val initialText = projectWidgets.plainText.text(initialValue.whole, RichTextEditor.hardcodedLive, Mandatory)
                (initialValue, initialText)
              }

            val initEditor: InitialValues => StateSnapshot[String] => EditorImpl =
              initialValues => {

                val initialValue = Some(initialValues._1)

                val propsMemo =
                  newPropsMemo[PropsInputs, Props] { in =>
                    val (ss, args, asyncState) = in
                    editor.NonEmpty(
                      project            = args.project,
                      naTags             = args.project.naTagsForReq(reqId),
                      plainTextNoCtx     = args.plainTextNoCtx,
                      textSearch         = args.textSearch,
                      projectWidgets     = args.projectWidgets,
                      edit               = ss,
                      asyncStatus        = EditorStatus.async(asyncState),
                      abort              = abort,
                      abortVerb          = abortVerb,
                      abortConfirmation  = someConfirmJs,
                      autoFocus          = args.autoFocus,
                      commitFn           = commitFn,
                      commitVerb         = commitVerb,
                      editorStyle        = if (allowCustomStyle) args.style else defaultStyle,
                      preview            = args.previewRW(pid),
                      preEditValue       = initialValue,
                      extraControls      = EditControlsFeature.ExtraControls.empty,
                      showInstructions   = true,
                      optionalFullscreen = someOptionalFullscreen)
                  }

                new State(_, propsMemo)
              }

            startWithStateSnapshot(
              initialData       = initCBO,
              initalValueOption = ivo)(
              initialValueFn    = _._2)(
              editor            = initEditor)
          }
        }

        private class State(ss       : StateSnapshot[String],
                            propsMemo: PropsInputs => Props) extends EditorImpl {

          @elidable(elidable.FINER)
          override def toString = s"EditRichTextNonEmpty(${ss.value})"

          override type Props     = base.Props
          override def renderImpl = _.render
          override def changeImpl = _.validated
          override val props      = (args, asyncState) => propsMemo((ss, args, asyncState))

          override def clipboardData: Option[ClipboardData] =
            Some(ClipboardData(ss.value))

          override def setPotentialValue(p: PotentialValue): CallbackOption[Unit] =
            setPotentialValueStd(potentialValueAcceptor.toCBO)(p, ss)
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
    object EditUseCaseStep extends ForChangeType { base =>
      import shipreq.webapp.client.project.widgets.editors_with_controls.RichTextEditor.hardcodedLive
      import shipreq.webapp.client.project.widgets.editors_with_controls.UseCaseStepEditor
      import UseCaseStepFlowText.TextAndFlow

      val potentialValueAcceptor = CallbackTo.pure(UseCaseStepEditor.potentialValueAcceptor)

      override type Args   = EditorArgs.ForUseCaseStepEditor
      override type Change = UseCaseStepGD.NonEmptyValues
      type Props           = UseCaseStepEditor.Props
      type PropsInputs     = (StateSnapshot[String], Args, AsyncState)
      type InitialValues   = (UseCaseStepEditor.InitialValue, String)

      def apply(id: UseCaseStepId, pid: PreviewId): InitFn = ictx => {
        import ictx._

        Internal.init(potentialValueAcceptor) { ivo => args =>

          val abortCB = abort(args.hooks, Some(pid))

          val commitFn: UseCaseStepEditor.CommitFn =
            // Below you'll see that we're filtering flow to create visibleFlow
            // There's no need to re-insert invisibleFlow here because flow is passed as a SetDiff
            // meaning that regardless of what the actual flow is, we only send what the user changes.
            Reusable.fn(v => commit(sspUpdateContent)(UpdateContentCmd.UpdateUseCaseStep(id, v), args.hooks, Some(pid)))

          def getStepFocus(p: Project) =
            p.content.reqs.useCases.focusStep(id)

          val initCB: CallbackTo[InitialValues] =
            for {
              projectWidgets <- args.pxProjectWidgets.value.toCallback
              project        <- pxProject.toCallback
            } yield {
              val stepFocus = getStepFocus(project)
              val visibleFlow = args.filterDead match {
                case HideDead => Direction.Values(stepFocus.flow(_, Live))
                case ShowDead => Direction.Values(stepFocus.flow)
              }
              val initialValue = TextAndFlow(stepFocus.step.titleExplicitly, visibleFlow)
              val initialText = projectWidgets.plainText.useCaseStepTextAndFlow(initialValue, hardcodedLive)
              (initialValue, initialText)
            }

          val initEditor: InitialValues => StateSnapshot[String] => EditorImpl =
            initialValues => {

              val initialValue = Some(initialValues._1)

              val propsMemo =
                newPropsMemo[PropsInputs, Props] { in =>
                  val (ss, args, asyncState) = in

                  val step = getStepFocus(args.project)

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
                    project        = args.project,
                    plainTextNoCtx = args.plainTextNoCtx,
                    textSearch     = args.textSearch,
                    projectWidgets = args.projectWidgets,
                    edit           = ss,
                    asyncStatus    = EditorStatus.async(asyncState),
                    abort          = abortCB,
                    commit         = commitFn,
                    shiftRunner    = shiftRunner,
                    addStepRunner  = addStepRunner,
                    preview        = args.previewRW(pid),
                    preEditValue   = initialValue)
                }

              new State(_, propsMemo)
            }

          startWithStateSnapshot(
            initialData       = initCB.toCBO,
            initalValueOption = ivo)(
            initialValueFn    = _._2)(
            editor            = initEditor)
        }
      }

      private class State(ss       : StateSnapshot[String],
                          propsMemo: PropsInputs => Props) extends EditorImpl {

        @elidable(elidable.FINER)
        override def toString = s"EditUseCaseStep(${ss.value})"

        override type Props     = base.Props
        override def renderImpl = _.render
        override def changeImpl = _.validatedChanges
        override val props      = (args, asyncState) => propsMemo((ss, args, asyncState))

        override def clipboardData: Option[ClipboardData] =
          Some(ClipboardData(ss.value))

        override def setPotentialValue(p: PotentialValue): CallbackOption[Unit] =
          setPotentialValueStd(potentialValueAcceptor.toCBO)(p, ss)
      }
    }

  } // Internal
}
