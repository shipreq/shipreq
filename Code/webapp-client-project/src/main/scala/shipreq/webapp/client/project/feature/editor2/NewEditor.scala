package shipreq.webapp.client.project.feature.editor2

import japgolly.microlibs.nonempty._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
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
import shipreq.webapp.base.validation.Simple.Invalidity

/** Interface to start a new editor.
  *
  * Doesn't perform ANY applicability checks.
  *
  * The input to [[create]] is a Callback to invoke after the editor opens.
  */
final case class NewEditor(create: Callback => Callback) extends AnyVal

object NewEditor {

  final case class Static(previewFeature  : PreviewFeature.Write.Composite[PreviewId],
                          pxProject       : Px[Project],
                          pxPlainText     : Px[PlainText.ForProject],
                          pxProjectWidgets: Px[ProjectWidgets],
                          pxTextSearch    : Px[TextSearch],
                          saveIO          : ServerCall[UpdateContentCmd])

  final case class Ctx(static      : Static,
                       stateAccess : StateAccessPure[State.ForCell],
                       asyncFeature: AsyncFeature.Write.D0[AsyncError])

  type PrepareFn = Ctx => NewEditor

  def prepare(rowKey: RowKey): rowKey.FieldKey => PrepareFn =
    toPrepareFn compose rowKey.fold[? => InternalStartFn](
      codeGroup    = prepareForCodeGroup,
      genericReq   = prepareForGenericReq,
      useCase      = prepareForUseCase,
      useCaseSteps = prepareForUseCaseSteps)

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  private type StartFn = CallbackOption[Editor]
  private type InternalStartFn = Internal => StartFn

  private val toPrepareFn: InternalStartFn => PrepareFn =
    start => ctx => {
      val i = new Internal(ctx)
      i.newEditor(start(i))
    }

  private val prepareForCodeGroup: RowKey.CodeGroup => FieldKey.ForCodeGroup => InternalStartFn = r => {
    case    FieldKey.Code           => _.EditReqCodes.group(r.id)
    case f@ FieldKey.CodeGroupTitle => _.EditRichText.CodeGroupTitle(r.id, PreviewId(r, f))
  }

  private val prepareForGenericReq: RowKey.GenericReq => FieldKey.ForGenericReq => InternalStartFn = r => {
    case f: FieldKey.CustomTextField     => _.EditRichText.CustomTextField(r.id, f.field, PreviewId(r, f))
    case    FieldKey.Codes               => _.EditReqCodes.req(r.id)
    case f@ FieldKey.GenericReqTitle     => _.EditRichText.GenericReqTitle(r.id, PreviewId(r, f))
    case    FieldKey.Implications(scope) => _.EditImplications(r.id, scope)
    case    FieldKey.ReqType             => _.EditReqType(r.id)
    case    FieldKey.Tags(field)         => _.EditTags(r.id, field)
  }

  private val prepareForUseCase: RowKey.UseCase => FieldKey.ForUseCase => InternalStartFn = r => {
    case f: FieldKey.CustomTextField     => _.EditRichText.CustomTextField(r.id, f.field, PreviewId(r, f))
    case    FieldKey.Codes               => _.EditReqCodes.req(r.id)
    case    FieldKey.Implications(scope) => _.EditImplications(r.id, scope)
    case    FieldKey.Tags(field)         => _.EditTags(r.id, field)
    case f@ FieldKey.UseCaseTitle        => _.EditRichText.UseCaseTitle(r.id, PreviewId(r, f))
  }

  private val prepareForUseCaseSteps: FieldKey.UseCaseStep => InternalStartFn =
    f => _.EditUseCaseStep(f.id, PreviewId(RowKey.UseCaseSteps, f))

  private trait EditorImpl extends Editor {
    protected type Props
    protected val props: AsyncState => CallbackTo[Props]
    protected def renderImpl: Props => VdomElement
    protected def changeImpl: Props => PotentialChange[Invalidity, Change]

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

  private class Internal(ctx: Ctx) {
    import ctx._
    import static._

    def abort: Callback =
      stateAccess.setState(None)

    def commit(cmd: UpdateContentCmd): Callback =
      asyncFeature((s, f) => saveIO(cmd, s >> abort, f))

    def makeAbortCommit[A](cmd: A => UpdateContentCmd): Some[AbortCommit[Callback, A ~=> Callback]] =
      Some(AbortCommit(abort, Reusable.fn(v => commit(cmd(v)))))

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

    /** Creates a Callback that when invoked, will initialise and start an editor.
      *
      * @tparam S Initial data. Data captured before starting the editor.
      * @tparam A The initial value of the editor.
      * @tparam E The editor
      */
    def startWithStateSnapshot[S, A: Reusability, E <: Editor](initialData: CallbackOption[S])
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

    def newEditor(startFn: => StartFn): NewEditor =
      NewEditor(cb => startFn.get.flatMap(stateAccess.setState(_, cb)))

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditReqCodes {
      import shipreq.webapp.client.project.widgets.ReqCodeEditor

      private def trieCB: CallbackTo[ReqCode.Trie] =
        pxProject.toCallback.map(_.reqCodes.trie)

      def req(id: ReqId): StartFn = {
        val initialValuesCB: CallbackTo[Set[ReqCode.Value]] =
          pxProject.toCallback.map(_.reqCodes.activeReqCodesByReqId(id))

        val abortCommit: ReqCodeEditor.Multiple.AbortCommit =
          makeAbortCommit(UpdateContentCmd.PatchReqCodes(id, _))

        startWithStateSnapshot(
          initialValuesCB.toCBO)(
          ReqCodeEditor.Multiple.seqFmt merge _.toVector.map(PlainText.reqCode).sorted)(
          initialValues => new StateMultiple(_, Some(initialValues), abortCommit))
      }

      private class StateMultiple(ss         : StateSnapshot[String],
                                  initial    : Some[Set[ReqCode.Value]],
                                  abortCommit: ReqCodeEditor.Multiple.AbortCommit) extends EditorImpl {

        override type Change = ReqCodeEditor.Multiple.Output
        override type Props = ReqCodeEditor.Multiple.Props
        override def renderImpl = _.render
        override def changeImpl = _.validated
        override val props = as =>
          for {
            trie <- trieCB
          } yield ReqCodeEditor.Multiple.Props(
            ss,
            initial,
            trie,
            EditorStatus.async(as),
            abortCommit)
      }

      def group(id: ReqCodeId): StartFn = {
        val initialValueCB: CallbackOption[ReqCode.Value] =
          pxProject.toCallback.map(_.reqCodes.reqCode(id)).toCBO

        val abortCommit: ReqCodeEditor.Single.AbortCommit =
          makeAbortCommit(UpdateContentCmd.SetCodeGroupCode(id, _))

        startWithStateSnapshot(initialValueCB)(PlainText.reqCode)(
          i => new StateSingle(_, Some(i), abortCommit))
      }

      private class StateSingle(ss         : StateSnapshot[String],
                                initial    : Some[ReqCode.Value],
                                abortCommit: ReqCodeEditor.Single.AbortCommit) extends EditorImpl {

        override type Change = ReqCodeEditor.Single.Output
        override type Props = ReqCodeEditor.Single.Props
        override def renderImpl = _.render
        override def changeImpl = _.validated
        override val props = as =>
          for {
            trie <- trieCB
          } yield ReqCodeEditor.Single.Props(
            ss,
            initial,
            trie,
            EditorStatus.async(as),
            abortCommit)
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditReqType {
      import shipreq.webapp.client.project.widgets.ReqTypeSelector
      import ReqTypeSelector.RT

      val pxCustomReqTypes = ReqTypeSelector.pxCustomReqTypes(pxProject)

      def apply(id: GenericReqId): StartFn = {
        val abortCommit: ReqTypeSelector.AbortCommit =
          makeAbortCommit[RT](t => UpdateContentCmd.SetGenericReqType(id, t.id)).value

        val initialCB: CallbackOption[RT] =
          pxProject.toCallback.map(p =>
            p.reqs.get(id).flatMap(req =>
              p.config.reqTypes.custom.get(req.reqTypeId)
            )
          ).asCBO

        initialCB.map { initial =>
          val pxChoices = ReqTypeSelector.pxChoices(initial, pxCustomReqTypes)
          State(initial, initial, pxChoices, abortCommit)
        }
      }

      private case class State(initialValue: RT,
                               editValue   : RT,
                               pxChoices   : Px[NonEmptySet[RT]],
                               abortCommit : ReqTypeSelector.AbortCommit) extends EditorImpl {

        def ss = StateSnapshot(editValue)(e => stateAccess.setState(copy(editValue = e).some))

        override type Change = CustomReqType
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
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditImplications {
      import shipreq.webapp.client.project.widgets.ImplicationEditor
      import ImplicationEditor.{Lookup, ValidationFn}

      val pxLookupAll = Px.apply2(pxProject, pxPlainText)(ImplicationEditor.Lookup.all)

      def apply(id: ReqId, scope: ImplicationScope): StartFn =
        scope.fold(customField(id, _), all(id, _))

      def all(id: ReqId, dir: Direction): StartFn =
        start(id, dir, pxLookupAll)

      def customField(id: ReqId, fid: CustomField.Implication.Id): StartFn = {
        val dir = CustomField.Implication.dir
        val lookup = Px.apply2(pxProject, pxLookupAll)(ImplicationEditor.Lookup.forCustomColumn(_, _, fid))
        start(id, dir, lookup)
      }

      private def start(id: ReqId, dir: Direction, pxLookup: Px[Lookup]): StartFn = {
        val pxPubids = pxProject.map(p => ImplicationEditor.initialValue(p, dir, id))

        val pxInit: Px[(Set[ReqId], String)] =
          for {
            project <- pxProject
            lookup <- pxLookup
            pubids <- pxPubids
          } yield ImplicationEditor.initialValueAndText((id, pubids).some, project, lookup)

        val pxValFn: Px[ValidationFn] =
          for {
            project <- pxProject
            init <- pxInit
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

        override type Change = ImplicationEditor.Output
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
            textSearch)
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditTags {
      import shipreq.webapp.client.project.widgets.TagEditor
      import TagEditor.Lookup

      def apply(id: ReqId, fid: Option[CustomField.Tag.Id]): StartFn = {
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

        override type Change = TagEditor.Output
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
            abortCommit)
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditRichText {
      import shipreq.webapp.base.text._
      import shipreq.webapp.client.project.widgets.RichTextEditor

      abstract class Base[T <: Text.Generic](val editor: RichTextEditor[T]) {
        val T: editor.text.type = editor.text

        protected def start(cmd           : T.OptionalText => UpdateContentCmd,
                            initialValueCB: CallbackOption[T.OptionalText],
                            pid           : PreviewId): StartFn = {

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

          override type Change = T.OptionalText
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
              initial)
        }
      }

      object GenericReqTitle extends Base(RichTextEditor.GenericReqTitle) {
        def apply(id: GenericReqId, pid: PreviewId): StartFn =
          start(
            UpdateContentCmd.SetGenericReqTitle(id, _),
            getGenericReq(id).map(_.title),
            pid)
      }

      object UseCaseTitle extends Base(RichTextEditor.UseCaseTitle) {
        def apply(id: UseCaseId, pid: PreviewId): StartFn =
          start(
            UpdateContentCmd.SetUseCaseTitle(id, _),
            getUseCase(id).map(_.title),
            pid)
      }

      object CodeGroupTitle extends Base(RichTextEditor.CodeGroupTitle) {
        def apply(id: ReqCodeId, pid: PreviewId): StartFn =
          start(
            UpdateContentCmd.SetCodeGroupTitle(id, _),
            getCodeGroup(id).map(_.title).widen,
            pid)
      }

      object CustomTextField extends Base(RichTextEditor.CustomTextField) {
        def apply(id: ReqId, fid: CustomField.Text.Id, pid: PreviewId): StartFn =
          start(
            UpdateContentCmd.SetCustomTextField(id, fid, _),
            pxProject.toCallback.map(p => ReqData.textAt(fid, id).get(p.reqText)).toCBO,
            pid)
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditUseCaseStep {
      import shipreq.webapp.client.project.widgets.RichTextEditor.hardcodedLive
      import shipreq.webapp.client.project.widgets.UseCaseStepEditor
      import UseCaseStepFlowText.TextAndFlow

      def apply(id: UseCaseStepId, pid: PreviewId): StartFn = {

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
          i => new State(_, Some(i._1), pid, commitFn))
      }

      private class State(ss     : StateSnapshot[String],
                          initial: Some[UseCaseStepEditor.InitialValue],
                          pid    : PreviewId,
                          commit : UseCaseStepEditor.CommitFn) extends EditorImpl {

        override type Change = UseCaseStepGD.NonEmptyValues
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

  } // end Internal

}
