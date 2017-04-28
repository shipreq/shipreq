package shipreq.webapp.client.project.feature

import japgolly.microlibs.nonempty._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Lens
import scala.annotation.elidable
import shipreq.base.util._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.base.text._
import shipreq.webapp.client.base.feature._
import shipreq.webapp.client.base.lib.AbortCommit
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.protocol.ServerCall
import shipreq.webapp.client.project.widgets.high.ProjectWidgets

/**
 * Provides optional editors for parts of a project's content.
 *
 * Usage
 * =====
 *
 * - Choose your dimensions.
 *   [[ContentEditorFeature.D1]] - Editor for a single req. (Usually, Key = req field)
 *   [[ContentEditorFeature.D2]] - Editor for a multiple reqs. (Usually, Key2 = req, Key1 = req field)
 *
 * - Add `D{1,2}.State.Simple` to your parent component's state.
 *
 * - Create an instance of `Feature` in the parent component's backend.
 *
 * - To render, apply keys from the state until you arrive at `D0.State`, then call `.render()` if available.
 *   (Pass `State.ReadOnly` to child components if needed.)
 *
 * - To start editing, apply keys from the feature until you arrive at `D0.Feature`, then call `.startEdit()`.
 *   (Pass the feature to child components if needed.)
 *
 * Sharing State
 * =============
 *
 * It may be desirable to have shared state between components (for example: `ReqTable` and `ReqDetail`).
 *
 * Say we have T as our top component, and two child components C₁ and C₂ which want to share editor state.
 *
 * - Add the highest dimension's `State` to T (eg. `TwoD.State`).
 *
 * - If the keys used by C₁ and C₂ differ, create a type K' to contain all values of both key types. Next create
 *   [[Intersection]]s to map from the top-level keys K' to each child's keys.
 */
object ContentEditorFeature {

  type AsyncError = String

  type AsyncState = AsyncFeature.ReadOnly.D0[AsyncError]

  /**
    * This is not safe for reusability because the implementation calls both `Px#value()` and `CallbackTo#runNow()`.
    */
  trait Editor {
    def render(as: AsyncState): Option[VdomElement]
  }

  object Editor {
    implicit def reusabilityEditor: Reusability[Editor] =
      Reusability.never // ∵ Editor is not safe for reusability
  }

  @inline implicit class CEFState0Ops(private val s: D0.State) extends AnyVal {
    def renderOr[A](as: AsyncState)(a: => A)(implicit ev: VdomElement => A): A =
      s.flatMap(_.render(as)).fold(a)(ev)
  }

  // ===================================================================================================================

  /**
   * Data required by all features (0D, 1D, 2D) without modification.
   *
   * @tparam S The top-most state.
   * @tparam P Preview key.
   */
  case class Static[S, P]($               : StateAccessPure[S],
                          previewFeature  : PreviewFeature.Feature.Composite[P],
                          pxProject       : Px[Project],
                          pxPlainText     : Px[PlainText.ForProject],
                          pxProjectWidgets: Px[ProjectWidgets],
                          pxTextSearch    : Px[TextSearch],
                          saveIO          : ServerCall[UpdateContentCmd])

  // ===================================================================================================================

  /**
   * ADT representing all types of fields supported by the editor.
   * Meant to be used as a key for some given content (e.g. for requirement FR-1).
   */
  sealed trait EditFieldKey
  object EditFieldKey {
    sealed trait ForReq extends EditFieldKey
    sealed trait ForReqCodeGroup extends EditFieldKey

    case object ReqType                       extends ForReq
    case object Code                          extends ForReq with ForReqCodeGroup
    case object Title                         extends ForReq with ForReqCodeGroup
    case object Tags                          extends ForReq
    case class Implications(dir: Direction)   extends ForReq
    case class CustomField(id: CustomFieldId) extends ForReq
    case class UseCaseStep(id: UseCaseStepId) extends EditFieldKey

    object Implications {
      private val memo = Direction.memo(new Implications(_))
      def apply(d: Direction): Implications = memo(d)
    }

    // DeletionReason is a bit odd in that it is append-only, not directly editable.
    // case object DeletionReason extends EditFieldKey

    @inline implicit def equalityForReq: UnivEq[ForReq] =
      UnivEq.derive

    @inline implicit def equality: UnivEq[EditFieldKey] =
      UnivEq.derive

    implicit val reusability: Reusability[EditFieldKey] =
      Reusability.byUnivEq
  }

  // ===================================================================================================================

  /** Determinations of whether or not a field is allowed to be edited.
    *
    * Each class herein just provides reusable compositions that eventually just reduce to
    * `EditFieldKey => Permission`.
    *
    * This is especially important on dense screens like ReqTable where having a reusable instance for all editable
    * fields per-row / per-req can prevent a lot of needless vdom re-calculation and processing.
    */
  object Editability {

    def forProject(p: Project): ForProject =
      ForProject(p.config, p.reqs, p.reqCodes)

    final case class ForProject(cfg: ProjectConfig, reqs: Requirements, reqCodes: ReqCodes) {
      val forReqs = ForReqs(cfg, reqs)
      val forReqCodeGroups = ForReqCodeGroups(reqCodes)
      val forUseCaseSteps = ForUseCaseSteps(reqs.useCases)
    }

    implicit val reusabilityForProject: Reusability[ForProject] =
      Reusability.caseClass

    sealed trait ForKey[K] extends Any {
      def apply(k: K): Permission
    }

    final case class ForReqs(cfg: ProjectConfig, reqs: Requirements) {
      def apply(id: ReqId): ForReq = {
        val req: Req = reqs.need(id)
        req.live(cfg.reqTypes) match {
          case Live => ForReq(Some((req.reqTypeId, cfg)))
          case Dead => ForReq(None)
        }
      }
    }

    implicit val reusabilityForReqs: Reusability[ForReqs] =
      Reusability.caseClass

    final case class ForReq(whenReqIsLive: Option[(ReqTypeId, ProjectConfig)]) extends AnyVal with ForKey[EditFieldKey.ForReq] {
      def apply(k: EditFieldKey.ForReq): Permission =
        whenReqIsLive match {
          case Some((reqTypeId, cfg)) => k match {

            case EditFieldKey.Code
               | EditFieldKey.Title
               | EditFieldKey.Tags
               | EditFieldKey.Implications(_) => Allow

            case EditFieldKey.ReqType =>
              reqTypeId match {
                case _: CustomReqTypeId    => Allow
                case StaticReqType.UseCase => Deny
              }

            case EditFieldKey.CustomField(fid) =>
              cfg.fields.get(fid) match {
                case Some(f) => Allow when f.applicable(reqTypeId).is(Applicable) && f.live(cfg).is(Live)
                case None    => Deny // Field has been removed
              }
          }
          case None => Deny
        }
    }

    implicit val reusabilityForReq: Reusability[ForReq] =
      Reusability.caseClass

    final case class ForReqCodeGroups(reqCodes: ReqCodes) extends AnyVal {
      def apply(id: ReqCodeId): ForReqCodeGroup =
        ForReqCodeGroup(
          reqCodes.getById(id) match {
            case Some(_: ReqCode.ActiveGroup) => Allow
            case Some(_: ReqCode.ActiveReq)
               | Some(_: ReqCode.Inactive)
               | None                         => Deny
          }
        )
    }

    implicit val reusabilityForReqCodeGroups: Reusability[ForReqCodeGroups] =
      Reusability.caseClass

    final case class ForReqCodeGroup(permission: Permission) extends AnyVal with ForKey[EditFieldKey.ForReqCodeGroup] {
      def apply(k: EditFieldKey.ForReqCodeGroup): Permission =
        k match {
          case EditFieldKey.Code
             | EditFieldKey.Title => permission
        }
    }

    implicit val reusabilityForReqCodeGroup: Reusability[ForReqCodeGroup] =
      Reusability.caseClass

    final case class ForUseCaseSteps(useCases: UseCases) extends AnyVal with ForKey[EditFieldKey.UseCaseStep] {
      def apply(k: EditFieldKey.UseCaseStep): Permission =
        Allow when useCases.focusStep(k.id).live.is(Live)
    }

    implicit val reusabilityForUseCaseSteps: Reusability[ForUseCaseSteps] =
      Reusability.caseClass
  }

  // ===================================================================================================================

  /** A command to start a new editor.
    *
    * @tparam P Key used in [[PreviewFeature]].
    */
  sealed trait NewEditor[+P] {
    type Key <: EditFieldKey
    def key: Key
    def allowEdit(e: Editability.ForProject): Permission

//    def feature[S, PP >: P](static: Static[S, PP])
//                           (async: AsyncActionFeature.D0.Feature[AsyncError],
//                            lens: Lens[S, D0.State],
//                            editability: Editability.ForKey[Key]): D0.Feature =
//      editability(key) match {
//        case Allow => new D0.MainFeatureImpl(static, async, lens, this)
//        case Deny => D0.Feature.Nop
//      }
  }

  object NewEditor {

    sealed abstract class ForReq[+P](final val key: EditFieldKey.ForReq) extends NewEditor[P] {
      final override type Key = EditFieldKey.ForReq
      final override def allowEdit(e: Editability.ForProject) = e.forReqs(req.id)(key)
      val req: Req
    }

    sealed abstract class ForReqCodeGroup[+P](final val key: EditFieldKey.ForReqCodeGroup) extends NewEditor[P] {
      final override type Key = EditFieldKey.ForReqCodeGroup
      final override def allowEdit(e: Editability.ForProject) = e.forReqCodeGroups(rcg.id)(key)
      val rcg: ReqCodeGroup
    }

    final case class ReqCodesForReq(req: Req)
      extends ForReq[Nothing](EditFieldKey.Code)

    final case class ReqType(req: GenericReq)
      extends ForReq[Nothing](EditFieldKey.ReqType)

    final case class ImplicationsAll(req: Req, dir: Direction, initialValues: Vector[Pubid])
      extends ForReq[Nothing](EditFieldKey.Implications(dir))

    final case class ImplicationsCustomField(req: Req, fid: CustomField.Implication.Id)
      extends ForReq[Nothing](EditFieldKey.CustomField(fid))

    final case class Tags(req: Req, fid: Option[CustomField.Tag.Id])
      extends ForReq[Nothing](fid.fold[EditFieldKey.ForReq](EditFieldKey.Tags)(EditFieldKey.CustomField))

    final case class ReqTitle[+P](req: Req, focusId: P)
      extends ForReq[P](EditFieldKey.Title)

    final case class CustomTextField[+P](req: Req, fid: CustomField.Text.Id, focusId: P)
      extends ForReq[P](EditFieldKey.CustomField(fid))

    final case class ReqCodeForReqCodeGroup(rcg: ReqCodeGroup)
      extends ForReqCodeGroup[Nothing](EditFieldKey.Code)

    final case class ReqCodeGroupTitle[+P](rcg: ReqCodeGroup, focusId: P)
      extends ForReqCodeGroup[P](EditFieldKey.Title)

    final case class UseCaseStep[+P](id: UseCaseStepId, focusId: P) extends NewEditor[P] {
      override type Key = EditFieldKey.UseCaseStep
      override val key = EditFieldKey.UseCaseStep(id)
      override def allowEdit(e: Editability.ForProject) = e.forUseCaseSteps(key)
    }

    def reqType(req: Req): Option[ReqType] =
      req match {
        case r: GenericReq => Some(ReqType(r))
        case _: UseCase    => None
      }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  /** Provides an implementation to really start a new editor.
    *
    * Doesn't perform applicability checks.
    * Meant for internal use only.
    */
  final private class StartNewEditor[S, P](static: Static[S, P],
                                           async: AsyncFeature.Feature.D0[AsyncError],
                                           lens: Lens[S, D0.State],
                                           editor: NewEditor[P]) {
    import static._

    def apply(cb: Callback): Callback =
      // Editability confirmed in Feature#apply
      startEditWithoutChecks.flatMap($.modState(_, cb))

    private type StartEditFn = CallbackTo[S => S]

    private def startEditWithoutChecks: StartEditFn =
      editor match {
        case NewEditor.ReqTitle               (gr: GenericReq, p) => EditRichText.GenericReqTitle(gr, p)
        case NewEditor.ReqTitle               (uc: UseCase, p)    => EditRichText.UseCaseTitle(uc, p)
        case NewEditor.Tags                   (req, fid)          => EditTags(req, fid)
        case NewEditor.CustomTextField        (req, fid, p)       => EditRichText.CustomTextField(req, fid, p)
        case NewEditor.ReqCodesForReq         (req)               => EditReqCodes.req(req)
        case NewEditor.ReqCodeForReqCodeGroup (rcg)               => EditReqCodes.group(rcg)
        case NewEditor.ReqCodeGroupTitle      (rcg, p)            => EditRichText.ReqCodeGroupTitle(rcg, p)
        case NewEditor.ReqType                (req)               => EditReqType(req)
        case NewEditor.ImplicationsAll        (req, dir, ivs)     => EditImplications.all(req, dir, ivs)
        case NewEditor.ImplicationsCustomField(req, fid)          => EditImplications.customField(req, fid)
        case NewEditor.UseCaseStep            (id, p)             => EditUseCaseStep(id, p)
      }

    private def startEditFn(instance: => CallbackTo[Editor]): StartEditFn =
      CallbackTo.liftTraverse((s: S) =>
        if (lens.get(s).isDefined)
          CallbackTo(s)
        else
          instance.map(ei => lens.set(Some(ei))(s))
      ).id

    private def startEditFnWithStateSnapshot[A, B: Reusability, C <: Editor](initialData: CallbackTo[A])
                                                                            (initialValue: A => B)
                                                                            (editor: A => StateSnapshot[B] => C): StartEditFn =
      initialData.flatMap { a =>
        val editorA = editor(a)
        def newEditor: B => C =
          b => editorA(StateSnapshot.withReuse(b)(update))
        lazy val update: B ~=> Callback =
          Reusable.fn(b => $.modState(lens set newEditor(b).some))
        startEditFn(CallbackTo(newEditor(initialValue(a))))
      }

    private def abort: Callback =
      $.modState(lens set None)

    private def commit(cmd: UpdateContentCmd): Callback =
      async((s, f) => saveIO(cmd, s >> abort, f))

    private def makeAbortCommit[A](cmd: A => UpdateContentCmd): Some[AbortCommit[Callback, A ~=> Callback]] =
      Some(AbortCommit(abort, Reusable.fn(v => commit(cmd(v)))))

    /**
     * Instance of [[Editor]] that ensures editing is allowed before rendering.
     */
    private trait EditorImpl extends Editor {
      final type RenderInput = AsyncState
      final type RenderImpl = RenderInput => CallbackTo[Some[VdomElement]]

      protected val renderImpl: RenderImpl

      protected def makeRenderImpl[A](f: RenderInput => CallbackTo[A])(implicit ev: A => VdomElement): RenderImpl =
        as => f(as).map(a => Some(ev(a)))

      private val pxAllowEdit: Px[Permission] =
        pxProject.map(editor allowEdit Editability.forProject(_))

      final override def render(as: AsyncState) =
        // Looks like this could block async but not so. Can't go from edit → async → notAllowed.
        // Unsafety is allowed here because EditorInstance is never Reusable
        pxAllowEdit.value() match {
          case Allow => renderImpl(as).runNow()
          case Deny  => None
        }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditReqCodes {
      import shipreq.webapp.client.project.widgets.high.ReqCodeEditor

      private def trieCB: CallbackTo[ReqCode.Trie] =
        pxProject.toCallback.map(_.reqCodes.trie)

      def req(req: Req): StartEditFn = {
        val id = req.id

        val initialValuesCB: CallbackTo[Set[ReqCode.Value]] =
          pxProject.toCallback.map(_.reqCodes.activeReqCodesByReqId(id))

        val abortCommit: ReqCodeEditor.Multiple.AbortCommit =
          makeAbortCommit(UpdateContentCmd.PatchReqCodes(id, _))

        startEditFnWithStateSnapshot(
          initialValuesCB)(
          ReqCodeEditor.Multiple.seqFmt merge _.toVector.map(PlainText.reqCode).sorted)(
          initialValues => new StateMultiple(_, Some(initialValues), abortCommit))
      }

      private class StateMultiple(ss         : StateSnapshot[String],
                                  initial    : Some[Set[ReqCode.Value]],
                                  abortCommit: ReqCodeEditor.Multiple.AbortCommit) extends EditorImpl {
        override val renderImpl = makeRenderImpl(as =>
          for {
            trie <- trieCB
          } yield ReqCodeEditor.Multiple.Props(
            ss,
            initial,
            trie,
            EditorStatus.async(as),
            abortCommit)
            .render)
      }

      def group(rcg: ReqCodeGroup): StartEditFn = {
        val id          = rcg.id

        val initialValueCB: CallbackTo[ReqCode.Value] =
          pxProject.toCallback.map(_.reqCodes.reqCode(id))

        val abortCommit: ReqCodeEditor.Single.AbortCommit =
          makeAbortCommit(UpdateContentCmd.SetReqCodeGroupCode(id, _))

        startEditFnWithStateSnapshot(
          initialValueCB)(
          PlainText.reqCode)(
          i => new StateSingle(_, Some(i), abortCommit))
      }

      private class StateSingle(ss         : StateSnapshot[String],
                                initial    : Some[ReqCode.Value],
                                abortCommit: ReqCodeEditor.Single.AbortCommit) extends EditorImpl {
        override val renderImpl = makeRenderImpl(as =>
          for {
            trie <- trieCB
          } yield ReqCodeEditor.Single.Props(
            ss,
            initial,
            trie,
            EditorStatus.async(as),
            abortCommit)
            .render)
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditReqType {
      import shipreq.webapp.client.project.widgets.high.ReqTypeSelector
      import ReqTypeSelector.RT

      val pxCustomReqTypes = ReqTypeSelector.pxCustomReqTypes(pxProject)

      def apply(req: GenericReq): StartEditFn = {
        val id = req.id

        val abortCommit: ReqTypeSelector.AbortCommit =
          makeAbortCommit[RT](t => UpdateContentCmd.SetGenericReqType(id, t.id)).value

        val initialCB: CallbackTo[RT] =
          pxProject.toCallback.map(_.config.reqTypes.custom.need(req.reqTypeId))

        startEditFn(initialCB.map { initial =>
          val pxChoices = ReqTypeSelector.pxChoices(initial, pxCustomReqTypes)
          State(initial, initial, pxChoices, abortCommit)
        })
      }

      private case class State(initialValue: RT,
                               editValue   : RT,
                               pxChoices   : Px[NonEmptySet[RT]],
                               abortCommit : ReqTypeSelector.AbortCommit) extends EditorImpl {

        def ss = StateSnapshot(editValue)(e => $.modState(lens set copy(editValue = e).some))

        override val renderImpl = makeRenderImpl(as =>
          for {
            choices <- pxChoices.toCallback
          } yield ReqTypeSelector.Props(
            initialValue,
            ss,
            choices,
            EditorStatus.async(as),
            abortCommit)
            .render)
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditImplications {
      import shipreq.webapp.client.project.widgets.high.ImplicationEditor
      import ImplicationEditor.{Lookup, ValidationFn}

      val pxLookupAll = Px.apply2(pxProject, pxPlainText)(ImplicationEditor.Lookup.all)

      def all(req: Req, dir: Direction, initialValues: Vector[Pubid]): StartEditFn =
        startEdit(req, dir, pxLookupAll, Px.constByValue[Seq[Pubid]](initialValues))

      def customField(req: Req, fid: CustomField.Implication.Id): StartEditFn = {
        val dir = CustomField.Implication.dir
        val lookup = Px.apply2(pxProject, pxLookupAll)(ImplicationEditor.Lookup.forCustomColumn(_, _, fid))
        val pubids = ??? //pxProject.map[Seq[Pubid]](p => ImplicationEditor.initialValueForCustomColumn(p, fid, req.id))
        startEdit(req, dir, lookup, pubids)
      }

      private def startEdit(req: Req, dir: Direction, pxLookup: Px[Lookup], pxPubids: Px[Seq[Pubid]]): StartEditFn = {
        val subjectId = req.id

        val pxInit: Px[(Set[ReqId], String)] =
          for {
            project <- pxProject
            lookup <- pxLookup
            pubids <- pxPubids
          } yield ImplicationEditor.initialValueAndText((subjectId, pubids).some, project, lookup)

        val pxValFn: Px[ValidationFn] =
          for {
            project <- pxProject
            init <- pxInit
          } yield ImplicationEditor.validationFn(project, subjectId.some, init._1, dir)

        val abortCommit: ImplicationEditor.AbortCommit =
          makeAbortCommit(UpdateContentCmd.PatchImplications(subjectId, dir, _))

        startEditFnWithStateSnapshot(
          pxInit.toCallback)(
          _._2)(
          _ => new State(_, pxLookup, pxValFn, abortCommit))
      }

      private class State(ss         : StateSnapshot[String],
                          pxLookup   : Px[Lookup],
                          pxValFn    : Px[ValidationFn],
                          abortCommit: ImplicationEditor.AbortCommit) extends EditorImpl {
        override val renderImpl = makeRenderImpl(as =>
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
            .render)
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditTags {
      import shipreq.webapp.client.project.widgets.high.TagEditor
      import TagEditor.Lookup

      def apply(req: Req, fid: Option[CustomField.Tag.Id]): StartEditFn = {
        val id       = req.id
        val lookupFn = fid.fold[Project => Lookup](Lookup.notUsedInTagFields)(Lookup.forTagField)
        val pxLookup = pxProject map lookupFn

        val pxInit: Px[(Set[ApplicableTagId], String)] =
          for {
            project <- pxProject
            lookup <- pxLookup
          } yield TagEditor.initialValues(project.reqTags(id), project.config, lookup)

        val abortCommit: TagEditor.AbortCommit =
          makeAbortCommit(UpdateContentCmd.PatchReqTags(id, _))

        startEditFnWithStateSnapshot(
          pxInit.toCallback)(
          _._2)(
          init => new State(_, Some(init._1), pxLookup, abortCommit))
      }

      private class State(ss           : StateSnapshot[String],
                          initialValues: Some[Set[ApplicableTagId]],
                          pxLookup     : Px[Lookup],
                          abortCommit  : TagEditor.AbortCommit) extends EditorImpl {
        override val renderImpl = makeRenderImpl(as =>
          for {
            lookup <- pxLookup.toCallback
          } yield TagEditor.Props(
            initialValues,
            ss,
            lookup,
            EditorStatus.async(as),
            abortCommit)
            .render)
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditRichText {
      import shipreq.webapp.base.text._
      import shipreq.webapp.client.project.widgets.high.RichTextEditor

      abstract class Base[T <: Text.Generic](val editor: RichTextEditor[T]) {
        val T: editor.text.type = editor.text

        def startEdit(cmd           : T.OptionalText => UpdateContentCmd,
                      initialValueCB: CallbackTo[T.OptionalText],
                      focusId       : P): StartEditFn = {

          val abortCommit: editor.AbortCommit =
            makeAbortCommit(cmd)

          val initCB =
            for {
              initialValue <- initialValueCB
              plainText    <- pxPlainText.toCallback
            } yield {
              val initialText = plainText.format(RichTextEditor.hardcodedLive, initialValue)
              (initialValue, initialText)
            }

          startEditFnWithStateSnapshot(
            initCB)(
            _._2)(
            i => new State(_, Some(i._1), focusId, abortCommit))
        }

        private class State(ss         : StateSnapshot[String],
                            initial    : Some[T.OptionalText],
                            focusId    : P,
                            abortCommit: editor.AbortCommit) extends EditorImpl {

          override val renderImpl = makeRenderImpl(as =>
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
              previewFeature(focusId, previewState),
              initial)
              .render)
        }
      }

      object GenericReqTitle extends Base(RichTextEditor.GenericReqTitle) {
        def apply(req: GenericReq, focusId: P): StartEditFn =
          startEdit(
            UpdateContentCmd.SetGenericReqTitle(req.id, _),
            CallbackTo pure req.title,
            focusId)
      }

      object UseCaseTitle extends Base(RichTextEditor.UseCaseTitle) {
        def apply(uc: UseCase, focusId: P): StartEditFn =
          startEdit(
            UpdateContentCmd.SetUseCaseTitle(uc.id, _),
            CallbackTo pure uc.title,
            focusId)
      }

      object ReqCodeGroupTitle extends Base(RichTextEditor.ReqCodeGroupTitle) {
        def apply(rcg: ReqCodeGroup, focusId: P): StartEditFn =
          startEdit(
            UpdateContentCmd.SetReqCodeGroupTitle(rcg.id, _),
            CallbackTo pure rcg.title,
            focusId)
      }

      object CustomTextField extends Base(RichTextEditor.CustomTextField) {
        def apply(req: Req, fid: CustomField.Text.Id, focusId: P): StartEditFn =
          startEdit(
            UpdateContentCmd.SetCustomTextField(req.id, fid, _),
            pxProject.toCallback.map(p => ReqData.textAt(fid, req.id).get(p.reqText)),
            focusId)
      }
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    object EditUseCaseStep {
      import shipreq.webapp.client.project.widgets.high.RichTextEditor.hardcodedLive
      import shipreq.webapp.client.project.widgets.high.UseCaseStepEditor
      import UseCaseStepFlowText.TextAndFlow

      def apply(id: UseCaseStepId, focusId: P): StartEditFn = {

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

          startEditFnWithStateSnapshot(
            pxInit.toCallback)(
            _._2)(
            i => new State(_, Some(i._1), focusId, commitFn))
      }

      private class State(ss     : StateSnapshot[String],
                          initial: Some[UseCaseStepEditor.InitialValue],
                          focusId: P,
                          commit : UseCaseStepEditor.CommitFn) extends EditorImpl {

        override val renderImpl = makeRenderImpl(as =>
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
            previewFeature(focusId, previewState),
            initial)
            .render)
      }
    }

  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object D0 {
    type State = Option[Editor]

    sealed abstract class Feature {
      /**
        * Change the UI so that the user can edit a portion of data.
        *
        * @return `None` if the underlying data isn't allowed to be edited.
        *         `None` if the editor is already active.
        *         `Some[Callback]` otherwise that when invoked, will add an editor to state and UI.
        */
      def startEdit(cb: Callback): Option[Callback]
      final def startEdit: Option[Callback] = startEdit(Callback.empty)
    }

    object Feature {
      def apply[S, P, K](static: Static[S, P])
                        (editor: NewEditor[P] {type Key = K})
                        (async: AsyncFeature.Feature.D0[AsyncError],
                         lens: Lens[S, State],
                         editability: Editability.ForKey[K]): Feature =
        editability(editor.key) match {
          case Allow =>
            val starter = new StartNewEditor(static, async, lens, editor)
            new Feature {
              override def startEdit(cb: Callback) =
                Some(starter(cb))
            }
          case Deny => Nop
        }

      object Nop extends Feature {
        override def startEdit(cb: Callback) = None
      }
    }

  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object D1 {

    final class State[A, B](private[feature] val values: Map[A, Editor],
                            i: Intersection[A, B]) extends State.ReadOnly[B] {
      @elidable(elidable.FINE)
      override def toString = s"D1.State($values)"

      override def isEmpty = values.isEmpty

      override def apply(key: B): D0.State =
        i.reverse.fold(key, values.get)(None)

      def set(key: B, o: D0.State): State[A, B] = {
        val m = Dimensions.set1(i)(values, key, o)
        new State(m, i)
      }

      override def mapKey[C](j: Intersection[B, C]): State[A, C] =
        new State(values, i <=> j)

      def mergeInto(parent: State[A, A]): State[A, A] = {
        val m = Dimensions.merge(i.getOption)(parent.values, values)
        new State(m, Intersection.id[A])
      }
    }

    object State {
      type Simple[K] = State[K, K]

      sealed abstract class ReadOnly[K] {
        def isEmpty: Boolean
        def apply(key: K): D0.State
        def mapKey[C](j: Intersection[K, C]): ReadOnly[C]
      }

      implicit def reusabilityState1[K]: Reusability[ReadOnly[K]] =
        // Contents are effectively mutable
        Reusability.when(_.isEmpty)

      private[ContentEditorFeature] def empty[A, B](p: Intersection[A, B]): State[A, B] =
        new State(Map.empty, p)

      private[ContentEditorFeature] def emptyA[A]: State[A, A] =
        empty(Intersection.id[A])

      def init[A: UnivEq]: State[A, A] =
        emptyA

      @inline def at[K](k: K): Lens[State[K, K], D0.State] =
        atP[K, K](k)

      def atP[A, B](b: B): Lens[State[A, B], D0.State] =
        Lens((_: State[A, B])(b))(o => _.set(b, o))
    }

    sealed abstract class Feature[-K] {
      def apply(k: K): D0.Feature
    }

    object Feature {
      def apply[K](f: K => D0.Feature): Feature[K] =
        new Feature[K] { override def apply(k: K) = f(k) }

      def optional[K](f: K => Option[D0.Feature]): Feature[K] =
        apply(f(_) getOrElse D0.Feature.Nop)

      object Nop extends Feature[Any] {
        override def apply(k: Any) = D0.Feature.Nop
      }

//      implicit def reusability[K]: Reusability[Feature[K]] =
    }

    /**
      * A means for a child to initialise itself when the state is in the parent in an unknown shape.
      */
    abstract class InitChild[K, P] {
      type Parent
      val parent    : StateAccessPure[Parent]
      val editorLens: K => Option[Lens[Parent, D0.State]]
      val preview   : PreviewFeature.Feature.Composite[P]

      def feature(f: (K, Lens[Parent, D0.State]) => D0.Feature): Feature[K] =
        D1.Feature.optional[K](k =>
          editorLens(k).map(el =>
            f(k, el)))
    }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object D2 {

    final class State[A2, B2, A1, B1](private[feature] val values: Map[A2, D1.State[A1, A1]],
                                      i2: Intersection[A2, B2],
                                      i1: Intersection[A1, B1]) extends State.ReadOnly[B2, B1] {
      @elidable(elidable.FINE)
      override def toString = s"D2.State($values)"

      override def isEmpty = values.isEmpty

      override def apply(key: B2): D1.State[A1, B1] =
        i2.reverse.fold(key, values.get)(None) match {
          case Some(s) => s mapKey i1
          case None    => D1.State.empty(i1)
        }

      def set(k2: B2, v: D1.State[A1, B1]): State[A2, B2, A1, B1] = {
        val m = Dimensions.set2(i2)(values)(k2, v mergeInto _.getOrElse(D1.State.emptyA), _.isEmpty)
        new State(m, i2, i1)
      }

      override def mapKey2[C2](j: Intersection[B2, C2]): State[A2, C2, A1, B1] =
        new State(values, i2 <=> j, i1)

      override def mapKey1[C1](j: Intersection[B1, C1]): State[A2, B2, A1, C1] =
        new State(values, i2, i1 <=> j)
    }

    object State {
      type Simple[K2, K1] = State[K2, K2, K1, K1]

      def init[K2: UnivEq, K1: UnivEq]: State[K2, K2, K1, K1] =
        new State(UnivEq.emptyMap, Intersection.id[K2], Intersection.id[K1])

      sealed abstract class ReadOnly[K2, K1] {
        def isEmpty: Boolean
        def apply(key: K2): D1.State.ReadOnly[K1]
        def mapKey2[K](j: Intersection[K2, K]): ReadOnly[K, K1]
        def mapKey1[K](j: Intersection[K1, K]): ReadOnly[K2, K]
      }

      implicit def reusabilityState2[K2, K1]: Reusability[ReadOnly[K2, K1]] =
        // Contents are effectively mutable
        Reusability.when(_.isEmpty)

      def at[A2, B2, A1, B1](k: B2): Lens[State[A2, B2, A1, B1], D1.State[A1, B1]] =
        Lens((_: State[A2, B2, A1, B1])(k))(o => _.set(k, o))
    }

    sealed abstract class Feature[-K2, -K1] {
      def apply(k2: K2): D1.Feature[K1]
    }

    object Feature {
      def apply[K2, K1](f: K2 => D1.Feature[K1]): Feature[K2, K1] =
        new Feature[K2, K1] { override def apply(k: K2) = f(k) }

      def optional[K2, K1](f: K2 => Option[D1.Feature[K1]]): Feature[K2, K1] =
        apply(f(_) getOrElse D1.Feature.Nop)

      object Nop extends Feature[Any, Any] {
        override def apply(k2: Any) = D1.Feature.Nop
      }

//      implicit def reusability[A, B]: Reusability[Feature[A, B]] =
    }

    /**
     * A means for a child to initialise itself when the state is in the parent in an unknown shape.
     */
    abstract class InitChild[K2, K1, P] {
      type Parent
      val parent    : StateAccessPure[Parent]
      val editorLens: (K2, K1) => Option[Lens[Parent, D0.State]]
      val preview   : PreviewFeature.Feature.Composite[P]

      def feature(f: (K2, K1, Lens[Parent, D0.State]) => D0.Feature): Feature[K2, K1] =
        D2.Feature[K2, K1](k2 =>
          D1.Feature.optional[K1](k1 =>
            editorLens(k2, k1).map(el =>
              f(k2, k1, el))))
    }
  }

}
