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

  /**
   * ADT representing all types of fields supported by the editor.
   * Meant to be used as a key for some given content (e.g. for requirement FR-1).
   */
  sealed abstract class EditFieldKey
  object EditFieldKey {
    case object ReqType                       extends EditFieldKey
    case object Code                          extends EditFieldKey
    case object Title                         extends EditFieldKey
    case object Tags                          extends EditFieldKey
    case class Implications(dir: Direction)   extends EditFieldKey
    case class CustomField(id: CustomFieldId) extends EditFieldKey
    case class UseCaseStep(id: UseCaseStepId) extends EditFieldKey

    object Implications {
      private val memo = Direction.memo(new Implications(_))
      def apply(d: Direction): Implications = memo(d)
    }

    // DeletionReason is a bit odd in that it is append-only, not directly editable.
    // case object DeletionReason extends EditFieldKey

    @inline implicit def equality: UnivEq[EditFieldKey] =
      UnivEq.derive

    implicit val reusability: Reusability[EditFieldKey] =
      Reusability.byUnivEq
  }

  /**
   * Representation of an editor.
   * This is used like a command to create new editors.
   *
   * @tparam P Key used in [[PreviewFeature]].
   */
  sealed trait Editor[+P]
  object Editor {
    case class ReqCodesForReq         (req: Req)                                               extends Editor[Nothing]
    case class ReqCodeForReqCodeGroup (rcg: ReqCodeGroup, initialValue: ReqCode.Value)         extends Editor[Nothing]
    case class ReqType                (req: GenericReq)                                        extends Editor[Nothing]
    case class ImplicationsAll        (req: Req, dir: Direction, initialValues: Vector[Pubid]) extends Editor[Nothing]
    case class ImplicationsCustomField(req: Req, fid: CustomField.Implication.Id)              extends Editor[Nothing]
    case class Tags                   (req: Req, fid: Option[CustomField.Tag.Id])              extends Editor[Nothing]
    case class ReqTitle           [+P](req: Req, focusId: P)                                   extends Editor[P]
    case class ReqCodeGroupTitle  [+P](rcg: ReqCodeGroup, focusId: P)                          extends Editor[P]
    case class CustomTextField    [+P](req: Req, fid: CustomField.Text.Id, focusId: P)         extends Editor[P]
    case class UseCaseStep        [+P](id: UseCaseStepId, focusId: P)                          extends Editor[P]

    def reqType(req: Req): Option[ReqType] =
      req match {
        case r: GenericReq => Some(ReqType(r))
        case _: UseCase    => None
      }

    def allowEditFn(editor: Editor[Any]): Project => Permission = {
      def liveReq(id: ReqId, ofid: Option[FieldId]): Project => Permission =
        p => {
          val r: Req =
            p.reqs.need(id)

          def live: Live =
            r.live(p.config.reqTypes)

          def fieldApplicable: Applicable =
            ofid match {
              case None => Applicable // No field specified
              case Some(fid) =>
                p.config.fields.get(fid) match {
                  // Check field
                  case Some(f) => f.applicable(r.reqTypeId) & (Applicable when f.live(p.config).is(Live))
                  // Field has been removed
                  case None => NotApplicable
                }
            }

          Allow.when(live.is(Live) && fieldApplicable.is(Applicable))
        }

      def liveRCG(id: ReqCodeId): Project => Permission =
        _.reqCodes.getById(id) match {
          case Some(_: ReqCode.ActiveGroup) => Allow
          case Some(_: ReqCode.ActiveReq)
             | Some(_: ReqCode.Inactive)
             | None                         => Deny
        }

      def liveUseCaseStep(id: UseCaseStepId): Project => Permission =
        Allow when _.reqs.useCases.focusStep(id).live.is(Live)

      editor match {
        case Editor.ReqCodesForReq         (req)         => liveReq(req.id, None)
        case Editor.ReqType                (req)         => liveReq(req.id, None)
        case Editor.ImplicationsAll        (req, _, _)   => liveReq(req.id, None)
        case Editor.ReqTitle               (req, _)      => liveReq(req.id, None)
        case Editor.Tags                   (req, fid)    => liveReq(req.id, fid)
        case Editor.ImplicationsCustomField(req, fid)    => liveReq(req.id, fid.some)
        case Editor.CustomTextField        (req, fid, _) => liveReq(req.id, fid.some)
        case Editor.ReqCodeForReqCodeGroup (rcg, _)      => liveRCG(rcg.id)
        case Editor.ReqCodeGroupTitle      (rcg, _)      => liveRCG(rcg.id)
        case Editor.UseCaseStep            (id, _)       => liveUseCaseStep(id)
      }
    }
  }

  type AsyncError = String

  type AsyncState = AsyncFeature.ReadOnly.D0[AsyncError]

  /**
    * This is not safe for reusability because the implementation calls both `Px#value()` and `CallbackTo#runNow()`.
    */
  trait EditorInstance {
    def render(as: AsyncState): Option[VdomElement]
  }

  object EditorInstance {
    implicit def reusabilityEditor: Reusability[EditorInstance] =
      Reusability.never // ∵ Editor is not safe for reusability
  }

  @inline implicit class CEFState0Ops(private val s: D0.State) extends AnyVal {
    def renderOr[A](as: AsyncState)(a: => A)(implicit ev: VdomElement => A): A =
      s.flatMap(_.render(as)).fold(a)(ev)
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object D0 {
    type State = Option[EditorInstance]

    sealed abstract class Feature {
      /**
        * Change the UI so that the user can edit a portion of data.
        *
        * @return `None` if the underlying data isn't allowed to be edited.
        *         `None` if the editor is already active.
        *         `Some[Callback]` otherwise that when invoked, will add an editor to state and UI.
        */
      def startEdit(p: Project, cb: Callback = Callback.empty): Option[Callback]
    }

    object Feature {
      def apply[S, P](static: Static[S, P],
                      async : AsyncFeature.Feature.D0[AsyncError])
                     (lens  : Lens[S, State],
                      editor: Option[Editor[P]]): Feature =
        editor match {
          case Some(e) => new MainFeatureImpl(static, async, lens, e)
          case None    => Nop
        }

      object Nop extends Feature {
        override def startEdit(p: Project, cb: Callback = Callback.empty) = None
      }
    }

    final private class MainFeatureImpl[S, P](static: Static[S, P],
                                              async : AsyncFeature.Feature.D0[AsyncError],
                                              lens  : Lens[S, State],
                                              editor: Editor[P]) extends Feature {
      import static._

      override def startEdit(p: Project, cb: Callback = Callback.empty): Option[Callback] =
        Editor.allowEditFn(editor)(p).option(
          startEditWithoutChecks.flatMap($.modState(_, cb)))

      private type StartEditFn = CallbackTo[S => S]

      private def startEditWithoutChecks: StartEditFn =
        editor match {
          case Editor.ReqTitle               (gr: GenericReq, p) => EditRichText.GenericReqTitle(gr, p)
          case Editor.ReqTitle               (uc: UseCase, p)    => EditRichText.UseCaseTitle(uc, p)
          case Editor.Tags                   (req, fid)          => EditTags(req, fid)
          case Editor.CustomTextField        (req, fid, p)       => EditRichText.CustomTextField(req, fid, p)
          case Editor.ReqCodesForReq         (req)               => EditReqCodes.req(req)
          case Editor.ReqCodeForReqCodeGroup (rcg, iv)           => EditReqCodes.group(rcg, iv)
          case Editor.ReqCodeGroupTitle      (rcg, p)            => EditRichText.ReqCodeGroupTitle(rcg, p)
          case Editor.ReqType                (req)               => EditReqType(req)
          case Editor.ImplicationsAll        (req, dir, ivs)     => EditImplications.all(req, dir, ivs)
          case Editor.ImplicationsCustomField(req, fid)          => EditImplications.customField(req, fid)
          case Editor.UseCaseStep            (id, p)             => EditUseCaseStep(id, p)
        }

      def startEditFn(instance: => CallbackTo[EditorInstance]): StartEditFn =
        CallbackTo.liftTraverse((s: S) =>
          if (lens.get(s).isDefined)
            CallbackTo(s)
          else
            instance.map(ei => lens.set(Some(ei))(s))
        ).id

      private def startEditFnWithStateSnapshot[A, B: Reusability, C <: EditorInstance](initialData: CallbackTo[A])
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
       * Instance of [[EditorInstance]] that ensures editing is allowed before rendering.
       */
      private trait EditorInstanceImpl extends EditorInstance {

        final type RenderInput = AsyncState
        final type RenderImpl = RenderInput => CallbackTo[Some[VdomElement]]

        protected val renderImpl: RenderImpl

        protected def makeRenderImpl[A](f: RenderInput => CallbackTo[A])(implicit ev: A => VdomElement): RenderImpl =
          as => f(as).map(a => Some(ev(a)))

        private val pxAllowEdit: Px[Permission] =
          pxProject.map(Editor.allowEditFn(editor))

        final override def render(as: AsyncState) =
          // Looks like this could block async but not so. Can't go from edit → async → notAllowed.
          // Unsafety is allowed here because EditorInstance is never Reusable
          pxAllowEdit.value() match {
            case Allow => renderImpl(as).runNow()
            case Deny  => None
          }
      }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

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
                                    abortCommit: ReqCodeEditor.Multiple.AbortCommit) extends EditorInstanceImpl {
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

        def group(rcg: ReqCodeGroup, initialValue: ReqCode.Value): StartEditFn = {
          val id          = rcg.id
          val initialText = PlainText reqCode initialValue

          val abortCommit: ReqCodeEditor.Single.AbortCommit =
            makeAbortCommit(UpdateContentCmd.SetReqCodeGroupCode(id, _))

          startEditFnWithStateSnapshot(
            CallbackTo pure initialText)(
            Identity.apply)(
            _ => new StateSingle(_, Some(initialValue), abortCommit))
        }

        private class StateSingle(ss         : StateSnapshot[String],
                                  initial    : Some[ReqCode.Value],
                                  abortCommit: ReqCodeEditor.Single.AbortCommit) extends EditorInstanceImpl {
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

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

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
                                 abortCommit : ReqTypeSelector.AbortCommit) extends EditorInstanceImpl {

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

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      object EditImplications {
        import shipreq.webapp.client.project.widgets.high.ImplicationEditor
        import ImplicationEditor.{Lookup, ValidationFn}

        val pxLookupAll = Px.apply2(pxProject, pxPlainText)(ImplicationEditor.Lookup.all)

        def all(req: Req, dir: Direction, initialValues: Vector[Pubid]): StartEditFn =
          startEdit(req, dir, pxLookupAll, Px.constByValue[Seq[Pubid]](initialValues))

        def customField(req: Req, fid: CustomField.Implication.Id): StartEditFn = {
          val dir = CustomField.Implication.dir
          val lookup = Px.apply2(pxProject, pxLookupAll)(ImplicationEditor.Lookup.forCustomColumn(_, _, fid))
          val pubids = pxProject.map[Seq[Pubid]](p => ImplicationEditor.initialValueForCustomColumn(p, fid, req.id))
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
                            abortCommit: ImplicationEditor.AbortCommit) extends EditorInstanceImpl {
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

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

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
                            abortCommit  : TagEditor.AbortCommit) extends EditorInstanceImpl {
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

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

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
                              abortCommit: editor.AbortCommit) extends EditorInstanceImpl {

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

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

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
                            commit : UseCaseStepEditor.CommitFn) extends EditorInstanceImpl {

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
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object D1 {

    final class State[A, B](private[feature] val values: Map[A, EditorInstance],
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
