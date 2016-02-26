package shipreq.webapp.client.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.ScalazReact._
import monocle.Lens
import scala.annotation.elidable
import shipreq.base.util._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.Event.NESD
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.base.text.{PlainText, TextSearch}
import shipreq.webapp.client.data.TCB
import shipreq.webapp.client.lib.KeyHandlers
import shipreq.webapp.client.protocol.ServerCall
import shipreq.webapp.client.widgets.high.ProjectWidgets

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
 * - Add `State.Simple` to your parent component's state.
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
  case class Static[S, P]($               : CompState.Access[S],
                          previewFeature  : PreviewFeature[S, P],
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
    case object ImplicationSrc                extends EditFieldKey
    case object ImplicationTgt                extends EditFieldKey
    case class CustomField(id: CustomFieldId) extends EditFieldKey

    // DeletionReason is a bit odd in that it is append-only, not directly editable.
    // case object DeletionReason extends EditFieldKey

    @inline implicit def equality: UnivEq[EditFieldKey] =
      UnivEq.deriveAuto

    implicit val reusability: Reusability[EditFieldKey] =
      Reusability.byEqual
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

    def reqType(req: Req): Option[ReqType] =
      req match {
        case r: GenericReq => Some(ReqType(r))
        case _: UseCase    => None
      }
  }

  /**
    * This is effectively mutable because of the underlying usage of Pxs and reading of PreviewFeature state.
    */
  trait EditorInstance {
    def render(): Option[ReactElement]
  }

  object EditorInstance {
    implicit def reusabilityEditor: Reusability[EditorInstance] =
      Reusability.never // ∵ Editor is effectively mutable
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object D0 {
    type State = Option[EditorInstance]

    abstract class Feature {
      def startEdit(focus: => Callback): Option[Callback]
    }

    object Feature {
      def apply[S, P](static: Static[S, P],
                      async : AsyncActionFeature.D0.Feature[String])
                     (lens  : Lens[S, State],
                      editor: Option[Editor[P]]): Feature =
        editor match {
          case Some(e) => new MainFeatureImpl(static, async, lens, e)
          case None    => nop
        }

      val nop: Feature =
        new Feature {
          override def startEdit(focus: => Callback) = None
        }
    }

    final private class MainFeatureImpl[S, P](static: Static[S, P],
                                              async : AsyncActionFeature.D0.Feature[String],
                                              lens  : Lens[S, State],
                                              editor: Editor[P]) extends Feature {
      import static._

      val pxAllowEdit: Px[Permission] = {
        def liveReq(id: ReqId, ofid: Option[FieldId]): Px[Permission] =
          pxProject.map { p =>
            val r = p.reqs.req(id)

            def isLive = r.live(p.config.customReqTypes) :: Live

            def isFieldApplicable =
              (ofid match {
                case None =>
                  Applicable // No field specified
                case Some(fid) =>
                  p.config.fields.get(fid) match {
                    case Some(f) => f.applicable(r.reqTypeId)  // Check field
                    case None    => NotApplicable              // Field has been removed
                  }
              }) :: Applicable

            Allow <~ (isLive && isFieldApplicable)
          }

        def liveRCG(id: ReqCodeId): Px[Permission] =
          pxProject.map(p =>
            p.reqCodes.getById(id) match {
              case Some(_: ReqCode.ActiveGroup) => Allow
              case Some(_: ReqCode.ActiveReq)
                 | Some(_: ReqCode.Inactive)
                 | None                         => Deny
            }
          )

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
        }
      }

      override def startEdit(focus: => Callback): Option[Callback] =
        pxAllowEdit.value().option(
          $.modState(startEditWithoutChecks, focus))

      private type StartEditFn = S => S

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
        }

      def startEditFn(instance: EditorInstance): StartEditFn =
        lens set instance.some

      private def rvarToCellEditor[A: Reusability, B <: EditorInstance](f: ReusableVar[A] => B): A => B = {
        lazy val update: A ~=> Callback =
          ReusableFn(a => $.modState(lens set f(ReusableVar(a)(update)).some))
        a => f(ReusableVar(a)(update))
      }

      private def rvarStrToStartEditFn[B <: EditorInstance](f: ReusableVar[String] => B, initial: String): StartEditFn =
        startEditFn(rvarToCellEditor(f) apply initial)


      private def abort: Callback =
        $.modState(lens set None)

      private def commit(cmd: UpdateContentCmd): Callback =
        async.wrapAsync((s, f) => saveIO(cmd, s >> abort, f))

      private def commitOrIgnore[A, B](a: A)(filterIgnorable: A => Option[B])(cmd: B => UpdateContentCmd): Callback =
        filterIgnorable(a) match {
          case Some(b) => commit(cmd(b))
          case None    => abort
        }

      private def commitAndAbort[A, B](singleLine: Boolean, o: Option[A])(commitFn: A => Callback) =
        KeyHandlers.commit(o map commitFn, singleLine) + KeyHandlers.abort(abort)

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

      /**
       * Instance of [[EditorInstance]] that ensures editing is allowed before rendering.
       */
      private trait EditorInstanceImpl extends EditorInstance {

        val renderCB: CallbackTo[Some[ReactElement]]

        protected def renderStatic[A](a: A)(implicit e: A => ReactElement): CallbackTo[Some[ReactElement]] =
          CallbackTo pure Some(e(a))

        protected def renderDynamic[A](a: => A)(implicit e: A => ReactElement): CallbackTo[Some[ReactElement]] =
          CallbackTo(Some(e(a)))

        final override def render() =
          pxAllowEdit.value() match {
            case Allow => renderCB.runNow()
            case Deny  => None
          }
      }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      object EditReqCodes {
        import shipreq.webapp.client.widgets.high.ReqCodeEditor

        private def trie() = pxProject.value().reqCodes.trie

        def req(req: Req): StartEditFn = {
          val id            = req.id
          val initialValues = pxProject.value().reqCodes.activeReqCodesByReqId(id)
          val initialText   = ReqCodeEditor.Multiple.seqFmt merge initialValues.toVector.map(PlainText.reqCode).sorted

          val extra: ReqCodeEditor.Multiple.Extra =
            ReusableFn(
              commitAndAbort(false, _)(
                commitOrIgnore(_)(
                  ignoreEmptySetDiff(initialValues))(UpdateContentCmd.PatchReqCodes(id, _))))

          rvarStrToStartEditFn(new StateMultiple(_, Some(initialValues), extra), initialText)
        }

        private class StateMultiple(rvar   : ReusableVar[String],
                                    initial: Some[Set[ReqCode.Value]],
                                    extra  : ReqCodeEditor.Multiple.Extra) extends EditorInstanceImpl {
          def props = ReqCodeEditor.Multiple.Props(rvar, initial, trie(), extra)
          override val renderCB = renderDynamic(props.render)
        }

        def group(rcg: ReqCodeGroup, initialValue: ReqCode.Value): StartEditFn = {
          val id          = rcg.id
          val initialText = PlainText reqCode initialValue

          val extra: ReqCodeEditor.Single.Extra =
            ReusableFn(
              commitAndAbort(true, _)(
                commitOrIgnore(_)(
                  ignoreEqual(initialValue))(UpdateContentCmd.SetReqCodeGroupCode(id, _))))

          rvarStrToStartEditFn(new StateSingle(_, Some(initialValue), extra), initialText)
        }

        private class StateSingle(rvar   : ReusableVar[String],
                                  initial: Some[ReqCode.Value],
                                  extra  : ReqCodeEditor.Single.Extra) extends EditorInstanceImpl {
          def props = ReqCodeEditor.Single.Props(rvar, initial, trie(), extra)
          override val renderCB = renderDynamic(props.render)
        }
      }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      object EditReqType {
        import shipreq.webapp.client.widgets.high.ReqTypeSelector
        import ReqTypeSelector.A

        val pxCustomReqTypes = ReqTypeSelector.pxCustomReqTypes(pxProject)

        def apply(req: GenericReq): StartEditFn = {
          val id        = req.id
          val initial   = pxProject.value().config.reqTypeC(req.reqTypeId)
          val pxChoices = ReqTypeSelector.pxChoices(initial, pxCustomReqTypes)

          val is = new State(ignoreEqual(initial), initial, pxChoices, t => UpdateContentCmd.SetGenericReqType(id, t.id))
          startEditFn(is)
        }

        private case class State(ignoreInitial: A => Option[A],
                                 edit         : A,
                                 pxChoices    : Px[NonEmptySet[A]],
                                 cmd          : A => UpdateContentCmd.SetGenericReqType) extends EditorInstanceImpl {

          def evar = ExternalVar(edit)(e => $.modState(lens set copy(edit = e).some))

          def commitCB: Option[TCB.Commit] =
            ignoreInitial(edit).map(a => TCB.Commit(commit(cmd(a))))

          def props = ReqTypeSelector.Props(evar, Some(TCB Abort abort), commitCB, pxChoices.value())

          override val renderCB = renderDynamic(ReqTypeSelector.Component(props))
        }
      }


      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      object EditImplications {
        import shipreq.webapp.client.widgets.high.ImplicationEditor
        import ImplicationEditor.{Lookup, ValidationFn}

        val pxLookupAll = Px.apply2(pxProject, pxPlainText)(ImplicationEditor.Lookup.all)

        def all(req: Req, dir: Direction, initialValues: Vector[Pubid]): StartEditFn =
          startEdit(req, dir, pxLookupAll, initialValues)

        def customField(req: Req, fid: CustomField.Implication.Id): StartEditFn = {
          val dir = CustomField.Implication.dir
          val lookup = Px.apply2(pxProject, pxLookupAll)(ImplicationEditor.Lookup.forCustomColumn(_, _, fid))
          val pubids = ImplicationEditor.initialValueForCustomColumn(pxProject.value(), fid, req.id)
          startEdit(req, dir, lookup, pubids)
        }

        private def startEdit(req: Req, dir: Direction, pxLookup: Px[Lookup], pubids: Seq[Pubid]): StartEditFn = {
          val subjectId = req.id

          val (initialValues, initialText) = ImplicationEditor.initialValueAndText(
            (subjectId, pubids).some, pxProject.value(), pxLookup.value())

          val pxValFn =
            pxProject.map(p =>
              ImplicationEditor.validationFn(p, subjectId.some, initialValues, dir))

          val cmd: NESD[ReqId] => UpdateContentCmd =
            UpdateContentCmd.PatchImplications(subjectId, dir, _)

          val extra: ImplicationEditor.Extra =
            ReusableFn(
              commitAndAbort(true, _)(
                commitOrIgnore(_)(
                  NonEmpty(_))(cmd)))

          rvarStrToStartEditFn(new State(_, pxLookup, pxValFn, extra), initialText)
        }

        private class State(rvar  : ReusableVar[String],
                            lookup: Px[Lookup],
                            valFn : Px[ValidationFn],
                            extra : ImplicationEditor.Extra) extends EditorInstanceImpl {
          def props = ImplicationEditor.Props(rvar, lookup.value(), valFn.value(), pxTextSearch.value(), extra)
          override val renderCB = renderDynamic(props.render)
        }
      }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      object EditTags {
        import shipreq.webapp.client.widgets.high.TagEditor
        import TagEditor.Lookup

        def apply(req: Req, fid: Option[CustomField.Tag.Id]): StartEditFn = {
          val id       = req.id
          val lookupFn = fid.fold[Project => Lookup](Lookup.notUsedInTagFields)(Lookup.forTagField)
          val pxLookup = pxProject map lookupFn

          val (initialValues, initialText) = {
            val p = pxProject.value()
            TagEditor.initialValues(p.reqTags(id), p.config, pxLookup.value())
          }

          val extra: TagEditor.Extra =
            ReusableFn(
              commitAndAbort(true, _)(
                commitOrIgnore(_)(
                  ignoreEmptySetDiff(initialValues, _.map(_.id).toSet))(UpdateContentCmd.PatchReqTags(id, _))))

          rvarStrToStartEditFn(new State(_, pxLookup, extra), initialText)
        }

        private class State(rvar  : ReusableVar[String],
                            lookup: Px[Lookup],
                            extra : TagEditor.Extra) extends EditorInstanceImpl {
          def props = TagEditor.Props(rvar, lookup.value(), extra)
          override val renderCB = renderDynamic(props.render)
        }
      }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      object EditRichText {
        import shipreq.webapp.base.text._
        import shipreq.webapp.client.widgets.high.RichTextEditor

        abstract class Base[T <: Text.Generic](val editor: RichTextEditor[T]) {
          val T: editor.text.type = editor.text

          def startEdit(cmd         : T.OptionalText => UpdateContentCmd,
                        initialValue: T.OptionalText,
                        focusId     : P): StartEditFn = {

            val extra: editor.Extra =
              ReusableFn(
                commitAndAbort(T.singleLine, _)(t =>
                  commit(cmd(t))))

            val initialText: String =
              pxPlainText.value().format(editor.hardcodedLive, initialValue)

            rvarStrToStartEditFn(new State(_, Some(initialValue), focusId, extra), initialText)
          }

          private class State(rvar   : ReusableVar[String],
                              initial: Some[T.OptionalText],
                              focusId: P,
                              extra  : editor.Extra) extends EditorInstanceImpl {

            override val renderCB =
              $.state.map { s =>
                import Px.AutoValue._
                val props = editor.Props(
                  pxProject, pxPlainText, pxTextSearch, pxProjectWidgets,
                  rvar, previewFeature.forChild(focusId, s), initial, extra)
                Some(props.render: ReactElement)
              }
          }
        }

        object GenericReqTitle extends Base(RichTextEditor.GenericReqTitle) {
          def apply(req: GenericReq, focusId: P): StartEditFn =
            startEdit(
              UpdateContentCmd.SetGenericReqTitle(req.id, _),
              req.title,
              focusId)
        }

        object UseCaseTitle extends Base(RichTextEditor.UseCaseTitle) {
          def apply(uc: UseCase, focusId: P): StartEditFn =
            startEdit(
              UpdateContentCmd.SetUseCaseTitle(uc.id, _),
              uc.title,
              focusId)
        }

        object ReqCodeGroupTitle extends Base(RichTextEditor.ReqCodeGroupTitle) {
          def apply(rcg: ReqCodeGroup, focusId: P): StartEditFn =
            startEdit(
              UpdateContentCmd.SetReqCodeGroupTitle(rcg.id, _),
              rcg.title,
              focusId)
        }

        object CustomTextField extends Base(RichTextEditor.CustomTextField) {
          def apply(req: Req, fid: CustomField.Text.Id, focusId: P): StartEditFn =
            startEdit(
              UpdateContentCmd.SetCustomTextField(req.id, fid, _),
              ReqData.textAt(fid, req.id).get(pxProject.value().reqText),
              focusId)
        }
      }

    }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object D1 {

    final class State[A, B](val values: Map[A, EditorInstance], i: Intersection[A, B]) extends State.ReadOnly[B] {
      @elidable(elidable.FINE)
      override def toString = s"D1.State($values)"

      override def isEmpty = values.isEmpty

      override def apply(key: B): D0.State =
        i.reverse.fold(key, values.get)(None)

      def set(key: B, o: D0.State): State[A, B] = {
        val m = Dimensions.set1(i)(values, key, o)
        new State(m, i)
      }

      override def mapK[C](j: Intersection[B, C]): State[A, C] =
        new State(values, i composeIntersection j)

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
        def mapK[C](j: Intersection[K, C]): ReadOnly[C]
      }

      implicit def reusabilityState1[K]: Reusability[ReadOnly[K]] =
        // Contents are effectively mutable
        Reusability.whenTrue(_.isEmpty)

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

    abstract class Feature[-K] {
      def apply(k: K): D0.Feature
    }

    object Feature {
      def apply[K](f: K => D0.Feature): Feature[K] =
        new Feature[K] { override def apply(k: K) = f(k) }

      def optional[K](f: K => Option[D0.Feature]): Feature[K] =
        apply(f(_) getOrElse D0.Feature.nop)

      def nop: Feature[Any] =
        apply(_ => D0.Feature.nop)
    }

    /**
      * A means for a child to initialise itself when the state is in the parent in an unknown shape.
      */
    abstract class InitChild[K, P] {
      type Parent
      val parent    : CompState.Access[Parent]
      val editorLens: K => Option[Lens[Parent, D0.State]]
      val preview   : PreviewFeature[Parent, P]

      def feature(f: (K, Lens[Parent, D0.State]) => D0.Feature): Feature[K] =
        D1.Feature.optional[K](k =>
          editorLens(k).map(el =>
            f(k, el)))
    }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object D2 {

    final class State[A2, B2, A1, B1](val values: Map[A2, D1.State[A1, A1]],
                                      i2: Intersection[A2, B2],
                                      i1: Intersection[A1, B1]) extends State.ReadOnly[B2, B1] {
      @elidable(elidable.FINE)
      override def toString = s"D2.State($values)"

      override def isEmpty = values.isEmpty

      override def apply(key: B2): D1.State[A1, B1] =
        i2.reverse.fold(key, values.get)(None) match {
          case Some(s) => s mapK i1
          case None    => D1.State.empty(i1)
        }

      def set(k2: B2, v: D1.State[A1, B1]): State[A2, B2, A1, B1] = {
        val m = Dimensions.set2(i2)(values)(k2, v mergeInto _.getOrElse(D1.State.emptyA), _.isEmpty)
        new State(m, i2, i1)
      }

      override def mapK2[C2](j: Intersection[B2, C2]): State[A2, C2, A1, B1] =
        new State(values, i2 composeIntersection j, i1)

      override def mapK1[C1](j: Intersection[B1, C1]): State[A2, B2, A1, C1] =
        new State(values, i2, i1 composeIntersection j)
    }

    object State {
      type Simple[K2, K1] = State[K2, K2, K1, K1]

      def init[K2: UnivEq, K1: UnivEq]: State[K2, K2, K1, K1] =
        new State(UnivEq.emptyMap, Intersection.id[K2], Intersection.id[K1])

      sealed abstract class ReadOnly[K2, K1] {
        def isEmpty: Boolean
        def apply(key: K2): D1.State.ReadOnly[K1]
        def mapK2[K](j: Intersection[K2, K]): ReadOnly[K, K1]
        def mapK1[K](j: Intersection[K1, K]): ReadOnly[K2, K]
      }

      implicit def reusabilityState2[K2, K1]: Reusability[ReadOnly[K2, K1]] =
        // Contents are effectively mutable
        Reusability.whenTrue(_.isEmpty)

      def at[A2, B2, A1, B1](k: B2): Lens[State[A2, B2, A1, B1], D1.State[A1, B1]] =
        Lens((_: State[A2, B2, A1, B1])(k))(o => _.set(k, o))
    }

    abstract class Feature[-K2, -K1] {
      def apply(k2: K2): D1.Feature[K1]
    }

    object Feature {
      def apply[K2, K1](f: K2 => D1.Feature[K1]): Feature[K2, K1] =
        new Feature[K2, K1] { override def apply(k: K2) = f(k) }

      def optional[K2, K1](f: K2 => Option[D1.Feature[K1]]): Feature[K2, K1] =
        apply(f(_) getOrElse D1.Feature.nop)

      def nop: Feature[Any, Any] =
        apply(_ => D1.Feature.nop)
    }

    implicit def reusabilityFeature[A, B]: Reusability[Feature[A, B]] =
      Reusability.byRef

    /**
     * A means for a child to initialise itself when the state is in the parent in an unknown shape.
     */
    abstract class InitChild[K2, K1, P] {
      type Parent
      val parent    : CompState.Access[Parent]
      val editorLens: (K2, K1) => Option[Lens[Parent, D0.State]]
      val preview   : PreviewFeature[Parent, P]

      def feature(f: (K2, K1, Lens[Parent, D0.State]) => D0.Feature): Feature[K2, K1] =
        D2.Feature[K2, K1](k2 =>
          D1.Feature.optional[K1](k1 =>
            editorLens(k2, k1).map(el =>
              f(k2, k1, el))))
    }
  }

}
