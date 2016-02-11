package shipreq.webapp.client.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import monocle._
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
 *   [[ContentEditorFeature.OneD]] - Editor for a single req. (Usually, Key = req field)
 *   [[ContentEditorFeature.TwoD]] - Editor for a multiple reqs. (Usually, Key2 = req, Key1 = req field)
 *
 * - Add `State.Simple` to your parent component's state.
 *
 * - Create an instance of `Feature` in the parent component's backend.
 *
 * - To render, apply keys from the state until you arrive at `ZeroD.State`, then call `.render()` if available.
 *   (Pass `State.ReadOnly` to child components if needed.)
 *
 * - To start editing, apply keys from the feature until you arrive at `ZeroD.Feature`, then call `.startEdit()`.
 *   (Pass the feature to child components if needed.)
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

  sealed trait Editor[+P]
  object Editor {
    case class ReqCodesForReq         (req: GenericReq)                                               extends Editor[Nothing]
    case class ReqCodeForReqCodeGroup (rcg: ReqCodeGroup, initialValue: ReqCode.Value)                extends Editor[Nothing]
    case class ReqType                (req: GenericReq)                                               extends Editor[Nothing]
    case class ImplicationsAll        (req: GenericReq, dir: Direction, initialValues: Vector[Pubid]) extends Editor[Nothing]
    case class ImplicationsCustomField(req: GenericReq, fid: CustomField.Implication.Id)              extends Editor[Nothing]
    case class Tags                   (req: GenericReq, fid: Option[CustomField.Tag.Id])              extends Editor[Nothing]
    case class GenericReqTitle    [+P](req: GenericReq, focusId: P)                                   extends Editor[P]
    case class ReqCodeGroupTitle  [+P](rcg: ReqCodeGroup, focusId: P)                                 extends Editor[P]
    case class CustomTextField    [+P](req: GenericReq, fid: CustomField.Text.Id, focusId: P)         extends Editor[P]
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

  object ZeroD {
    type State = Option[EditorInstance]

    trait Feature {
      def startEdit(focus: => Callback): Option[Callback]
    }

    object Feature {
      def apply[S, P](static: Static[S, P],
                      async : AsyncActionFeature.Single.Feature[S, String])
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
                                              async : AsyncActionFeature.Single.Feature[S, String],
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
          case Editor.GenericReqTitle        (req, _)      => liveReq(req.id, None)
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
          case Editor.ReqCodesForReq         (req)           => EditReqCodes.req(req)
          case Editor.ReqCodeForReqCodeGroup (rcg, iv)       => EditReqCodes.group(rcg, iv)
          case Editor.ReqType                (req)           => EditReqType(req)
          case Editor.ImplicationsAll        (req, dir, ivs) => EditImplications.all(req, dir, ivs)
          case Editor.ImplicationsCustomField(req, fid)      => EditImplications.customField(req, fid)
          case Editor.Tags                   (req, fid)      => EditTags(req, fid)
          case Editor.GenericReqTitle        (req, p)        => EditRichText.GenericReqTitle(req, p)
          case Editor.ReqCodeGroupTitle      (rcg, p)        => EditRichText.ReqCodeGroupTitle(rcg, p)
          case Editor.CustomTextField        (req, fid, p)   => EditRichText.CustomTextField(req, fid, p)
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

        def req(req: GenericReq): StartEditFn = {
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

        def all(req: GenericReq, dir: Direction, initialValues: Vector[Pubid]): StartEditFn =
          startEdit(req, dir, pxLookupAll, initialValues)

        def customField(req: GenericReq, fid: CustomField.Implication.Id): StartEditFn = {
          val dir = CustomField.Implication.dir
          val lookup = Px.apply2(pxProject, pxLookupAll)(ImplicationEditor.Lookup.forCustomColumn(_, _, fid))
          val pubids = ImplicationEditor.initialValueForCustomColumn(pxProject.value(), fid, req.id)
          startEdit(req, dir, lookup, pubids)
        }

        private def startEdit(req: GenericReq, dir: Direction, pxLookup: Px[Lookup], pubids: Seq[Pubid]): StartEditFn = {
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

        def apply(req: GenericReq, fid: Option[CustomField.Tag.Id]): StartEditFn = {
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

        object ReqCodeGroupTitle extends Base(RichTextEditor.ReqCodeGroupTitle) {
          def apply(rcg: ReqCodeGroup, focusId: P): StartEditFn =
            startEdit(
              UpdateContentCmd.SetReqCodeGroupTitle(rcg.id, _),
              rcg.title,
              focusId)
        }

        object CustomTextField extends Base(RichTextEditor.CustomTextField) {
          def apply(req: GenericReq, fid: CustomField.Text.Id, focusId: P): StartEditFn =
            startEdit(
              UpdateContentCmd.SetCustomTextField(req.id, fid, _),
              ReqData.textAt(fid, req.id).get(pxProject.value().reqText),
              focusId)
        }
      }

    }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object OneD {

    final class State[A, B](private[feature] val values: Map[A, EditorInstance], p: Prism[A, B]) extends State.ReadOnly[B] {
      override def toString = s"OneD.State($values)"

      def isEmpty = values.isEmpty

      def apply(key: B): ZeroD.State =
        values.get(p reverseGet key)

      def set(key: B, o: ZeroD.State): State[A, B] = {
        val k = p reverseGet key
        val m = o match {
          case None    => values - k
          case Some(v) => values.updated(k, v)
        }
        new State(m, p)
      }

      def mapK[C](q: Prism[B, C]): State[A, C] =
        new State(values, p ^<-? q)

      def mergeInto(parent: State[A, A]): State[A, A] = {

        // Include everything from small
        var m = values

        // Include everything from big which isn't covered by small
        for ((a, v) <- parent.values)
          if (p.getOption(a).isEmpty)
            m = m.updated(a, v)

        new State(m, Prism.id[A])
      }
    }

    object State {
      type Simple[K] = State[K, K]

      sealed abstract class ReadOnly[K] {
        def isEmpty: Boolean
        def apply(key: K): ZeroD.State
      }

      implicit def reusabilityState1[K]: Reusability[ReadOnly[K]] =
      // Contents are effectively mutable
        Reusability.fn((a, b) => a.isEmpty && b.isEmpty)

      private[ContentEditorFeature] def empty[A, B](p: Prism[A, B]): State[A, B] =
        new State(Map.empty, p)

      private[ContentEditorFeature] def emptyA[A]: State[A, A] =
        empty(Prism.id[A])

      def init[A: UnivEq]: State[A, A] =
        emptyA

      @inline def at[K](k: K): Lens[State[K, K], ZeroD.State] =
        atP[K, K](k)

      def atP[A, B](b: B): Lens[State[A, B], ZeroD.State] =
        Lens((_: State[A, B])(b))(o => _.set(b, o))
    }

    trait Feature[K] {
      def apply(k: K): ZeroD.Feature
    }

    def Feature[S, P, K](static: Static[S, P],
                         async : AsyncActionFeature.Keyed.Feature[S, K, String])
                        (lens  : Lens[S, State[K, K]],
                         editor: K => Option[Editor[P]]): Feature[K] =
      new Feature[K] {
        override def apply(k: K) =
          ZeroD.Feature(static, async(k))(lens ^|-> State.at(k), editor(k))
      }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object TwoD {

    final class State[A2, B2, A1, B1](private[feature] val values: Map[A2, OneD.State[A1, A1]],
                                      f2: B2 => A2,
                                      p1: Prism[A1, B1]) extends State.ReadOnly[B2, B1] {
      override def toString = s"TwoD.State($values)"

      @inline def isEmpty = values.isEmpty

      def apply(key: B2): OneD.State[A1, B1] =
        values.get(f2(key)) match {
          case Some(s) => s mapK p1
          case None    => OneD.State.empty(p1)
        }

      def set(k2: B2, v: OneD.State[A1, B1]): State[A2, B2, A1, B1] = {
        val k      = f2(k2)
        val oldSeg = values.getOrElse(k, OneD.State.emptyA[A1])
        val newSeg = v.mergeInto(oldSeg)
        val newVal = if (newSeg.isEmpty) values - k else values.updated(k, newSeg)
        new State(newVal, f2, p1)
      }

      def mapK2[C2](f: C2 => B2): State[A2, C2, A1, B1] =
        new State(values, f2 compose f, p1)

      def mapK1[C1](q: Prism[B1, C1]): State[A2, B2, A1, C1] =
        new State(values, f2, p1 ^<-? q)
    }

    object State {
      type Simple[K2, K1] = State[K2, K2, K1, K1]

      sealed abstract class ReadOnly[K2, K1] {
        def isEmpty: Boolean
        def apply(key: K2): OneD.State.ReadOnly[K1]
        def mapK2[K](f: K => K2): ReadOnly[K, K1]
      }

      implicit def reusabilityState2[K2, K1]: Reusability[ReadOnly[K2, K1]] =
        // Contents are effectively mutable
        Reusability.fn((a, b) => a.isEmpty && b.isEmpty)

      def init[K2: UnivEq, K1: UnivEq]: State[K2, K2, K1, K1] =
        new State(UnivEq.emptyMap, identity[K2], Prism.id[K1])

      def at[A2, B2, A1, B1](k: B2): Lens[State[A2, B2, A1, B1], OneD.State[A1, B1]] =
        Lens((_: State[A2, B2, A1, B1])(k))(o => _.set(k, o))
    }

    trait Feature[K2, K1] {
      def apply(k2: K2): OneD.Feature[K1]
    }

    object Feature {
      def apply[S, P, K2, K1](static: Static[S, P],
                              async : AsyncActionFeature.Table.Feature[S, K2, K1, String])
                             (lens  : Lens[S, State[K2, K2, K1, K1]],
                              editor: K2 => K1 => Option[Editor[P]]): Feature[K2, K1] =
        withKeyId(static, async, identity[K2])(lens, editor)

      def withKeyId[S, P, K2, K2Id, K1](static: Static[S, P],
                                        async : AsyncActionFeature.Table.Feature[S, K2Id, K1, String],
                                        id    : K2 => K2Id)
                                       (lens  : Lens[S, State[K2Id, K2Id, K1, K1]],
                                        editor: K2 => K1 => Option[Editor[P]]): Feature[K2, K1] =
        new Feature[K2, K1] {
          override def apply(k: K2) = {
            val i = id(k)
            OneD.Feature(static, async(i))(lens ^|-> State.at(i), editor(k))
          }
        }
    }
  }

}
