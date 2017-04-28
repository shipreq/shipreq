package shipreq.webapp.client.project.feature.editor

import japgolly.microlibs.nonempty._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import scalaz.{-\/, \/, \/-}
import shipreq.base.util._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.base.text._
import shipreq.webapp.client.base.feature._
import shipreq.webapp.client.base.lib.AbortCommit
import shipreq.webapp.client.project.feature.PreviewFeature
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.protocol.ServerCall
import shipreq.webapp.client.project.widgets.high.ProjectWidgets
import EditorFeature.{AsyncError, AsyncState, Editor, State}

/** A command to start a new editor.
  *
  * @tparam P Id used in [[shipreq.webapp.client.project.feature.PreviewFeature]].
  */
sealed abstract class NewEditorCmd[+P]
object NewEditorCmd {
  case class CustomTextField[+P](id: ReqId, field: CustomField.Text.Id, focusId: P)         extends NewEditorCmd[P]
  case class ReqCode            (id: ReqCodeId)                                             extends NewEditorCmd[Nothing]
  case class ReqCodes           (id: ReqId)                                                 extends NewEditorCmd[Nothing]
  case class ReqType            (id: GenericReqId)                                          extends NewEditorCmd[Nothing]
  case class Implications       (id: ReqId, scope: CustomField.Implication.Id \/ Direction) extends NewEditorCmd[Nothing]
  case class Tags               (id: ReqId, field: Option[CustomField.Tag.Id])              extends NewEditorCmd[Nothing]
  case class Title          [+P](id: ReqCodeId \/ ReqId, focusId: P)                        extends NewEditorCmd[P]
  case class UseCaseStep    [+P](id: UseCaseStepId, focusId: P)                             extends NewEditorCmd[P]
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

/** For a specific type of Row and associated Cell, provides a function (∀P) that
  * requests a value specific to the Row/Cell combination,
  * and (if legal), returns a command to create a new editor.
  */
sealed trait MakeNewEditorCmd[R <: RowKey, C <: R#CellKeyConstraint] {
  type Input[P]
  def apply[P](row: R, cell: C): Input[P] => Option[NewEditorCmd[P]]
  // def apply[P](row: R, cell: C): Option[Input[P] => NewEditorCmd[P]]
}

object MakeNewEditorCmd {
  type Nullary [R <: RowKey, C <: R#CellKeyConstraint] = MakeNewEditorCmd[R, C] { type Input[P] = Unit }
  type HasFocus[R <: RowKey, C <: R#CellKeyConstraint] = MakeNewEditorCmd[R, C] { type Input[P] = P }

  private trait Poly {
    def fn[A]: A => NewEditorCmd[A]
  }

  private def hasFocus[R <: RowKey, C <: R#CellKeyConstraint](f: (R, C) => Poly): HasFocus[R, C] =
    new MakeNewEditorCmd[R, C] {
      override type Input[P] = P
      override def apply[P](row: R, cell: C) = f(row, cell).fn[P].andThen(Some(_))
    }

  private def nullary[R <: RowKey, C <: R#CellKeyConstraint](f: (R, C) => Option[NewEditorCmd[Nothing]]): Nullary[R, C] =
    new MakeNewEditorCmd[R, C] {
      override type Input[P] = Unit
      override def apply[P](row: R, cell: C) = _ => f(row, cell)
    }

  @inline private implicit def autoSome(a: NewEditorCmd[Nothing]): Option[NewEditorCmd[Nothing]] = Some(a)

  implicit val reqCodes = nullary [RowKey.Req, CellKey.Code.type      ]((r, _) => NewEditorCmd.ReqCodes(r.id))
  implicit val reqTitle = hasFocus[RowKey.Req, CellKey.Title.type     ]((r, _) => new Poly { def fn[P] = NewEditorCmd.Title(\/-(r.id), _) })
  implicit val reqText  = hasFocus[RowKey.Req, CellKey.CustomTextField]((r, c) => new Poly { def fn[P] = NewEditorCmd.CustomTextField(r.id, c.field, _) })
  implicit val reqImp   = nullary [RowKey.Req, CellKey.Implications   ]((r, c) => NewEditorCmd.Implications(r.id, c.scope))
  implicit val reqTags  = nullary [RowKey.Req, CellKey.Tags           ]((r, c) => NewEditorCmd.Tags(r.id, c.field))
  implicit val reqType  = nullary [RowKey.Req, CellKey.ReqType.type   ]((r, _) =>
    r.id match {
      case i: GenericReqId => Some(NewEditorCmd.ReqType(i))
      case _: UseCaseId    => None
    })

  implicit val rcgCode  = nullary [RowKey.ReqCodeGroup, CellKey.Code.type ]((r, _) => NewEditorCmd.ReqCode(r.id))
  implicit val rcgTitle = hasFocus[RowKey.ReqCodeGroup, CellKey.Title.type]((r, _) => new Poly { def fn[P] = NewEditorCmd.Title(-\/(r.id), _) })

  implicit val useCaseSteps = hasFocus[RowKey.UseCaseSteps.type, CellKey.UseCaseStep](
    (_, c) => new Poly { def fn[P] = NewEditorCmd.UseCaseStep(c.id, _) })
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

/**
 * @tparam P Preview feature ID.
 */
final case class Static[P](previewFeature  : PreviewFeature.Feature.Composite[P],
                           pxProject       : Px[Project],
                           pxPlainText     : Px[PlainText.ForProject],
                           pxProjectWidgets: Px[ProjectWidgets],
                           pxTextSearch    : Px[TextSearch],
                           saveIO          : ServerCall[UpdateContentCmd])

/** Provides an implementation to really start a new editor.
  *
  * Doesn't perform ANY applicability checks.
  *
  * Meant for internal use only.
  */
private[editor] final class StartNewEditor[P](static      : Static[P],
                                              $           : StateAccessPure[State.ForCell],
                                              asyncFeature: AsyncFeature.Feature.D0[AsyncError],
                                              newEditorCmd: NewEditorCmd[P]) {
  import static._

  def apply(cb: Callback): Callback =
    startFnForCmd.get.flatMap($.setState(_, cb))

  private type StartFn = CallbackOption[Editor]

  private def startFnForCmd: StartFn =
    newEditorCmd match {
      case NewEditorCmd.ReqCodes       (id)                       => EditReqCodes.req(id)
      case NewEditorCmd.ReqCode        (id)                       => EditReqCodes.group(id)
      case NewEditorCmd.ReqType        (id)                       => EditReqType(id)
      case NewEditorCmd.Implications   (id, -\/(field))           => EditImplications.customField(id, field)
      case NewEditorCmd.Implications   (id, \/-(dir))             => EditImplications.all(id, dir)
      case NewEditorCmd.Tags           (id, field)                => EditTags(id, field)
      case NewEditorCmd.CustomTextField(id, field            , p) => EditRichText.CustomTextField(id, field, p)
      case NewEditorCmd.Title          (\/-(id: GenericReqId), p) => EditRichText.GenericReqTitle(id, p)
      case NewEditorCmd.Title          (\/-(id: UseCaseId)   , p) => EditRichText.UseCaseTitle(id, p)
      case NewEditorCmd.Title          (-\/(id)              , p) => EditRichText.ReqCodeGroupTitle(id, p)
      case NewEditorCmd.UseCaseStep    (id                   , p) => EditUseCaseStep(id, p)
    }

  private def startWithStateSnapshot[A, B: Reusability, C <: Editor](initialData: CallbackOption[A])
                                                                    (initialValue: A => B)
                                                                    (editor: A => StateSnapshot[B] => C): StartFn =
    initialData.flatMap { a =>
      val editorA = editor(a)
      def newEditor: B => Some[C] = b => Some(editorA(StateSnapshot.withReuse(b)(update)))
      lazy val update: B ~=> Callback = Reusable.fn(b => $.setState(newEditor(b)))
      CallbackOption.liftOption(newEditor(initialValue(a)))
    }

  private def abort: Callback =
    $.setState(None)

  private def commit(cmd: UpdateContentCmd): Callback =
    asyncFeature((s, f) => saveIO(cmd, s >> abort, f))

  private def makeAbortCommit[A](cmd: A => UpdateContentCmd): Some[AbortCommit[Callback, A ~=> Callback]] =
    Some(AbortCommit(abort, Reusable.fn(v => commit(cmd(v)))))

  private def getGenericReq(id: GenericReqId): CallbackOption[GenericReq] =
    pxProject.toCallback.map(_.reqs.genericReqs.get(id)).asCBO

  private def getUseCase(id: UseCaseId): CallbackOption[UseCase] =
    pxProject.toCallback.map(_.reqs.useCases.imap.get(id)).asCBO

  private def getReqCodeGroup(id: ReqCodeId): CallbackOption[LiveReqCodeGroup] =
    pxProject.toCallback.map(_.reqCodes.getById(id).flatMap {
      case d: ReqCode.ActiveGroup => d.group.some
      case _: ReqCode.ActiveReq
         | _: ReqCode.Inactive    => None
    }).asCBO

  /**
   * Instance of [[Editor]] that ensures editing is allowed before rendering.
   */
  private trait EditorImpl extends Editor {
    final type RenderInput = AsyncState
    final type RenderImpl = RenderInput => CallbackTo[Some[VdomElement]]

    protected val renderImpl: RenderImpl

    protected def makeRenderImpl[A](f: RenderInput => CallbackTo[A])(implicit ev: A => VdomElement): RenderImpl =
      as => f(as).map(a => Some(ev(a)))

    final override def render(p: Permission, a: AsyncState) =
      // Looks like this could block async but not so. Can't go from edit → async → notAllowed.
      // Unsafety is allowed here because EditorInstance is never Reusable
      p match {
        case Allow => renderImpl(a).runNow()
        case Deny  => None
      }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object EditReqCodes {
    import shipreq.webapp.client.project.widgets.high.ReqCodeEditor

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

    def group(id: ReqCodeId): StartFn = {
      val initialValueCB: CallbackOption[ReqCode.Value] =
        pxProject.toCallback.map(_.reqCodes.reqCode(id)).toCBO

      val abortCommit: ReqCodeEditor.Single.AbortCommit =
        makeAbortCommit(UpdateContentCmd.SetReqCodeGroupCode(id, _))

      startWithStateSnapshot(initialValueCB)(PlainText.reqCode)(
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

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object EditReqType {
    import shipreq.webapp.client.project.widgets.high.ReqTypeSelector
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

      def ss = StateSnapshot(editValue)(e => $.setState(copy(editValue = e).some))

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

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object EditImplications {
    import shipreq.webapp.client.project.widgets.high.ImplicationEditor
    import ImplicationEditor.{Lookup, ValidationFn}

    val pxLookupAll = Px.apply2(pxProject, pxPlainText)(ImplicationEditor.Lookup.all)

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

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object EditTags {
    import shipreq.webapp.client.project.widgets.high.TagEditor
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

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object EditRichText {
    import shipreq.webapp.base.text._
    import shipreq.webapp.client.project.widgets.high.RichTextEditor

    abstract class Base[T <: Text.Generic](val editor: RichTextEditor[T]) {
      val T: editor.text.type = editor.text

      protected def start(cmd: T.OptionalText => UpdateContentCmd,
                          initialValueCB: CallbackOption[T.OptionalText],
                          focusId: P): StartFn = {

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
      def apply(id: GenericReqId, focusId: P): StartFn =
        start(
          UpdateContentCmd.SetGenericReqTitle(id, _),
          getGenericReq(id).map(_.title),
          focusId)
    }

    object UseCaseTitle extends Base(RichTextEditor.UseCaseTitle) {
      def apply(id: UseCaseId, focusId: P): StartFn =
        start(
          UpdateContentCmd.SetUseCaseTitle(id, _),
          getUseCase(id).map(_.title),
          focusId)
    }

    object ReqCodeGroupTitle extends Base(RichTextEditor.ReqCodeGroupTitle) {
      def apply(id: ReqCodeId, focusId: P): StartFn =
        start(
          UpdateContentCmd.SetReqCodeGroupTitle(id, _),
          getReqCodeGroup(id).map(_.title).widen,
          focusId)
    }

    object CustomTextField extends Base(RichTextEditor.CustomTextField) {
      def apply(id: ReqId, fid: CustomField.Text.Id, focusId: P): StartFn =
        start(
          UpdateContentCmd.SetCustomTextField(id, fid, _),
          pxProject.toCallback.map(p => ReqData.textAt(fid, id).get(p.reqText)).toCBO,
          focusId)
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object EditUseCaseStep {
    import shipreq.webapp.client.project.widgets.high.RichTextEditor.hardcodedLive
    import shipreq.webapp.client.project.widgets.high.UseCaseStepEditor
    import UseCaseStepFlowText.TextAndFlow

    def apply(id: UseCaseStepId, focusId: P): StartFn = {

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
