package shipreq.webapp.client.project.widgets

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.Memo
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq._
import scala.collection.immutable.SortedSet
import scalacss.ScalaCssReact._
import scalacss.StyleA
import scalajs.js.{UndefOr, undefined}
import scalaz.\/
import shipreq.base.util._
import shipreq.base.util.SafeStringOps._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Grammar => G, _}
import shipreq.webapp.base.util.ReqCodeTreeItem
import shipreq.webapp.base.data.{Contextualise, Plain}
import shipreq.webapp.base.jsfacade.KaTeX
import shipreq.webapp.base.lib.ClientUtil.{renderSeq, renderVector, sepComma, sepSpace}
import shipreq.webapp.base.text.Text.AnyOptional
import shipreq.webapp.client.project.app.Style.{widgets => *}

object ProjectWidgets {

  type AnyCtx = ProjectWidgets[_ <: ProjectText.Context]
  type NoCtx  = ProjectWidgets[ProjectText.Context.None]

  def apply[Ctx <: ProjectText.Context](project    : Project,
                                        plainText  : PlainText.ForProject[Ctx],
                                        reqDetailRC: RouterCtl[ExternalPubid]): ProjectWidgets[Ctx] =
    new ProjectWidgets(project, plainText, reqDetailRC)

  implicit def subst1[F[_], C <: ProjectText.Context](pw: F[ProjectWidgets[C]]): F[ProjectWidgets.AnyCtx] =
    pw.asInstanceOf[F[AnyCtx]]
  implicit def subst2[F[_], G[_], C <: ProjectText.Context](pw: F[G[ProjectWidgets[C]]]): F[G[ProjectWidgets.AnyCtx]] =
    pw.asInstanceOf[F[G[AnyCtx]]]

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val emptySpan: VdomTag =
    <.span

  val blankButMandatory: VdomTag =
    <.div(*.blankButMandatory, "blank")

  private[ProjectWidgets] object Internal {

    val deadValidity: Validity => Live => (Live, Validity) =
      Validity.memo(validityWhenDead =>
        Live.memo {
          case Live => (Live, Valid)
          case Dead => (Dead, validityWhenDead)
        }
      )

    val invalidWhenDead: Live => (Live, Validity) =
      deadValidity(Invalid)

    val stepFlowClauseBase: Direction => VdomTag = {
      val base = <.div(*.useCaseStepFlowClause)
      Direction.memo {
        case Forwards  => base("→")
        case Backwards => base("←")
      }
    }

    @inline implicit def surroundDisplay(s: GrammarSpec.Surrounds): GrammarSpec.Surround =
      s.display

    @inline def memo[A: UnivEq](f: A => VdomElement): A => VdomElement =
      Memo(f)

    val issueDescSurroundPrefix = G.issueDescSurround.prefix.trim
    val issueDescSurroundSuffix = G.issueDescSurround.suffix.trim
  }
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

final class ProjectWidgets[Ctx <: ProjectText.Context](project      : Project,
                                                       val plainText: PlainText.ForProject[Ctx],
                                                       reqDetailRC  : RouterCtl[ExternalPubid])
    extends ProjectText[Ctx, VdomTag](project, plainText.ctx) {

  import ProjectWidgets.Internal._

  def withCtx[Ctx2 <: ProjectText.Context](newCtx: Ctx2): ProjectWidgets[Ctx2] =
    if (newCtx ==* ctx)
      this.asInstanceOf[ProjectWidgets[Ctx2]]
    else
      ProjectWidgets(project, plainText withCtx newCtx, reqDetailRC)

  override def _text(text: AnyOptional, live: Live): VdomTag =
    <.span(text map textByLive(live): _*)

  override protected def whenBlankButMandatory =
    ProjectWidgets.blankButMandatory

  private val textByLive: Live => Atom.AnyAtom => TagMod =
    Live.memo { live =>
      import Atom._
      lazy val atom: AnyAtom => TagMod = {
        case a: Literal         # Literal        => <.span(a.value)
        case a: NewLine         # BlankLine      => <.div(*.blankLine)
        case a: TagRef          # TagRef         => tagInText(live)(a.value)
        case a: PlainTextMarkup # WebAddress     => <.a(^.href := a.value, a.value)
        case a: PlainTextMarkup # EmailAddress   => <.a(^.href := "mailto:" ~ a.value, a.value)
        case a: PlainTextMarkup # MathTeX        => katex(a)
        case a: ListMarkup      # UnorderedList  => <.ul(*.ul, a.items.whole.toTagMod(row => <.li(row toTagMod atom)))
        case a: ContentRef      # ReqRef         => reqRefInValidText(a.value)
        case a: ContentRef      # CodeRef        => codeRef(a.value)
        case a: ContentRef      # UseCaseStepRef => useCaseStepRefById(a.value)
        case a: Issue           # Issue          => issue(a.typ, a.desc, live)
      }
      atom
    }

  private def linkOrSpan(req: Req): VdomTag =
    linkOrSpan(req, req.pubid.external(project))

  private def linkOrSpan(req: Req, ep: ExternalPubid): VdomTag =
    gctx match {
      case ProjectText.Context.Req(id) if req.id ==* id => ProjectWidgets.emptySpan
      case ProjectText.Context.None
         | _: ProjectText.Context.Req                   => reqDetailRC.link(ep)
    }

  @inline private def reqRefInValidText: PubidFormat =
    PubidFormat.invalidWhenDeadWithCtx

  /** Contextualised */
  private val codeRef: ReqCodeId => VdomElement =
    memo { id =>
      import ProjectText.ReqCodeResolution._

      implicit def liveWithValidity(a: Live): (Live, Validity) =
        invalidWhenDead(a)

      def toRef(code: ReqCode.Value, r: ReqId): VdomElement = {
        val req = project.content.reqs.need(r)
        ref(
          linkOrSpan(req)(*.reqRef(req live project.config.reqTypes)),
          code,
          plainText reqTitle req)
      }

      def toGroup(code: ReqCode.Value, g: CodeGroup): VdomElement =
        ref(<.span(*.codeGroupRef(g.live)), code, plainText.codeGroupTitle(g))

      def ref(base: VdomTag, code: ReqCode.Value, title: String): VdomTag =
        base(
          ^.title := UiText.hoverText(title),
          G.reflinkSurround(PlainText reqCode code))

      ProjectText.ReqCodeResolution(id, project.content.reqCodes) match {
        case ActiveCodeToReq     (c, r) => toRef(c, r)
        case ActiveCodeToGroup   (c, g) => toGroup(c, g)
        case DeadGroup           (c, g) => toGroup(c, g)
        case ReqWithAltCode      (c, r) => toRef(c, r)
        case ReqWithoutActiveCode(_, r) => reqRefInValidText(r)
      }
    }

  private def useCaseStepRefById(id: UseCaseStepId): VdomTag =
    useCaseStepRef(project.content.reqs.useCases.focusStep(id))

  private val useCaseStepRef: UseCaseStep.Focus => VdomTag =
    Memo.by((_: UseCaseStep.Focus).id)(
      mkUseCaseStep((base, ld, label) =>
        base(
          *.useCaseStepRef(ld),
          G.reflinkSurround(label))))

  private def mkUseCaseStep[A](r: (VdomTag, (Live, Validity), String) => A)(f: UseCaseStep.Focus): A = {
    val label = plainText.useCaseStepLabel(f)
    val title = UiText.hoverText(plainText.text(f.titleA, f.live, Mandatory))
    val ld = deadValidity(Invalid)(f.live)
    val base = linkOrSpan(f.uc)(^.title := title)
    r(base, ld, label)
  }

  private val tagInText: Live => ApplicableTagId => VdomElement =
    Live.memo { liveText =>
      memo[ApplicableTagId] { id =>
        val tag = project.config.tags.atag(id)
        val liveTag = tag.live
        val valid = Invalid.when(liveText.is(Live) && liveTag.is(Dead))
        tagWithoutStyle(Contextualise, tag)(*.tagInText(liveTag, valid))
      }
    }

  private def tagWithoutStyle(c: Contextualise, t: ApplicableTag): VdomTag = {
    var desc = if (t.name.compareToIgnoreCase(t.key.value) == 0) "" else t.name
    for (d <- t.desc) {
      if (desc.nonEmpty)
        desc += "\n\n"
      desc += d
    }
    val keyTxt = t.key.value
    val displayTxt = c match {
      case Contextualise => G.hashRefKey.prefix ~ keyTxt
      case Plain         => keyTxt
    }
    <.span(
      (^.title := desc).when(desc.nonEmpty),
      displayTxt)
  }

  private def issue(id: CustomIssueTypeId, desc: Text.InlineIssueDesc.OptionalText, liveText: Live): VdomElement =
    NonEmptyVector.maybe(desc, issueWithoutDesc(id))(issueWithDesc(id, _, liveText))

  private val issueWithoutDesc: CustomIssueTypeId => VdomElement =
    memo { id =>
      val issueType = cfg.customIssueType(id)
      <.span(
        *.issue,
        G.hashRefKey.prefix ~ issueType.key.value)
    }

  private def issueWithDesc(id: CustomIssueTypeId, desc: Text.InlineIssueDesc.NonEmptyText, liveText: Live): VdomElement = {
    val issueType = project.config.customIssueType(id)
    <.span(
      *.issue,
      G.hashRefKey.prefix ~ issueType.key.value ~ issueDescSurroundPrefix,
      text(desc, liveText)(*.issueDesc),
      issueDescSurroundSuffix)
  }

  private def katex(m: Atom.PlainTextMarkup#MathTeX): VdomTag =
    try
      <.span(*.math, ^.dangerouslySetInnerHtml := KaTeX.renderToStringUnsafe(m.value))
    catch {
      case _: Throwable => <.span(*.mathFail, UiText.mathFailed)
    }

  /** eg. "UC" */
  val reqTypeShort: ReqTypeId => VdomElement =
    memo { id =>
      val rt = project.config.reqTypes.need(id)
      <.span(
        *.reqTypeShort(rt.live),
        ^.title := rt.name,
        rt.mnemonic.value)
    }

  override def useCaseStepTextAndFlow(step: UseCaseStepFlowText.TextAndFlow[AnyOptional, Set[UseCaseStepId]], live: Live): VdomTag =
    makeUseCaseStepTextAndFlow(step, live)(useCaseFlowElementsById(_).iterator)

  private def makeUseCaseStepTextAndFlow[C[x] <: TraversableOnce[x], A](s: UseCaseStepFlowText.TextAndFlow[AnyOptional, C[A]],
                                                                        l: Live)
                                                                       (render: C[A] => TraversableOnce[VdomTag]): VdomTag = {

    val stepText = text(s.text, l, Mandatory.when(s.flow.forall(_.isEmpty)))

    def flowClause(dir: Direction): Option[VdomElement] = {
      val flowElements = s flow dir
      Option.unless(flowElements.isEmpty)(
        stepFlowClauseBase(dir)(
          TagMod.fromTraversableOnce(
            render(flowElements).toIterator.map(t => t: TagMod).intersperse(sepComma))))
    }

    val flowsMaybe: Option[VdomElement] =
      UseCaseStepFlowText.DefaultArrowOrder.map(flowClause) match {
        case (None   , None   ) => None
        case (Some(f), None   ) => Some(f)
        case (None   , Some(f)) => Some(f)
        case (Some(a), Some(b)) => Some(<.div(*.useCaseStepTextAndFlow_flow, a, b))
      }

    <.div(*.useCaseStepTextAndFlow_cont(l),
      <.div(*.useCaseStepTextAndFlow_text, stepText),
      flowsMaybe)
  }

  override protected val useCaseFlowElement: UseCaseStep.Focus => VdomTag =
    Memo.by((_: UseCaseStep.Focus).id)(
      mkUseCaseStep((base, ld, label) =>
        base(
          *.useCaseStepFlowElement(ld),
          label)))

  // Keep in sync with PlainText because it's used together for sorting/rendering in ReqTable
  override protected def deletionReasonWhenNoneGiven: VdomTag =
    ProjectWidgets.emptySpan

  // Keep in sync with PlainText because it's used together for sorting/rendering in ReqTable
  override protected def deletionReasonWhenReqTypeIsDead(rt: ReqType): VdomTag =
    <.span(
      UiText.ColumnNames.reqType + " ",
      reqTypeShort(rt.reqTypeId),
      " is deleted.")

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Public additions not part of ProjectText

  def implicationList(ids: Vector[Pubid]): VdomElement =
    PubidFormat.validWhenDead.pubids(ids)

  def pastPubids(ids: SortedSet[ExternalPubid]): VdomElement =
    renderSeq(
      ids.toIterator.map(ep => <.span(*.pastPubid, PlainText.pubid(ep))),
      sepComma)

  def reqCode(c: ReqCode.Value): VdomTag =
    <.pre(*.reqCodeFlat, PlainText reqCode c)

  def reqCodes(reqCodes: TraversableOnce[ReqCode.Value]): VdomTag =
    <.div(reqCodes toTagMod reqCode)

  def reqCodeTree(items: Vector[ReqCodeTreeItem]): VdomTag =
    <.div(items toTagMod reqCodeTreeItem)

  private val reqCodeTreeIdentation: NonEmptyVector[ReqCodeTreeItem.Indent] => VdomElement =
    memo(is =>
      <.pre(*.reqCodeTreeIndent, PlainText reqCodeIndentation is))

  def reqCodeTreeItem(item: ReqCodeTreeItem): VdomElement = {
    val indentation = NonEmptyVector.option(item.indent)
    var code = PlainText.reqCode(item.suffix)
    if (indentation.isDefined)
      code = G.reqCode.nodeSeparator ~ code
    <.div(
      indentation.whenDefined(reqCodeTreeIdentation),
      <.pre(*.reqCodeTreeCode, code))
  }

  /** eg. "UC: Use Case" */
  val reqTypeFull: ReqTypeId => VdomElement =
    id => {
      val rt = project.config.reqTypes.need(id)
      <.span(s"${rt.mnemonic.value}: ${rt.name}")
    }

  private val tagPlain: ApplicableTagId => VdomElement =
    memo { id =>
      val tag = project.config.tags.atag(id)
      tagWithoutStyle(Plain, tag)(*.tag(tag.live))
    }

  def tagList(ids: Vector[ApplicableTagId]): VdomElement =
    renderVector(ids, sepSpace)(tagPlain)

  def useCaseStepTextAndMaybeInvalidFlow[C[x] <: Traversable[x]](s: UseCaseStepFlowText.TextAndFlow[AnyOptional, C[String \/ UseCaseStepId]],
                                                                 l: Live): VdomTag = {
    /** eg. "1.p" instead of "1.0" */
    def erroneousUseCaseStepRef(s: String): VdomTag =
      <.span(*.erroneousUseCaseStepRef, s)

    makeUseCaseStepTextAndFlow(s, l)(
      _.map(_.fold(erroneousUseCaseStepRef, useCaseFlowElementById))(collection.breakOut))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final class PubidFormat private[PubidFormat](contextualise: Contextualise,
                                               styleFn      : Live => TagMod,
                                               titleFn      : PubidFormat.TitleFn,
                                               liveFn       : PubidFormat.LiveFn) {

    private val label: ExternalPubid => String = {
      val txt = PlainText.pubid(_: ExternalPubid)
      contextualise match {
        case Contextualise => G.reflinkSurround compose txt
        case Plain         => txt
      }
    }

    private val styleMemo = Live.memo(styleFn)

    type Out = VdomElement

    private val memo: ReqId => Out =
      Memo { reqId =>
        val req  = project.content.reqs.need(reqId)
        val ep   = req.pubid.external(project)
        val txt  = label(ep)
        val live = liveFn(req)

        linkOrSpan(req, ep)(
          styleMemo(live),
          ^.title :=? titleFn(req),
          txt)
      }

    def apply(id: ReqId): Out =
      memo(id)

    def apply(req: Req): Out =
      memo(req.id)

    def apply(pubid: Pubid): Out =
      apply(project.content.reqs.reqIdByPubid(pubid))

    private val sep: TagMod =
      contextualise match {
        case Contextualise => sepSpace
        case Plain         => sepComma
      }

    def pubids(v: Vector[Pubid]): VdomTag =
      renderVector(v, sep)(apply)

    def reqs(v: Vector[Req]): VdomTag =
      renderVector(v, sep)(apply)
  }

  object PubidFormat {
    type LiveFn  = Req => Live
    type TitleFn = Req => Option[String]

    private val defaultLiveFn: LiveFn =
      _.live(project.config.reqTypes)

    private val defaultTitleFn: TitleFn =
      r => Some(UiText.hoverText(plainText.reqTitle(r)))

    def apply(ctx    : Contextualise,
              styleFn: Live => TagMod,
              titleFn: TitleFn = defaultTitleFn,
              liveFn : LiveFn  = defaultLiveFn) =
      new PubidFormat(ctx, styleFn, titleFn, liveFn)

    private def reqRefFormat(ctx: Contextualise, validityWhenDead: Validity) = {
      val f = deadValidity(validityWhenDead)
      apply(ctx, l => *.reqRef(f(l)))
    }

    val validWhenDead: PubidFormat =
      reqRefFormat(Plain, Valid)

    val invalidWhenDeadWithCtx: PubidFormat =
      reqRefFormat(Contextualise, Invalid)
  }

}
