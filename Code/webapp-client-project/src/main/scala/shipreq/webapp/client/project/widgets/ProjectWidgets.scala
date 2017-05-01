package shipreq.webapp.client.project.widgets

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq._
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
import shipreq.webapp.client.base.data.{Contextualise, Plain}
import shipreq.webapp.client.base.jsfacade.KaTeX
import shipreq.webapp.client.base.lib.ClientUtil.{renderVector, sepComma, sepSpace}
import shipreq.webapp.client.project.app.Style.{widgets => *}
import ProjectWidgets.{apply => _, _}

object ProjectWidgets {
  def apply(project: Project, plainText: PlainText.ForProject, reqDetailRC: RouterCtl[ExternalPubid]) =
    new ProjectWidgets(project, plainText, reqDetailRC)

  val deadValidity: Validity => Live => (Live, Validity) =
    Validity.memo(validityWhenDead =>
      Live.memo {
        case Live => (Live, Valid)
        case Dead => (Dead, validityWhenDead)
      }
    )

  val invalidWhenDead: Live => (Live, Validity) =
    deadValidity(Invalid)

  val stepFlowArrow: Direction => VdomTag =
    Direction.memo {
      case Forwards  => <.span("→")
      case Backwards => <.span("←")
    }

  val emptySpan: VdomTag =
    <.span

  // Keep in sync with PlainText.DeletionReason because they're used together for sorting/rendering in ReqTable
  object DeletionReason extends ProjectText.DeletionReasonFormatter[VdomTag] {
    override type PT = ProjectWidgets

    override protected def noReasonGiven =
      emptySpan

    override protected def reqTypeIsDead(rt: ReqType)(pt: PT) =
      <.span(
        UiText.ColumnNames.reqType + " ",
        pt.reqTypeShort(rt.reqTypeId),
        " is deleted.")
  }
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

final class ProjectWidgets private(project    : Project,
                                   plainText  : PlainText.ForProject,
                                   reqDetailRC: RouterCtl[ExternalPubid])
    extends ProjectText[VdomTag](project, plainText.ctx) {

  override def withCtx(newCtx: ProjectText.Context): ProjectWidgets =
    withPlainText(plainText withCtx newCtx)

  def withPlainText(newPlainText: PlainText.ForProject): ProjectWidgets =
    new ProjectWidgets(project, newPlainText, reqDetailRC)

  private implicit def surroundDisplay(s: GrammarSpec.Surrounds): GrammarSpec.Surround = s.display

  @inline private def memo[A: UnivEq](f: A => VdomElement) = Memo(f)

  def issueO(liveText: Live, id: CustomIssueTypeId, desc: Text.InlineIssueDesc.OptionalText): VdomElement =
    NonEmptyVector.maybe(desc, issue(id))(issue1(liveText, id, _))

  val issue: CustomIssueTypeId => VdomElement =
    memo { id =>
      val i = project.config.customIssueType(id)
      <.span(
        *.issue,
        G.hashRefKey.prefix ~ i.key.value)
    }

  private val issueDescSurroundPrefix = G.issueDescSurround.prefix.trim
  private val issueDescSurroundSuffix = G.issueDescSurround.suffix.trim
  def issue1(liveText: Live, id: CustomIssueTypeId, desc: Text.InlineIssueDesc.NonEmptyText): VdomElement = {
    val i = project.config.customIssueType(id)
    <.span(
      *.issue,
      G.hashRefKey.prefix ~ i.key.value ~ issueDescSurroundPrefix,
      format1(liveText, desc)(*.issueDesc),
      issueDescSurroundSuffix)
  }

  @inline def reqRefInValidText: PubidFormat =
    PubidFormat.invalidWhenDeadWithCtx

  def implicationList(ids: Vector[Pubid]): VdomElement =
    PubidFormat.validWhenDead.pubids(ids)

  def pastPubids(ids: Vector[Pubid]): VdomElement =
    renderVector(ids, sepComma)(id => <.span(*.pastPubid, PlainText.pubid(id, project)))

  /** Contextualised */
  val codeRef: ReqCodeId => VdomElement =
    memo { id =>
      import ProjectText.ReqCodeResolution._
      implicit def liveWithValidity(a: Live): (Live, Validity) = invalidWhenDead(a)

      def toRef(c: ReqCode.Value, r: ReqId): VdomElement = {
        val req = project.reqs.need(r)
        ref(c, *.reqRef(req live project.config.reqTypes), plainText reqTitle req)
      }

      def toGroup(c: ReqCode.Value, g: ReqCodeGroup): VdomElement =
        ref(c, *.reqCodeGroupRef(g.live), plainText.reqCodeGroupTitle(g))

      def ref(c: ReqCode.Value, style: StyleA, title: UndefOr[String]): VdomElement =
        <.span(
          style,
          ^.title :=? title,
          G.reflinkSurround(PlainText reqCode c))

      ProjectText.resolveReqCode(id, project.reqCodes) match {
        case ActiveCodeToReq     (c, r) => toRef(c, r)
        case ActiveCodeToGroup   (c, g) => toGroup(c, g)
        case DeadGroup           (c, g) => toGroup(c, g)
        case ReqWithAltCode      (c, r) => toRef(c, r)
        case ReqWithoutActiveCode(_, r) => reqRefInValidText(r)
      }
    }

  /** eg. "UC: Use Case" */
  val reqTypeFull: ReqTypeId => VdomElement =
    id => {
      val rt = project.config.reqTypes.need(id)
      <.span(s"${rt.mnemonic.value}: ${rt.name}")
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

  def useCaseStepRef(id: UseCaseStepId): VdomTag =
    useCaseStepLabelMemo(project.reqs.useCases.focusStep(id))

  /** eg. "1.p" instead of "1.0" */
  def erroneousUseCaseStepRef(s: String): VdomTag =
  <.span(*.erroneousUseCaseStepRef, s)

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

  val tagPlain: ApplicableTagId => VdomElement =
    memo { id =>
      val tag = project.config.atag(id)
      tagWithoutStyle(Plain, tag)(*.tag(tag.live))
    }

  def tagList(ids: Vector[ApplicableTagId]): VdomElement =
    renderVector(ids, sepSpace)(tagPlain)

  val tagInText: Live => ApplicableTagId => VdomElement =
    Live.memo { liveText =>
      memo[ApplicableTagId] { id =>
        val tag = project.config.atag(id)
        val liveTag = tag.live
        val valid = Invalid.when(liveText.is(Live) && liveTag.is(Dead))
        tagWithoutStyle(Contextualise, tag)(*.tagInText(liveTag, valid))
      }
    }

  def katex(m: Atom.PlainTextMarkup#MathTeX): VdomTag =
    try
      <.span(*.math, ^.dangerouslySetInnerHtml := KaTeX.renderToStringUnsafe(m.value))
    catch {
      case _: Throwable => <.span(*.mathFail, UiText.mathFailed)
    }

  override val format: ProjectText.FormatAtomFn[VdomTag] = (live, input) => {
    import Atom._

    lazy val atom: AnyAtom => TagMod = {
      case a: Literal         # Literal        => <.span(a.value)
      case a: NewLine         # BlankLine      => <.div(*.blankLine)
      case a: TagRef          # TagRef         => tagInText(live)(a.value)
      case a: PlainTextMarkup # WebAddress     => <.a(^.href := a.value, a.value)
      case a: PlainTextMarkup # EmailAddress   => <.a(^.href := "mailto:" ~ a.value, a.value)
      case a: PlainTextMarkup # MathTeX        => katex(a)
      case a: ListMarkup      # UnorderedList  => <.ul(*.ul, a.items.whole.toTagMod(row => <.li(row toTagMod atom)))
      case a: ReqRef          # ReqRef         => reqRefInValidText(a.value)
      case a: ReqRef          # CodeRef        => codeRef(a.value)
      case a: UseCaseStepRef  # UseCaseStepRef => useCaseStepRef(a.value)
      case a: Issue           # Issue          => issueO(live, a.typ, a.desc)
    }

    <.span(input map atom: _*)
  }

  val reqCodeTreeIdentation: NonEmptyVector[ReqCodeTreeItem.Indent] => VdomElement =
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

  def reqCodeTree(items: Vector[ReqCodeTreeItem]): VdomTag =
    <.div(items toTagMod reqCodeTreeItem)

  def reqCode(c: ReqCode.Value): VdomTag =
    <.pre(*.reqCodeFlat, PlainText reqCode c)

  def reqCodes(reqCodes: TraversableOnce[ReqCode.Value]): VdomTag =
    <.div(reqCodes toTagMod reqCode)

  override def useCaseStep(l: Live, s: UseCaseStep[Set[UseCaseStepId]]): VdomTag =
    useCaseStepA(l, s)(useCaseFlowStepsOrdered)

  def useCaseStepE[C[x] <: Traversable[x]](l: Live, s: UseCaseStep[C[String \/ UseCaseStepId]]): VdomTag =
    useCaseStepA(l, s)(
      _.map(_.fold(erroneousUseCaseStepRef, useCaseFlowStepId))(collection.breakOut))

  private def useCaseStepA[C[x] <: TraversableOnce[x], A](l: Live, s: UseCaseStep[C[A]])
                                                         (f: C[A] => Seq[VdomTag]): VdomTag = {

    val text = format(l, s.text)

    def stepFlow(dir: Direction): Option[VdomElement] = {
      val ca = s flow dir
      if (ca.isEmpty)
        None
      else
        Some(stepFlowArrow(dir)(f(ca): _*))
    }

    val List(f1, f2) = UseCaseStepFlowText.DefaultArrowOrder.map(stepFlow)

    if (f1.isEmpty && f2.isEmpty)
      text
    else
      <.table(
        <.tbody(
          <.tr(
            <.td(*.useCaseStepLayoutCell, text),
            <.td(*.useCaseStepLayoutCell, f1.whenDefined, f2.whenDefined))))
  }

  private val useCaseStepLabelMemo: UseCaseStep.Focus => VdomTag =
    Memo.by((_: UseCaseStep.Focus).id) { f =>
      val label = plainText.useCaseStepLabel(f)
      val title = plainText.format(f.live, f.titleA)
      val ld = deadValidity(Invalid)(f.live)
      <.span(
        *.useCaseStepRef(ld),
        ^.title := title,
        label)
    }

  override protected def useCaseFlowStep(f: UseCaseStep.Focus): VdomTag =
    useCaseStepLabelMemo(f)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final class PubidFormat private[PubidFormat](ctx    : Contextualise,
                                               styleFn: Live => TagMod,
                                               titleFn: PubidFormat.TitleFn,
                                               liveFn : PubidFormat.LiveFn) {

    private val label: ExternalPubid => String = {
      val txt = PlainText.pubid(_: ExternalPubid)
      ctx match {
        case Contextualise => G.reflinkSurround compose txt
        case Plain         => txt
      }
    }

    private val styleMemo = Live.memo(styleFn)

    type Out = VdomElement

    private val memo: ReqId => Out =
      Memo { reqId =>
        val req   = project.reqs.need(reqId)
        val ep    = req.pubid.external(project)
        val txt   = label(ep)
        val live  = liveFn(req)

        reqDetailRC.link(ep)(
          styleMemo(live),
          ^.title :=? titleFn(req),
          txt)
      }

    def apply(id: ReqId): Out =
      memo(id)

    def apply(req: Req): Out =
      memo(req.id)

    def apply(pubid: Pubid): Out =
      apply(project.reqs.reqIdByPubid(pubid))

    private val sep: TagMod =
      ctx match {
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
      r => Some(plainText.reqTitle(r))

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
