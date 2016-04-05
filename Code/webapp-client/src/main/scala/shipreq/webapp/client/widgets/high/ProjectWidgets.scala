package shipreq.webapp.client.widgets.high

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.univeq._
import scalacss.ScalaCssReact._
import scalacss.StyleA
import scalajs.js.{undefined, UndefOr}
import shipreq.base.util._
import shipreq.base.util.SafeStringOps._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Grammar => G, _}
import shipreq.webapp.base.util.ReqCodeTreeItem
import shipreq.webapp.client.app.Style.{widgets => *}
import shipreq.webapp.client.data.{Contextualise, Plain}
import shipreq.webapp.client.jsfacade.KaTeX
import shipreq.webapp.client.lib.ClientUtil.{renderVector, sepComma, sepSpace}
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

  val invalidWhenDead = deadValidity(Invalid)
}

final class ProjectWidgets private(project    : Project,
                                   plainText  : PlainText.ForProject,
                                   reqDetailRC: RouterCtl[ExternalPubid]) extends ProjectText[ReactTag](project) {

  private implicit def surroundDisplay(s: G.Surrounds) = s.display

  @inline private def memo[A: UnivEq](f: A => ReactElement) = Memo(f)

  def issueO(liveText: Live, id: CustomIssueTypeId, desc: Text.InlineIssueDesc.OptionalText): ReactElement =
    NonEmptyVector.maybe(desc, issue(id))(issue1(liveText, id, _))

  val issue = memo[CustomIssueTypeId] { id =>
    val i = project.config.customIssueType(id)
    <.span(
      *.issue,
      G.hashRefKey.prefix ~ i.key.value)
  }

  private val issueDescSurroundPrefix = G.issueDescSurround.prefix.trim
  private val issueDescSurroundSuffix = G.issueDescSurround.suffix.trim
  def issue1(liveText: Live, id: CustomIssueTypeId, desc: Text.InlineIssueDesc.NonEmptyText): ReactElement = {
    val i = project.config.customIssueType(id)
    <.span(
      *.issue,
      G.hashRefKey.prefix ~ i.key.value ~ issueDescSurroundPrefix,
      format1(liveText, desc)(*.issueDesc),
      issueDescSurroundSuffix)
  }

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

    type Out = ReactElement

    private val memo: ReqId => Out =
      Memo { reqId =>
        val req   = project.reqs.req(reqId)
        val ep    = req.pubid.external(project)
        val txt   = label(ep)
        val live  = liveFn(req)

        reqDetailRC.link(ep)(
          styleMemo(live),
          ^.title := titleFn(req),
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

    def pubids(v: Vector[Pubid]): ReactTag =
      renderVector(v, sep)(apply)

    def reqs(v: Vector[Req]): ReactTag =
      renderVector(v, sep)(apply)
  }

  object PubidFormat {
    type LiveFn  = Req => Live
    type TitleFn = Req => Option[String]

    private val defaultLiveFn: LiveFn =
      _.live(project.config.customReqTypes)

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

    val validWhenDead =
      reqRefFormat(Plain, Valid)

    val invalidWhenDeadWithCtx =
      reqRefFormat(Contextualise, Invalid)
  }

  @inline def reqRefInValidText =
    PubidFormat.invalidWhenDeadWithCtx

  def implicationList(ids: Vector[Pubid]): ReactElement =
    PubidFormat.validWhenDead.pubids(ids)

  /** Contextualised */
  val codeRef = memo[ReqCodeId] { id =>
    import ProjectText.ReqCodeResolution._
    implicit def liveWithValidity(a: Live) = invalidWhenDead(a)

    def toRef(c: ReqCode.Value, r: ReqId): ReactElement = {
      val req = project.reqs.req(r)
      ref(c, *.reqRef(req live project.config.customReqTypes), plainText reqTitle req)
    }

    def toGroup(c: ReqCode.Value, g: ReqCodeGroup): ReactElement =
      ref(c, *.reqCodeGroupRef(g.live), plainText.reqCodeGroupTitle(g))

    def ref(c: ReqCode.Value, style: StyleA, title: UndefOr[String]): ReactElement =
      <.span(
        style,
        ^.title := title,
        G.reflinkSurround(PlainText reqCode c))

    ProjectText.resolveReqCode(id, project.reqCodes) match {
      case ActiveCodeToReq     (c, r) => toRef(c, r)
      case ActiveCodeToGroup   (c, g) => toGroup(c, g)
      case DeadGroup           (c, g) => toGroup(c, g)
      case ReqWithAltCode      (c, r) => toRef(c, r)
      case ReqWithoutActiveCode(_, r) => reqRefInValidText(r)
    }
  }

  val reqTypeFull: ReqTypeId => ReactElement =
    id => {
      val rt = project.config.reqType(id)
      <.span(s"${rt.mnemonic.value}: ${rt.name}")
    }

  val reqTypeShort: ReqTypeId => ReactElement =
    memo { id =>
      val rt = project.config.reqType(id)
      <.span(
        *.reqTypeShort(rt.live),
        ^.title := rt.name,
        rt.mnemonic.value)
    }

  private def tagWithoutStyle(c: Contextualise, t: ApplicableTag): ReactTag = {
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
      desc.nonEmpty ?= (^.title := desc),
      displayTxt)
  }

  val tagPlain = memo[ApplicableTagId] { id =>
    val tag = project.config.atag(id)
    tagWithoutStyle(Plain, tag)(*.tag(tag.live))
  }

  def tagList(ids: Vector[ApplicableTagId]): ReactElement =
    renderVector(ids, sepSpace)(tagPlain)

  val tagInText: Live => ApplicableTagId => ReactElement =
    Live.memo { liveText =>
      memo[ApplicableTagId] { id =>
        val tag = project.config.atag(id)
        val liveTag = tag.live
        val valid = Invalid <~ ((liveText :: Live) && (liveTag :: Dead))
        tagWithoutStyle(Contextualise, tag)(*.tagInText(liveTag, valid))
      }
    }

  def katex(m: Atom.PlainTextMarkup#MathTeX) =
    try
      <.span(*.math, ^.dangerouslySetInnerHtml(KaTeX renderToStringUnsafe m.value))
    catch {
      case _: Throwable => <.span(*.mathFail, UiText.mathFailed)
    }

  override val format: ProjectText.FormatAtomFn[ReactTag] = (live, input) => {
    import Atom._

    lazy val atom: AnyAtom => TagMod = {
      case a: Literal         # Literal       => <.span(a.value)
      case a: NewLine         # BlankLine     => <.div(*.blankLine)
      case a: TagRef          # TagRef        => tagInText(live)(a.value)
      case a: PlainTextMarkup # WebAddress    => <.a(^.href := a.value, a.value)
      case a: PlainTextMarkup # EmailAddress  => <.a(^.href := "mailto:" ~ a.value, a.value)
      case a: PlainTextMarkup # MathTeX       => katex(a)
      case a: ListMarkup      # UnorderedList => <.ul(*.ul, a.items.whole.map(row => <.li(row map atom: _*)))
      case a: ReqRef          # ReqRef        => reqRefInValidText(a.value)
      case a: ReqRef          # CodeRef       => codeRef(a.value)
      case a: Issue           # Issue         => issueO(live, a.typ, a.desc)
    }

    <.span(input map atom: _*)
  }

  val reqCodeTreeIdentation =
    memo[NonEmptyVector[ReqCodeTreeItem.Indent]](is =>
      <.pre(*.reqCodeTreeIndent, PlainText reqCodeIndentation is))

  def reqCodeTreeItem(item: ReqCodeTreeItem): ReactElement = {
    val indentation = NonEmptyVector.option(item.indent)
    var code = PlainText.reqCode(item.suffix)
    if (indentation.isDefined)
      code = G.reqCode.nodeSeparator ~ code
    <.div(
      indentation map reqCodeTreeIdentation,
      <.pre(*.reqCodeTreeCode, code))
  }

  def reqCodeTree(items: Vector[ReqCodeTreeItem]): TagMod =
    items map reqCodeTreeItem

  def flatReqCode(c: ReqCode.Value): ReactElement =
    <.pre(*.reqCodeFlat, PlainText reqCode c)

  def flatReqCodes(reqCodes: TraversableOnce[ReqCode.Value]): TagMod =
    reqCodes map flatReqCode

  def reqCodes(tree: Vector[ReqCodeTreeItem], flat: Vector[ReqCode.Value]): ReactElement =
    <.div(
      if (tree.nonEmpty)
        reqCodeTree(tree)
      else
        flatReqCodes(flat)
    )
}
