package shipreq.webapp.client.widgets.high

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.prefix_<^._
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

  val pubidDetailLink = memo[Pubid] { pubid =>
    val ep = pubid.external(project)
    val txt = PlainText.pubid(ep)
    val req = project.reqs.reqByPubid(pubid)
    reqDetailRC.link(ep)(
      *.pubidColumnValue(req live project.config.customReqTypes),
      txt)
  }

  /**
   * A reference to a requirement.
   *
   * "Basic" because it isn't memoised, and isn't meant to be used directly.
   */
  def reqRefBasic(req: Req, modifyPubidText: EndoFn[String], style: Req => TagMod): ReactElement = {
    val rt  = project.config.reqType(req.pubid.reqTypeId)
    <.span(
      style(req),
      ^.title := plainText.reqTitle(req),
      modifyPubidText(PlainText.pubid(rt, req.pubid.pos)))
  }

  def reqRefBasicById(id: ReqId, modifyPubidText: EndoFn[String], style: Req => TagMod): ReactElement =
    reqRefBasic(project.reqs req id, modifyPubidText, style)

  private val _reqRef2: Contextualise => Validity => ReqId => ReactElement =
    Contextualise.memo { c =>
      val f: EndoFn[String] = c match {
        case Contextualise => G.reflinkSurround
        case Plain         => identity
      }
      Validity.memo { v2 =>
        val g = deadValidity(v2)
        val style: Req => TagMod = req => *.reqRef(g(req live project.config.customReqTypes))
        memo(reqRefBasicById(_, f, style))
      }
    }

  def reqRef(c: Contextualise, validityWhenDead: Validity): ReqId => ReactElement =
    _reqRef2(c)(validityWhenDead)

  private val reqRefInText = reqRef(Contextualise, Invalid)

  def reqRefList(c: Contextualise, validityWhenDead: Validity)(reqs: Vector[ReqId]): ReactElement = {
    val f = reqRef(c, validityWhenDead)
    renderVector(reqs, sepComma)(f)
  }

  def pubidRef(c: Contextualise, validityWhenDead: Validity): Pubid => ReactElement = {
    val f = reqRef(c, validityWhenDead)
    pubid => f(project.reqs.reqIdByPubid(pubid))
  }

  def pubidRefList(c: Contextualise, validityWhenDead: Validity)(ids: Vector[Pubid]): ReactElement =
    renderVector(ids, sepComma)(pubidRef(c, validityWhenDead))

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
      case ReqWithoutActiveCode(_, r) => reqRefInText(r)
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
      case a: ReqRef          # ReqRef        => reqRefInText(a.value)
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
