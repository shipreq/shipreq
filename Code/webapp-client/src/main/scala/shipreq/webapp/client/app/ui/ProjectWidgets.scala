package shipreq.webapp.client.app.ui

import scalacss.ScalaCssReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import scalacss.StyleA
import scalajs.js.{undefined, UndefOr}
import shipreq.base.util._
import shipreq.base.util.SafeStringOps._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Grammar => G, _}
import shipreq.webapp.base.util.ReqCodeTreeItem
import shipreq.webapp.client.app.ui.Style.{widgets => *}
import shipreq.webapp.client.lib.{Contextualise, Plain}
import shipreq.webapp.client.lib.ui.UI
import shipreq.webapp.client.util._

object ProjectWidgets {
  def apply(project: Project, plainText: PlainText.ForProject) =
    new ProjectWidgets(project, plainText)
}

final class ProjectWidgets private(project: Project, plainText: PlainText.ForProject) extends ProjectText[ReactTag](project) {

  private implicit def surroundDisplay(s: G.Surrounds) = s.display

  private def memo[A: UnivEq](f: A => ReactElement): A => ReactElement =
    UnivEq mutableHashMapMemo f

  private val deadValidity: Validity => Live => (Live, Validity) =
    Validity.memo(validityWhenDead =>
      Live.memo {
        case Live => (Live, Valid)
        case Dead => (Dead, validityWhenDead)
      }
    )

  private val invalidWhenDead = deadValidity(Invalid)

  def issueO(id: CustomIssueTypeId, desc: Text.InlineIssueDesc.OptionalText): ReactElement =
    NonEmptyVector.maybe(desc, issue(id))(issue1(id, _))

  val issue = memo[CustomIssueTypeId] { id =>
    val i = project.config.customIssueType(id)
    <.span(
      *.issue,
      G.hashRefKey.prefix ~ i.key.value)
  }

  private val issueDescSurroundPrefix = G.issueDescSurround.prefix.trim
  private val issueDescSurroundSuffix = G.issueDescSurround.suffix.trim
  def issue1(id: CustomIssueTypeId, desc: Text.InlineIssueDesc.NonEmptyText): ReactElement = {
    val i = project.config.customIssueType(id)
    <.span(
      *.issue,
      G.hashRefKey.prefix ~ i.key.value ~ issueDescSurroundPrefix,
      format1(desc)(*.issueDesc),
      issueDescSurroundSuffix)
  }

  val pubidColumnValue = memo[Pubid] { pubid =>
    val txt = PlainText.pubid(project, pubid)
    val req = project.reqs.reqByPubid(pubid)
    <.span(*.pubidColumnValue(req.live), txt)
  }

  private def _reqRef1(f: EndoFn[String], style: Req => TagMod): ReqId => ReactElement =
    id => {
      val req = project.reqs.req(id)
      val rt  = project.config.reqType(req.pubid.reqTypeId)
      <.span(
        style(req),
        ^.title := plainText.reqTitle(req),
        f(PlainText.pubid(rt, req.pubid.pos)))
    }

  private val _reqRef2: Contextualise => Validity => ReqId => ReactElement =
    Contextualise.memo { c =>
      val f: EndoFn[String] = c match {
        case Contextualise => G.reflinkSurround
        case Plain         => identity
      }
      Validity.memo { v2 =>
        val g = deadValidity(v2)
        val style: Req => TagMod = req => *.reqRef(g(req.live))
        memo(_reqRef1(f, style))
      }
    }

  def reqRef(c: Contextualise, validityWhenDead: Validity): ReqId => ReactElement =
    _reqRef2(c)(validityWhenDead)

  private val reqRefInText = reqRef(Contextualise, Invalid)

  private val sepComma: TagMod = ", "
  private val sepSpace: TagMod = " "

  private def list[A, B](as: Vector[A], listSep: TagMod)(f: A => B)(implicit g: B => TagMod): ReactElement =
    <.div(
      NonEmptyVector.option(as)
        .map(_.intercalateF(listSep)(g compose f).whole))

  def reqRefList(c: Contextualise, validityWhenDead: Validity)(reqs: Vector[ReqId]): ReactElement = {
    val f = reqRef(c, validityWhenDead)
    list(reqs, sepComma)(f)
  }

  def pubidRef(c: Contextualise, validityWhenDead: Validity): Pubid => ReactElement = {
    val f = reqRef(c, validityWhenDead)
    pubid => f(project.reqs.reqIdByPubid(pubid))
  }

  def pubidRefList(c: Contextualise, validityWhenDead: Validity)(ids: Vector[Pubid]): ReactElement =
    list(ids, sepComma)(pubidRef(c, validityWhenDead))

  /** Contextualised */
  val codeRef = memo[ReqCodeId] { id =>
    import ProjectText.ReqCodeResolution._
    implicit def liveWithValidity(a: Live) = invalidWhenDead(a)

    def toRef(c: ReqCode.Value, r: ReqId): ReactElement = {
      val req = project.reqs.req(r)
      ref(c, *.reqRef(req.live), plainText reqTitle req)
    }

    def toGroup(c: ReqCode.Value, g: ReqCodeGroup): ReactElement =
      ref(c, *.reqCodeGroupRef(Live), plainText.reqCodeGroupTitle(g and id))

    def ref(c: ReqCode.Value, style: StyleA, title: UndefOr[String]): ReactElement =
      <.span(
        style,
        ^.title := title,
        G.reflinkSurround(PlainText reqCode c))

    ProjectText.resolveReqCode(id, project.reqCodes) match {
      case ActiveCode(c, r: ReqId)        => toRef(c, r)
      case ActiveCode(c, g: ReqCodeGroup) => toGroup(c, g)
      case DeadGroup(c)                   => ref(c, *.reqCodeGroupRef(Dead), undefined)
      case ReqWithAltCode(c, r)           => toRef(c, r)
      case ReqWithoutCodes(r)             => reqRefInText(r)
    }
  }

  val reqType = memo[ReqTypeId] { id =>
    val rt = project.config.reqType(id)
    <.span(
      *.reqType(rt.live),
      ^.title := rt.name,
      rt.mnemonic.value)
  }

  val tag = memo[ApplicableTagId] { id =>
    val tag = project.config.atag(id)
    <.span(
      *.tag(tag.live),
      ^.title := tag.name,
      tag.key.value)
  }

  def tagList(ids: Vector[ApplicableTagId]): ReactElement =
    list(ids, sepSpace)(tag)

  def katex(m: Atom.PlainTextMarkup#MathTeX) =
    try
      <.span(*.math, ^.dangerouslySetInnerHtml(KaTeX renderToStringUnsafe m.value))
    catch {
      case _: Throwable => <.span(*.mathFail, UiText.mathFailed)
    }

  override val format = (input: Text.AnyOptional) => {
    import Atom._

    lazy val atom: AnyAtom => TagMod = {
      case a: Literal         # Literal       => <.span(a.value)
      case a: NewLine         # BlankLine     => <.div(*.blankLine)
      case a: TagRef          # TagRef        => tag(a.value)
      case a: PlainTextMarkup # WebAddress    => <.a(^.href := a.value, a.value)
      case a: PlainTextMarkup # EmailAddress  => <.a(^.href := "mailto:" ~ a.value, a.value)
      case a: PlainTextMarkup # MathTeX       => katex(a)
      case a: ListMarkup      # UnorderedList => <.ul(*.ul, a.items.whole.map(row => <.li(row map atom: _*)))
      case a: ReqRef          # ReqRef        => reqRefInText(a.value)
      case a: ReqRef          # CodeRef       => codeRef(a.value)
      case a: Issue           # Issue         => issueO(a.typ, a.desc)
    }

    <.span(input map atom: _*)
  }

  val reqCodeTreeIdentation =
    UnivEq.mutableHashMapMemo[NonEmptyVector[ReqCodeTreeItem.Indent], ReactElement](is =>
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

  def flatReqCodes(reqCodes: Vector[ReqCode.Value]): TagMod =
    reqCodes map flatReqCode

  def reqCodes(tree: Vector[ReqCodeTreeItem], flat: Vector[ReqCode.Value]): ReactElement =
    <.div(
      if (tree.nonEmpty)
        reqCodeTree(tree)
      else
        flatReqCodes(flat)
    )
}
