package shipreq.webapp.client.app.ui

import scalacss.ScalaCssReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import scalacss.StyleA
import scalajs.js.{undefined, UndefOr}
import shipreq.webapp.base.UiText
import shipreq.base.util.{NonEmptyVector, Must, UnivEq}
import shipreq.base.util.SafeStringOps._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Grammar => G, _}
import shipreq.webapp.base.util.ReqCodeTreeItem
import shipreq.webapp.client.app.ui.Style.{widgets => *}
import shipreq.webapp.client.lib.ui.UI
import shipreq.webapp.client.util.KaTeX

object ProjectWidgets {
  def apply(project: Project, plainText: PlainText.ForProject) =
    new ProjectWidgets(project, plainText)
}

final class ProjectWidgets private(project: Project, plainText: PlainText.ForProject) extends ProjectText[ReactTag](project) {
  type Widget = ReactComponentC.ConstProps[Unit, Unit, Unit, TopNode]

  private implicit def surroundDisplay(s: G.Surrounds) = s.display

  private def memo[A: UnivEq](n: String, f: A => ReactElement): A => Widget =
    UnivEq.mutableHashMapMemo((a: A) => ReactComponentB.static(n, f(a)).buildU)

  private def memoM[A: UnivEq](n: String, f: A => Must[ReactElement]): A => Widget =
    memo(n, a => UI.mustA(f(a)))

  private def memoMW[A: UnivEq](f: A => Must[Widget]): A => Widget =
    UnivEq.mutableHashMapMemo(a =>
      UI.mustA[Widget, Widget](f(a))(err => ReactComponentB.static("", err).buildU, identity))

  def issueO(id: CustomIssueTypeId, desc: Text.InlineIssueDesc.OptionalText): ReactElement =
    NonEmptyVector.maybe(desc, issue(id)(): ReactElement)(issue1(id, _))

  val issue = memoM[CustomIssueTypeId]("Issue", id =>
    project.customIssueType(id).map(i =>
      <.span(
        *.issue,
        G.hashRefKey.prefix ~ i.key.value)
    ))

  private val issueDescSurroundPrefix = G.issueDescSurround.prefix.trim
  private val issueDescSurroundSuffix = G.issueDescSurround.suffix.trim
  def issue1(id: CustomIssueTypeId, desc: Text.InlineIssueDesc.NonEmptyText): ReactElement =
    UI.must(project.customIssueType(id))(i =>
      <.span(
        *.issue,
        G.hashRefKey.prefix ~ i.key.value ~ issueDescSurroundPrefix,
        format1(desc)(*.issueDesc),
        issueDescSurroundSuffix)
    )

  val pubidColumnValue = memoM[Pubid]("ID", pubid =>
    for {
      txt <- PlainText.pubid(project, pubid)
      req <- project.reqs.data.reqByPubidM(pubid)
    } yield
      <.span(*.pubidColumnValue(req.alive), txt)
  )

  private def _reqRef(f: String => String): ReqId => Must[ReactElement] = id =>
    for {
      req <- project.reqs.data.reqM(id)
      rt  <- project.reqType(req.pubid.reqTypeId)
    } yield
      <.span(
        *.reqRef(req.alive),
        ^.title := plainText.reqTitle(req),
        f(PlainText.pubid(rt, req.pubid.pos)))

  val reqRef: Boolean => ReqId => Widget = {
    val wo = memoM[ReqId]("Req", _reqRef(identity))
    val w  = memoM[ReqId]("Req", _reqRef(G.reflinkSurround))
    (surround: Boolean) => if (surround) w else wo
  }

  private val reqRefS = reqRef(true)

  private val listSep: TagMod = ", "

  def list[A](as: Vector[A])(f: A => TagMod): ReactElement =
    <.div(
      NonEmptyVector.option(as)
        .map(_.intercalateF(listSep)(f).whole))

  def reqRefList(surround: Boolean, reqs: Vector[ReqId]): ReactElement = {
    val f = reqRef(surround)
    list(reqs)(f(_)())
  }

  def pubidRef(surround: Boolean): Pubid => ReactElement = {
    val f = reqRef(surround)
    pubid => UI.must[ReqId, ReactElement](project.reqs.data.reqIdByPubidM(pubid))(f(_)())
  }

  def pubidRefList(surround: Boolean, ids: Vector[Pubid]): ReactElement =
    list(ids)(pubidRef(surround)(_))

  val codeRef = memoM[ReqCodeId]("CodeRef", id => {
    import Must.Auto._
    import ProjectText.ReqCodeResolution._

    def toRef(c: ReqCode.Value, r: ReqId): Must[ReactElement] =
      for (req <- project.reqs.data.reqM(r))
        yield ref(c, *.reqRef(req.alive), plainText reqTitle req)

    def toGroup(c: ReqCode.Value, g: ReqCodeGroup): ReactElement =
      ref(c, *.groupRef(Alive), UiText mustA plainText.reqCodeGroupTitle(id, g))

    def ref(c: ReqCode.Value, style: StyleA, title: UndefOr[String]): ReactElement =
      <.span(
        style,
        ^.title := title,
        G.reflinkSurround(PlainText reqCode c))

    ProjectText.resolveReqCode(id, project.reqCodes.data).flatMap {
      case ActiveCode(c, r: ReqId)        => toRef(c, r)
      case ActiveCode(c, g: ReqCodeGroup) => toGroup(c, g)
      case DeadGroup(c)                   => ref(c, *.groupRef(Dead), undefined)
      case ReqWithAltCode(c, r)           => toRef(c, r)
      case ReqWithoutCodes(r)             => reqRefS(r)()
    }
  })

  val reqType = memoM[ReqTypeId]("ReqType", id =>
    project.reqType(id).map(rt =>
      <.span(
        ^.title := rt.name,
        rt.mnemonic.value)
    ))

  val tag = memoM[ApplicableTagId]("Tag", id =>
    project.atag(id).map(tag =>
      <.span(
        *.tag,
        ^.title := tag.name,
        tag.key.value
      )
    ))

  def tags(tags: Vector[ApplicableTagId]): ReactElement =
    <.div(tags.map(id => tag(id)(): TagMod): _*)

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
      case a: TagRef          # TagRef        => tag(a.value)()
      case a: PlainTextMarkup # WebAddress    => <.a(^.href := a.value, a.value)
      case a: PlainTextMarkup # EmailAddress  => <.a(^.href := "mailto:" ~ a.value, a.value)
      case a: PlainTextMarkup # MathTeX       => katex(a)
      case a: ListMarkup      # UnorderedList => <.ul(*.ul, a.items.whole.map(row => <.li(row map atom: _*)))
      case a: ReqRef          # ReqRef        => reqRefS(a.value)()
      case a: ReqRef          # CodeRef       => codeRef(a.value)()
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
