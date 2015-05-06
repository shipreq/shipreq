package shipreq.webapp.client.app.ui

import scalacss.ScalaCssReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import shipreq.webapp.base.UiText
import shipreq.base.util.{NonEmptyVector, Must, UnivEq}
import shipreq.webapp.base.data._
import shipreq.webapp.base.text._
import shipreq.webapp.base.util.ReqCodeTreeItem
import shipreq.webapp.client.app.ui.Style.{widgets => *}
import shipreq.webapp.client.lib.ui.UI
import shipreq.webapp.client.util.KaTeX

object ProjectWidgets {
  def apply (project: Project, plainText: PlainText.ForProject) =
    new ProjectWidgets(project, plainText)
}

// TODO Maybe make project & plainText vals to reduce shit being passed around
final class ProjectWidgets private(project: Project, plainText: PlainText.ForProject) extends ProjectText[ReactTag](project) {
  type Widget = ReactComponentC.ConstProps[Unit, Unit, Unit, TopNode]

  private def memo[A: UnivEq](n: String, f: A => ReactTag): A => Widget =
    UnivEq.mutableHashMapMemo((a: A) => ReactComponentB.static(n, f(a)).buildU)

  private def memoM[A: UnivEq](n: String, f: A => Must[ReactTag]): A => Widget =
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
        s"#${i.key.value}") // TODO use grammar
    ))

  def issue1(id: CustomIssueTypeId, desc: Text.InlineIssueDesc.NonEmptyText): ReactElement =
    UI.must(project.customIssueType(id))(i =>
      <.span(
        *.issue,
        s"#${i.key.value}{",
        format1(desc)(*.issueDesc),
        "}") // TODO use grammar
    )

  val pubidText = memoM[Pubid]("ID", pubid =>
    PlainText.pubid(project, pubid) map (<.span(_))
  )

  val reqRef = memoM[ReqId]("Req", id =>
    for {
      req <- project.reqs.data.reqM(id)
      rt  <- project.reqType(req.pubid.reqTypeId)
    } yield
      <.span(
        *.reqRef(req.alive),
        ^.title := plainText.reqTitle(req),
        s"[${PlainText.pubid(rt, req.pubid.pos)}]") // TODO use grammar
    )

  def reqRefs(reqs: Vector[ReqId]): ReactElement =
    <.div(reqs.map(id => reqRef(id)(): TagMod): _*)

  val pubidRef = memoMW[Pubid](pubid =>
    project.reqs.data.reqIdByPubidM(pubid) map reqRef)

  def pubidRefs(ids: Vector[Pubid]): ReactElement =
    <.div(ids.map(id => pubidRef(id)(): TagMod): _*)

  val reqType = memoM[ReqTypeId]("ReqType", id =>
    project.reqType(id).map(rt =>
      <.span(
        ^.title := rt.name,
        s"${rt.mnemonic.value}")
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
      case a: PlainTextMarkup # EmailAddress  => <.a(^.href := s"mailto:${a.value}", a.value)
      case a: PlainTextMarkup # MathTeX       => katex(a)
      case a: ListMarkup      # UnorderedList => <.ul(*.ul, a.items.whole.map(row => <.li(row map atom: _*)))
      case a: ReqRef          # ReqRef        => reqRef(a.value)()
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
      code = Grammar.reqCode.nodeSeparator + code
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
