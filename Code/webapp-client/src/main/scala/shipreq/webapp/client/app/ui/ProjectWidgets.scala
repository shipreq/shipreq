package shipreq.webapp.client.app.ui

import japgolly.scalacss.ScalaCssReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import scalaz.{NonEmptyList, Memo}
import shipreq.base.util.{Must, UnivEq}
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Presentation, Text}
import shipreq.webapp.client.app.ui.Style.{widgets => *}
import shipreq.webapp.client.lib.ui.UI
import shipreq.webapp.client.util.KaTeX

final class ProjectWidgets(project: Project) {

  private val txtToStr = Presentation.textToString(project)

  type Widget = ReactComponentC.ConstProps[Unit, Unit, Unit, TopNode]

  private def memo[A: UnivEq](n: String, f: A => ReactTag): A => Widget =
    Memo.mutableHashMapMemo((a: A) => ReactComponentB.static(n, f(a)).buildU)

  private def memoM[A: UnivEq](n: String, f: A => Must[ReactTag]): A => Widget =
    memo(n, a => UI.mustA(f(a)))

  private def memoMW[A: UnivEq](f: A => Must[Widget]): A => Widget =
    Memo.mutableHashMapMemo(a =>
      UI.mustA[Widget, Widget](f(a))(err => ReactComponentB.static("", err).buildU, identity))

  def issueO(id: CustomIssueType.Id, desc: Text.InlineIssueDesc.OptionalText): ReactElement =
    desc match {
      case Nil    => issue(id)()
      case h :: t => issue1(id, NonEmptyList.nel(h, t))
    }

  val issue = memoM[CustomIssueType.Id]("Issue", id =>
    project.customIssueType(id).map(i =>
      <.span(
        *.issue,
        s"#${i.key.value}")
    ))

  def issue1(id: CustomIssueType.Id, desc: Text.InlineIssueDesc.NonEmptyText): ReactElement =
    UI.must(project.customIssueType(id))(i =>
      <.span(
        *.issue,
        s"#${i.key.value}{",
        text1(desc, *.issueDesc),
        "}")
    )

  val pubidText = memoM[Pubid]("ID", pubid =>
    Presentation.pubid(pubid)(project) map (<.span(_))
  )

  val reqDesc: Req => String = {
    val m = new scala.collection.mutable.HashMap[Req.Id, String]
    req =>
      m.getOrElseUpdate(req.id, req match {
        case r: GenericReq => txtToStr(r.desc)
      })
  }

  val reqRef = memoM[Req.Id]("Req", id =>
    for {
      req <- project.reqs.data.reqM(id)
      rt  <- project.reqType(req.pubid.reqTypeId)
    } yield
      <.span(
        *.reqRef(req.alive),
        ^.title := reqDesc(req),
        s"[${Presentation.pubid(rt, req.pubid.pos)}]")
    )

  def reqRefs(reqs: Vector[Req.Id]): ReactElement =
    <.div(reqs.map(id => reqRef(id)(): TagMod): _*)

  val pubidRef = memoMW[Pubid](pubid =>
    project.reqs.data.reqIdByPubidM(pubid) map reqRef)

  def pubidRefs(ids: Vector[Pubid]): ReactElement =
    <.div(ids.map(id => pubidRef(id)(): TagMod): _*)

  val reqType = memoM[ReqType.Id]("ReqType", id =>
    project.reqType(id).map(rt =>
      <.span(
        ^.title := rt.name,
        s"${rt.mnemonic.value}")
    ))

  val tag = memoM[ApplicableTag.Id]("Tag", id =>
    project.atag(id).map(tag =>
      <.span(
        *.tag,
        ^.title := tag.name,
        tag.key.value
      )
    ))

  def tags(tags: Vector[ApplicableTag.Id]): ReactElement =
    <.div(tags.map(id => tag(id)(): TagMod): _*)

  // TODO move
  def text1(t: Text.Generic#NonEmptyText, style: TagMod = EmptyTag): ReactElement = text(t.list, style)
  def text(t: Text.Generic#OptionalText, style: TagMod = EmptyTag): ReactElement = {
    import Text._
    import Text.Generic._

    lazy val atom: Generic#Atom => TagMod = {
      case a: Literal         # Literal       => <.span(a.value)
      case a: NewLine         # NewLine       => <.br
      case a: TagRef          # TagRef        => tag(a.value)()
      case a: PlainTextMarkup # WebAddress    => <.a(^.href := a.value, a.value)
      case a: PlainTextMarkup # EmailAddress  => <.a(^.href := s"mailto:${a.value}", a.value)
      case a: PlainTextMarkup # MathTeX       => <.span(*.math, ^.dangerouslySetInnerHtml(KaTeX renderToString a.value))
      case a: ListMarkup      # UnorderedList => <.ul(a.items.list.map(row => <.li(row map atom: _*)): _*)
      case a: ReqRef          # ReqRef        => reqRef(a.value)()
      case a: Issue           # Issue         => issueO(a.typ, a.desc)
    }

    <.span(style)(t map atom: _*)
  }
}
