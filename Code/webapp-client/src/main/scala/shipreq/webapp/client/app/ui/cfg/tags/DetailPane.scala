package shipreq.webapp.client.app.ui.cfg.tags

import japgolly.scalajs.react._, vdom.prefix_<^.{Tag => ReactTag, Modifier => TagMod, _}, ScalazReact._
import scalaz.Equal
import scalaz.effect.IO
import shipreq.webapp.base.data._
import shipreq.webapp.client.util.DND
import Tag.Id

private[tags] object DetailPane {

  case class Rel(id: Id, name: String, unlink: IO[Unit])

  implicit val relEquivalence = Equal.equalBy((_: Rel).id)

  type Rels = Seq[Rel]

  case class Props(descEditor: ReactElement, children: Rels, parents: Rels, moveChildIO: (Id, Id) => IO[Unit])

  type State = DND.Parent.PState[Rel]

  val DraggableRel = DND.Child.dndItemComponent[Rel](
    (r, h) => renderRel(r, Some(h)))

  def renderRel(r: Rel, dragHandle: Option[ReactTag]): ReactTag =
    <.li(
      ^.key := r.id.value,
      dragHandle,
      r.name,
      <.button(
        ^.marginLeft := "2ex",
        ^.onclick ~~> r.unlink,
        "Remove"))

  class Backend(c: BackendScope[Props, State]) {
    @inline def p = c.props

    def render: ReactElement =
      <.section(
        descPane,
        <.table(
          <.thead(<.tr(
            <.th("Parents"),
            <.th("Children"))),
          <.tbody(<.tr(
            <.td(parentsPane),
            <.td(childrenPane)))))

    def descPane: ReactElement =
      <.label(
        "Description:",
        p.descEditor)

    def parentsPane: ReactElement =
      relList(p.parents.sortBy(_.name), <.ul, "This is a top-level tag.", renderRel(_, None))

    def childrenPane: ReactElement =
      relList(p.children, <.ol, "This has no children.", r => DraggableRel(DND.Parent.cProps2(c, r, moveChildIO)))

    def moveChildIO(from: Rel, to: Rel): IO[Unit] =
      p.moveChildIO(from.id, to.id)

    def relList(rels: Rels, container: ReactTag, noneMsg: String, li: Rel => ReactElement): ReactElement =
      if (rels.isEmpty)
        <.div(noneMsg)
      else
        container(rels map li)
  }

  val Component = ReactComponentB[Props]("Cfg: Tag Detail")
    .initialState[State](DND.Parent.initialState)
    .backend(new Backend(_))
    .render(_.backend.render)
    .build
}