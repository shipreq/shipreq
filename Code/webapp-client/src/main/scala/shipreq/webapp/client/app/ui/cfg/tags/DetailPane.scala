package shipreq.webapp.client.app.ui.cfg.tags

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import org.scalajs.dom.HTMLSelectElement
import shipreq.base.util.Util
import scalaz.{Equal, Memo}
import scalaz.effect.IO

import shipreq.webapp.base.data._
import shipreq.webapp.client.util.DND
import Tag.Id

private[tags] object DetailPane {

  case class Rel(id: Id, name: String, unlink: IO[Unit])
  implicit val relEquivalence = Equal.equalBy((_: Rel).id)
  type Rels = Seq[Rel]

  case class AddRel(name: String, depth: Int, selectable: Option[Id])
  case class AddSelected(id: Id, onAdd: IO[Unit])
  case class AddRels(rels: Vector[AddRel], onSelect: Option[Id] => IO[Unit], selected: Option[AddSelected])

  case class Props(subjName: String,
                   parents: Rels, parentAdds: AddRels,
                   children: Rels, childAdds: AddRels, childMoveIO: (Id, Id) => IO[Unit])

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
        ^.onClick ~~> r.unlink,
        "Remove"))

  class Backend(c: BackendScope[Props, State]) {
    @inline def p = c.props

    def render: ReactElement =
      <.section(
        <.h3(s"Detail: ${p.subjName}"),
        <.table(
          <.thead(<.tr(
            <.th("Parents"),
            <.th("Children"))),
          <.tbody(<.tr(
            <.td(parentsPane),
            <.td(childrenPane)))))

    def parentsPane: ReactElement =
      pane(
        existingRels(p.parents.sortBy(_.name), <.ul, "This has no parents.", renderRel(_, None)),
        addableRels(p.parentAdds, "Add Parent"))

    def childrenPane: ReactElement =
      pane(
        existingRels(p.children, <.ol, "This has no children.",
          r => DraggableRel(DND.Parent.cProps2(c, r, (from, to) => p.childMoveIO(from.id, to.id)))),
        addableRels(p.childAdds, "Add Child"))

    def pane(existing: ReactElement, addable: TagMod): ReactElement =
      <.div(existing, addable)

    def existingRels(rels: Rels, container: ReactTag, noneMsg: String, li: Rel => ReactElement): ReactElement =
      if (rels.isEmpty)
        <.div(noneMsg)
      else
        container(rels map li)

    def addableRels(ar: AddRels, buttonLabel: String): TagMod =
      if (ar.rels.isEmpty)
        EmptyTag
      else {

        def dropdownChange: SyntheticEvent[HTMLSelectElement] => IO[Unit] =
          e => ar.onSelect(Util.parseLong(e.target.value).map(Id.apply))

        def option(r: AddRel) = {
          val base = r.selectable match {
            case Some(id) => <.option(^.value := id.value)
            case None     => <.option(^.disabled := true)
          }
          base(s"${indentation(r.depth)}${r.name}")
        }

        val dropdown =
          <.select(
            ^.value := ar.selected.map(_.id.value.toString).getOrElse("∅"),
            ^.onChange ~~> dropdownChange,
            <.option(^.value := "∅"),
            ar.rels.map(option))

        val addButton = {
          val b = <.button(^.marginLeft := 1.ex, buttonLabel)
          ar.selected match {
            case Some(s) => b(^.onClick ~~> s.onAdd)
            case None    => b(^.disabled := true)
          }
        }

        <.div(^.marginTop := 1.ex, dropdown, addButton)
      }
  }

  private[this] val indentation =
    Memo.immutableHashMapMemo[Int, String]("\u00A0\u00A0" * _)

  val Component = ReactComponentB[Props]("Cfg: Tag Detail")
    .initialState[State](DND.Parent.initialState)
    .backend(new Backend(_))
    .render(_.backend.render)
    .build
}