package shipreq.webapp.client.app.ui.cfg.tags

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import scalaz.Equal
import scalaz.effect.IO
import scalaz.std.option.optionEqual
import shipreq.webapp.base.data._
import shipreq.webapp.client.app.ui.SelectOne
import shipreq.webapp.client.util.DND
import SelectOne.Choice
import Tag.Id
import TagTree.FlatRow

private[tags] object DetailPane {

  case class Rel(id: Id, name: String, unlink: IO[Unit])
  implicit val relEquivalence = Equal.equalBy((_: Rel).id)
  type Rels = Seq[Rel]

  case class AddRel(value: FlatRow, selectable: Option[Id])
  case class AddSelected(id: Id, onAdd: IO[Unit])
  case class AddRels(rels: Vector[AddRel], onSelect: Option[Id] => IO[Unit], selected: Option[AddSelected])

  case class Props(subjName: String,
                   parents: Rels, parentAdds: AddRels,
                   children: Rels, childAdds: AddRels, childMoveIO: (Id, Id) => IO[Unit])

  type State = DND.Parent.PState[Rel]

  val DraggableRel = DND.Child.dndItemComponent[Rel]((outerAttr, hnd, rel) =>
    <.div(outerAttr,
      renderRel(rel, Some(hnd))))

  def renderRel(r: Rel, dragHandle: Option[ReactTag]): ReactTag =
    <.li(
      ^.key := r.id.value,
      dragHandle,
      r.name,
      <.button(
        ^.marginLeft := "2ex",
        ^.onClick ~~> r.unlink,
        "Remove"))

  val relDropdownComponent = SelectOne.Component[Option[Id]]
  val emptyRelChoice       = Choice[Option[Id]](None, "", disabled = false)

  final class Backend($: BackendScope[Props, State]) {
    @inline def p = $.props

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
          r => DraggableRel(DND.Parent.cProps2($, r, (from, to) => p.childMoveIO(from.id, to.id)))),
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

        val choices =
          emptyRelChoice +:
          ar.rels.map { r =>
            val s = r.selectable
            Choice[Option[Id]](s, r.value.indentedName, disabled = s.isEmpty)
          }

        val dropdown =
          relDropdownComponent(SelectOne.Props(ar.selected.map(_.id), choices, Some(ar.onSelect)))

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

  val Component = ReactComponentB[Props]("Cfg: Tag Detail")
    .initialState[State](DND.Parent.initialState)
    .backend(new Backend(_))
    .render(_.backend.render)
    .build
}
