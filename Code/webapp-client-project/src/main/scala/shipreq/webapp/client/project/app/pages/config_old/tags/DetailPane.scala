package shipreq.webapp.client.project.app.pages.config_old.tags

import japgolly.scalajs.react._, vdom.html_<^._
import japgolly.microlibs.nonempty.NonEmptyVector
import scalaz.Equal
import scalaz.std.option.optionEqual
import shipreq.webapp.base.data.{TagId => Id, _}
import shipreq.webapp.base.data.{Disabled, Enabled}
import shipreq.webapp.client.project.lib.DND
import shipreq.webapp.client.project.widgets.SelectOne
import SelectOne.Choice

private[tags] object DetailPane {

  case class Rel(id: Id, name: String, unlink: Callback)
  implicit val relEquivalence = Equal.equalBy((_: Rel).id)
  type Rels = Seq[Rel]

  case class AddRel(value: FlatTag, selectable: Option[Id])
  case class AddSelected(id: Id, onAdd: Callback)
  case class AddRels(rels: Vector[AddRel], onSelect: Option[Id] => Callback, selected: Option[AddSelected])

  case class Props(subjName: String,
                   parents: Rels, parentAdds: AddRels,
                   children: Rels, childAdds: AddRels, childMoveIO: (Id, Id) => Callback)

  type State = DND.Parent.PState[Rel]

  val DraggableRel = DND.Child.dndItemComponent[Rel]((outerAttr, hnd, rel) =>
    <.div(outerAttr,
      renderRel(rel, Some(hnd))))

  def renderRel(r: Rel, dragHandle: Option[VdomTag]): VdomTag =
    <.li(
      ^.key := r.id.value,
      dragHandle.whenDefined,
      r.name,
      <.button(
        ^.marginLeft := "2ex",
        ^.onClick --> r.unlink,
        "Remove"))

  val relDropdownComponent = SelectOne.Component[Option[Id]]
  val emptyRelChoice       = Choice[Option[Id]](None, "", Enabled)

  final class Backend($: BackendScope[Props, State]) {

    def render(p: Props): VdomElement = {
      def parentsPane: VdomElement =
        pane(
          existingRels(p.parents.sortBy(_.name), <.ul, "This has no parents.", renderRel(_, None)),
          addableRels(p.parentAdds, "Add Parent"))

      def childrenPane: VdomElement =
        pane(
          existingRels(p.children, <.ol, "This has no children.",
            r => DraggableRel(DND.Parent.cProps2($, r, (from, to) => p.childMoveIO(from.id, to.id)))),
          addableRels(p.childAdds, "Add Child"))

      <.section(
        <.h3(s"Detail: ${p.subjName}"),
        <.table(
          <.thead(<.tr(
            <.th("Parents"),
            <.th("Children"))),
          <.tbody(<.tr(
            <.td(parentsPane),
            <.td(childrenPane)))))
    }

    def pane(existing: VdomElement, addable: TagMod): VdomElement =
      <.div(existing, addable)

    def existingRels(rels: Rels, container: VdomTag, noneMsg: String, li: Rel => VdomElement): VdomElement =
      if (rels.isEmpty)
        <.div(noneMsg)
      else
        container(rels.toTagMod(li))

    def addableRels(ar: AddRels, buttonLabel: String): TagMod =
      if (ar.rels.isEmpty)
        EmptyVdom
      else {

        val choices = NonEmptyVector(
          emptyRelChoice,
          ar.rels.map { r =>
            val s = r.selectable
            Choice[Option[Id]](s, r.value.indentedName, Disabled when s.isEmpty)
          })

        val dropdown =
          relDropdownComponent(SelectOne.Props(ar.selected.map(_.id), choices, Some(ar.onSelect)))

        val addButton = {
          val b = <.button(^.marginLeft := 1.ex, buttonLabel)
          ar.selected match {
            case Some(s) => b(^.onClick --> s.onAdd)
            case None    => b(^.disabled := true)
          }
        }

        <.div(^.marginTop := 1.ex, dropdown, addButton)
      }
  }

  val Component = ScalaComponent.builder[Props]("Cfg: Tag Detail")
    .initialState[State](DND.Parent.initialState)
    .renderBackend[Backend]
    .build
}
