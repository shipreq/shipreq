package shipreq.webapp.client.project.app.pages.admin.access

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.base.util.{Enabled, NonEmptyArraySeq}
import shipreq.webapp.base.data.ProjectPerm
import shipreq.webapp.base.ui.widgets.Dropdown

object PermDropdown {

  type Item = Dropdown.Item[ProjectPerm]

  private def mkKey(perm: ProjectPerm): Dropdown.ItemKey =
    perm.ord.toString

  private val mkLabel: ProjectPerm => String = {
    case ProjectPerm.Admin        => "Admin"
    case ProjectPerm.Collaborator => "Collaborator"
  }

  private def mkItem(perm: ProjectPerm): Item =
    Dropdown.Item(
      key   = mkKey(perm),
      label = mkLabel(perm),
      value = perm,
    )

  val items: NonEmptyArraySeq[Item] =
    NonEmptyArraySeq.fromNEV(ProjectPerm.values.map(mkItem))

  def apply(selected: ProjectPerm, enabled: Enabled, onChange: Item => Callback): Dropdown.Props.NonEmpty =
    Dropdown.Props.NonEmpty(
      items,
      mkKey(selected),
      enabled)(
      onChange)

}
