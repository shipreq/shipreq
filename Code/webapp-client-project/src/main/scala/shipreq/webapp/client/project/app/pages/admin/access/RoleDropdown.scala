package shipreq.webapp.client.project.app.pages.admin.access

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.base.util.{Enabled, NonEmptyArraySeq}
import shipreq.webapp.base.data.ProjectRole
import shipreq.webapp.base.ui.widgets.Dropdown

object RoleDropdown {

  type Item = Dropdown.Item[ProjectRole]

  private def mkKey(role: ProjectRole): Dropdown.ItemKey =
    role.ord.toString

  private val mkLabel: ProjectRole => String = {
    case ProjectRole.Admin        => "Admin"
    case ProjectRole.Collaborator => "Collaborator"
  }

  private def mkItem(role: ProjectRole): Item =
    Dropdown.Item(
      key   = mkKey(role),
      label = mkLabel(role),
      value = role,
    )

  val items: NonEmptyArraySeq[Item] =
    NonEmptyArraySeq.fromNEV(ProjectRole.values.map(mkItem))

  def apply(selected: ProjectRole, enabled: Enabled, onChange: Item => Callback): Dropdown.Props.NonEmpty =
    Dropdown.Props.NonEmpty(
      items,
      mkKey(selected),
      enabled)(
      onChange)

}
