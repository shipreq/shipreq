package shipreq.webapp.client.project.app.pages.config.tags

import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data.{Disabled, Enabled, TagGroup}
import shipreq.webapp.base.feature.DragToReorderFeature
import shipreq.webapp.base.ui.semantic.Icon
import shipreq.webapp.client.project.app.Style.{tagConfig => *}

private[tags] object Shared {

  private val groupIcon =
    Icon.FolderOpen.tag(*.tagTreeGroupIcon)

  def group(g: TagGroup) =
    <.span(
      Shared.groupIcon,
      g.name,
      g.desc.whenDefined(^.title := _),
    )

  private val dragHandle: Enabled => VdomTag =
    Enabled.memo(e =>
      DragToReorderFeature.dragHandle(*.dragHandle(e)))

  def dragHandle(item: DragToReorderFeature.Item[Any], enabled: Enabled): TagMod =
    enabled match {
      case Enabled  => TagMod(dragHandle(Enabled)(item.source), item.target)
      case Disabled => dragHandle(Disabled)
    }

}
