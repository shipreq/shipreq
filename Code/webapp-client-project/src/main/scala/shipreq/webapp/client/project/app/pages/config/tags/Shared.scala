package shipreq.webapp.client.project.app.pages.config.tags

import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data.TagGroup
import shipreq.webapp.base.ui.semantic.Icon
import shipreq.webapp.client.project.app.Style.{tagConfig => *}

private[tags] object Shared {

  private val groupIcon =
    Icon.FolderOpen.tag(*.tagTreeGroupIcon)

  def group(g: TagGroup) =
    <.div(
      Shared.groupIcon,
      g.name,
      g.desc.whenDefined(^.title := _),
    )

}
