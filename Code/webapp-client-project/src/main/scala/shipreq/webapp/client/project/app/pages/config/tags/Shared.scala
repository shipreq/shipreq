package shipreq.webapp.client.project.app.pages.config.tags

import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util.{Disabled, Enabled}
import shipreq.webapp.base.feature.DragToReorderFeature
import shipreq.webapp.base.ui.semantic.Icon
import shipreq.webapp.client.project.app.Style.{tagConfig => *}
import shipreq.webapp.member.project.data._

private[tags] object Shared {

  private val groupIcon =
    Icon.FolderOpen.tag(*.tagTreeGroupIcon)

  def group(g: TagGroup) =
    <.span(
      *.group(g.live),
      Shared.groupIcon,
      g.name,
      g.desc.whenDefined(^.title := _),
    )

  private val dragHandle: Enabled => Live => VdomTag =
    Enabled.memo(e =>
      Live.memo(l =>
        DragToReorderFeature.dragHandle(*.dragHandle((e, l)))))

  def dragHandle(item: DragToReorderFeature.Item[Any], enabled: Enabled, live: Live): TagMod =
    (enabled & Disabled.when(live is Dead)) match {
      case Enabled  => TagMod(dragHandle(Enabled)(live)(item.source), item.target)
      case Disabled => dragHandle(Disabled)(live)
    }

  val fakeApplicableTagId =
    ApplicableTagId(-1)

  val fakeApplicableTag =
    ApplicableTag(fakeApplicableTagId, HashRefKey(""), None, None, ApplicableReqTypes.empty, Live)

  val fakeApplicableTagInTree =
    TagInTree(fakeApplicableTag, Vector.empty)
}
