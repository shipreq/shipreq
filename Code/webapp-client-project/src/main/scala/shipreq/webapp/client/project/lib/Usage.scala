package shipreq.webapp.client.project.lib

import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import shipreq.webapp.client.project.app.pages.root.SpecialRouterCtl
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data._
import shipreq.webapp.base.filter.Filter
import shipreq.webapp.client.project.app.Style

object Usage {

  private val hiddenS =
    <.span(^.visibility.hidden, "s")

  def tagMod(uses: Int): TagMod =
    TagMod(
      TagMod.when(uses == 0)(Style.usageZero),
      if (uses == 1)
        TagMod(uses + " use", hiddenS)
      else
        uses + " uses",
    )

  def tags(id        : ApplicableTagId,
           fd        : FilterDead,
           usageStats: LiveDeadStatMap[ApplicableTagId, Int],
           router    : SpecialRouterCtl): VdomTagOf[html.Anchor] = {
    val uses = usageStats(id)(fd)
    val rc   = router.reqTableWithFilter(fd, Filter.Valid.tag(id))
    rc.link(())(tagMod(uses))
  }

}
