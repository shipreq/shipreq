package shipreq.webapp.client.project.app.pages.root

import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.extra.router.RouterCtl
import shipreq.webapp.member.project.data.FilterDead
import shipreq.webapp.member.project.filter.Filter

/** "Special" as in "specialised", where a general [[RouterCtl]] doesn't have features specific to this particular SPA.
 */
trait SpecialRouterCtl {
  val general: RouterCtl[Routes.Page]
  def reqTableWithFilter(fd: FilterDead, filter: => Filter.Valid): RouterCtl[Unit]
}

object SpecialRouterCtl {
  implicit val reusability: Reusability[SpecialRouterCtl] =
    Reusability.byRef
}