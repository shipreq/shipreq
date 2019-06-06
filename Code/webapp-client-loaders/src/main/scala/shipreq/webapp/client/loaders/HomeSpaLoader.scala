package shipreq.webapp.client.loaders

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.ClientConfig
import shipreq.webapp.base.ui._
import shipreq.webapp.base.ui.semantic.Breadcrumb
import shipreq.webapp.base.user.Username

object HomeSpaLoader {

  final case class Props(username: Username) {
    @inline def render: VdomElement = Component(this)
  }

  private val navBarLeft: MemberNavBar.LeftProps =
    Reusable.byRef(Breadcrumb.Item.Div(ClientConfig.BreadcrumbNameMemberHome) :: Nil)

  private def render(p: Props): VdomElement = {
    val navBar = MemberNavBar.Props(p.username, navBarLeft)
    Loader.render(navBar)(EmptyVdom)
  }

  val Component = ScalaFnComponent[Props](render)
}
