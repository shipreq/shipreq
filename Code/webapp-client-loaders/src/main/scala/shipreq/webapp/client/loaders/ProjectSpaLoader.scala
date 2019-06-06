package shipreq.webapp.client.loaders

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.ui._
import shipreq.webapp.base.ui.semantic.Breadcrumb
import shipreq.webapp.base.user.Username

object ProjectSpaLoader {

  final case class Props(username: Username, projectName: Project.Name) {
    @inline def render: VdomElement = Component(this)
  }

  def layout(p: Props)(content: VdomNode): VdomElement = {
    val navBar = MemberNavBar.Props(
      p.username,
      Reusable.never(MemberNavBar.MemberHome :: Breadcrumb.Item.Div(p.projectName) :: Nil))

    Loader.render(navBar)(content)
  }

  private def render(p: Props): VdomElement =
    layout(p)(Loader.loading)

  val Component = ScalaFnComponent[Props](render)
}
