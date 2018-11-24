package shipreq.webapp.client.project.app.root

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusable
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.ui.semantic.{Breadcrumb, UsesSemanticUiManually}
import shipreq.webapp.base.ui._
import shipreq.webapp.base.user.Username

object LoadingPage {

  final case class Props(username: Username, projectName: Project.Name) {
    @inline def render: VdomElement = Component(this)
  }

  def layout(p: Props)(content: VdomElement): VdomElement = {

    val navBar = MemberNavBar.Props(
      p.username,
      Reusable.never(MemberNavBar.MemberHome :: Breadcrumb.Item.Div(p.projectName) :: Nil))

    def mainContent(m: TagMod): VdomElement =
      <.div(m, BaseStyles.containerLarge,

        // ↓ Nope ↓ - It uses BaseStyles which aren't loaded until JS loads meaning webapp-ssr can't use this.
        // ProjectItem.Component(p.project),

        content)

    MemberLayout.Props(navBar, mainContent).render
  }

  @UsesSemanticUiManually
  def render(p: Props): VdomElement =
    layout(p)(
      <.div(
        ^.cls := "ui segment basic",
        ^.height := "20rem",
        <.div(
          ^.cls := "ui text loader large active",
          "Loading...")))

  val Component = ScalaFnComponent[Props](render)
}
