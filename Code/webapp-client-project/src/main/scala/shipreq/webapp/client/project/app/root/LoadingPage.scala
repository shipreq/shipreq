package shipreq.webapp.client.project.app.root

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusable
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.data.{ProjectCatalogue, Username}
import shipreq.webapp.client.base.ui.semantic.{Breadcrumb, UsesSemanticUiManually}
import shipreq.webapp.client.base.ui.{BaseStyles, MemberNavBar}

object LoadingPage {

  final case class Props(username: Username,
                         project: ProjectCatalogue.Item) {
    @inline def render = Component(this)
  }

  def layout(p: Props)(content: VdomElement): VdomElement =
    <.div(

      MemberNavBar.Props(
        p.username,
        Reusable.never(MemberNavBar.MemberHome :: Breadcrumb.Item.Div(p.project.name) :: Nil))
        .render,

      <.div(BaseStyles.containerLarge,

        // Nope. It uses BaseStyles which aren't loaded until JS loads meaning webapp-gen can't use this.
        // ProjectItem.Component(p.project),

        content))

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
