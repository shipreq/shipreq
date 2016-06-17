package shipreq.webapp.client.project.app.root

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import shipreq.webapp.base.data.{ProjectCatalogue, Username}
import shipreq.webapp.client.base.ui.semantic.{Breadcrumb, UsesSemanticUiManually}
import shipreq.webapp.client.base.ui.{BaseStyles, MemberNavBar, inlineStyle}

object LoadingPage {

  final case class Props(username: Username,
                         project: ProjectCatalogue.Item) {
    @inline def render = Component(this)
  }

  @UsesSemanticUiManually
  final class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props): ReactElement =
      <.div(

        MemberNavBar.Props(
          p.username,
          MemberNavBar.MemberHome :: Breadcrumb.Item.Div(p.project.name) :: Nil,
          Nil)
          .render,

        <.div(BaseStyles.maxWidthContainer,

          // Nope. It uses BaseStyles which aren't loaded until JS loads meaning webapp-gen can't use this.
          // ProjectItem.Component(p.project),

          <.div(
            ^.cls := "ui segment basic",
            inlineStyle(_.literal("height" -> "20rem")),
            <.div(
              ^.cls := "ui text loader large active",
              "Loading..."))))
  }

  val Component = ReactComponentB[Props]("LoadingPage")
    .renderBackend[Backend]
    .build
}
