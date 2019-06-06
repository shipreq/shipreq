package shipreq.webapp.client.loaders

import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.ui.{InlineBaseStyles, MemberLayout, MemberNavBar}
import shipreq.webapp.base.ui.semantic.UsesSemanticUiManually

private[loaders] object Loader {

  def render(navBar: MemberNavBar.Props)(content: VdomNode): VdomElement = {

    def mainContent(m: TagMod): VdomElement =
      <.div(m,
        InlineBaseStyles.containerLarge,
        content)

    MemberLayout.Props(navBar, mainContent).render
  }

  @UsesSemanticUiManually
  def loading =
    <.div(
      ^.cls := "ui segment basic",
      ^.height := "20rem",
      <.div(
        ^.cls := "ui text loader large active",
        "Loading..."))

}
