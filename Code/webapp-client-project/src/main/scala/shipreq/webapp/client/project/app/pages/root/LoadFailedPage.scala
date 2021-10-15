package shipreq.webapp.client.project.app.pages.root

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.ui.widgets.NoContentMessage
import shipreq.webapp.client.loaders.ProjectSpaLoader

object LoadFailedPage {

  final case class Props(lp: ProjectSpaLoader.Props, error: ErrorMsg)

  def render(p: Props): VdomElement = {

    val msg = NoContentMessage.becauseAnErrorOccurred(
      "Error loading project",
      TagMod(^.whiteSpace.preWrap, p.error.value))

    ProjectSpaLoader.layout(p.lp)(
      <.div(^.paddingTop := "4rem", msg))
  }

  val Component = ScalaFnComponent[Props](render)
}
