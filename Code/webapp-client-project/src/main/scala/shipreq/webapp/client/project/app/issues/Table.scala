package shipreq.webapp.client.project.app.issues

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._

object Table {

  final case class Props() {
    @inline def render: VdomElement = Component(this)
  }

  //implicit val reusabilityProps: Reusability[Props] =
  //  Reusability.caseClass

  final class Backend($: BackendScope[Props, Unit]) {
    def render(p: Props): VdomElement =
      <.div("[TODO: Table]")
  }

  val Component = ScalaComponent.builder[Props]("Table")
    .renderBackend[Backend]
    //.configure(Reusability.shouldComponentUpdate)
    .build
}