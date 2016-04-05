package shipreq.webapp.client.app.reqdetail.uce

import shipreq.webapp.base.text.Text
import shipreq.webapp.client.app.Style.reqdetail.{useCaseStep => *}
import scalacss.ScalaCssReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.client.widgets.high.ProjectWidgets
import shipreq.webapp.client.feature.{AsyncActionFeature, ContentEditorFeature}
import shipreq.webapp.client.lib.DataReusability._

object StepText {

  case class Props(step      : UseCaseStep,
                   defaultTitle: Text.UseCaseTitle.OptionalText,
                   flow      : UseCases.StepFlow,
                   widgets   : ProjectWidgets,
                   editState : ContentEditorFeature.D0.State,
                   asyncState: AsyncActionFeature.D0.State[String],
                   startEdit : Callback) {
    @inline def id = step.id
    @inline def render = Component(this) //.withKey(step.id.value)(this)
  }

  implicit val propsReuse: Reusability[Props] = {
    val most = Reusability.caseClassExcept[Props]('flow, 'startEdit)

    val flow = Reusability.fn[Props]((x, y) =>
      (x.flow eq y.flow) || Direction.forall(d => x.flow(d)(x.id) ==* y.flow(d)(y.id)))

    most && flow
  }

  final class Backend($: BackendScope[Props, Unit]) {
    def render(p: Props) = {

      def body = {
        // TODO Missing flow in text --------------------------------------------------------------------------------
        def view: ReactNode = {
          val live = Live // TODO Live? ----------------------------------------
          // TODO This logic is effective anywhere step titles are displayed.
          // TODO Also affects parsing
          if (p.step.title.isEmpty && p.defaultTitle.nonEmpty)
            p.widgets.format(live, p.defaultTitle)
          else
            p.widgets.format(live, p.step.title)
        }
        p.asyncState renderOr (p.editState renderOr view)
      }

      <.div(*.body, body)
    }
  }

  val Component = ReactComponentB[Props]("UseCaseStep")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}
