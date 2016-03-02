package shipreq.webapp.client.app.reqdetail.uce

import shipreq.webapp.client.app.Style.reqdetail.{useCaseStep => *}
import scalacss.ScalaCssReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.client.widgets.high.ProjectWidgets
import shipreq.webapp.client.feature.{AsyncActionFeature, ContentEditorFeature}
import shipreq.webapp.client.lib.DataReusability._

object StepTree {
  /*
  case class Props(pos       : ReqTypePos,
                   loc       : VectorTree.Location,
                   step      : UseCaseStep,
                   flow      : UseCases.StepFlow,
                   stepLabel : UseCaseStepId ~=> String,
                   field     : StaticField.UseCaseStepTree,
                   widgets   : ProjectWidgets,
                   editState : ContentEditorFeature.D0.State,
                   asyncState: AsyncActionFeature.D0.State[String],
                   startEdit : Callback,
                   update    : UpdateContentCmd.ForUseCaseStep => Callback) {
    @inline def id = step.id
    @inline def render = Component.withKey(step.id.value)(this)
  }

  final class Backend($: BackendScope[Props, Unit]) {
    def render(p: Props) = {

      def header =
        p.field.stepLabel(p.pos, p.loc, mnemonicPrefix = false)

      <.div(*.container,
        <.div(*.header(p.loc.length - 1), header),
    }
  }

  val Component = ReactComponentB[Props]("UseCaseStep")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
  */

}
