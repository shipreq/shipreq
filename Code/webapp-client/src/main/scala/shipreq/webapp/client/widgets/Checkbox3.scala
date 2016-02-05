package shipreq.webapp.client.widgets

import japgolly.scalajs.react._, vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import org.scalajs.dom.html.Input
import shipreq.webapp.client.data.On
import shipreq.webapp.client.lib.ClientUtil

/**
 * Tri-state checkbox.
 */
object Checkbox3 {
  type State3 = Option[On]
  type State2 = On

  case class Props(state: State3, set: State2 => Callback)

  implicit val propsReusability: Reusability[Props] = Reusability.by(_.state)

  private def updateDom($: CompScope.Mounted[Input], nextProps: Props): Callback =
    updateDom($.getDOMNode(), nextProps.state)

  private def updateDom(d: Input, s: State3) = Callback {
    d.checked       = s.exists(_ :: On)
    d.indeterminate = s.isEmpty
  }

  def nextState(s: State3): State2 =
    s.fold[State2](On)(!_)

  private def render($: CompScope.DuringCallbackU[Props, Unit, Unit], p: Props) = {
    val s = p.state
    val setNext = $.propsCB >>= (_ set nextState(s))
    <.input.checkbox(
      ClientUtil checkboxLikeEventHandlers setNext)
  }

  val Component = ReactComponentB[Props]("Checkbox3")
    .renderP(render)
    .domType[Input]
    .componentDidMount($ => updateDom($, $.props))
    .componentWillReceiveProps(i => updateDom(i.$, i.nextProps))
    .configure(Reusability.shouldComponentUpdate)
    .build
}