package shipreq.webapp.client.app.ui.reqtable.edit

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.{ExternalVar, Px}
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom.raw.HTMLSelectElement
import shipreq.base.util.NonEmptySet
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.client.app.ui.SelectOne
import shipreq.webapp.client.app.ui.SelectOne.Choice
import shipreq.webapp.client.lib.TCB
import shipreq.webapp.client.util.Enabled

object ReqTypeSelector {

  type A = CustomReqType

  val selectComp = SelectOne.Component[A]

  private val selectRef = Ref[HTMLSelectElement]("i")

  case class Props(edit   : ExternalVar[A],
                   abort  : Option[TCB.Abort],
                   commit : Option[TCB.Commit],
                   choices: NonEmptySet[A])

  val Component = ReactComponentB[Props]("ReqTypeSelector")
    .render_P(render)
    //.componentDidMount(selectRef(_).tryFocus)
    .build

  def render(p: Props): ReactElement = {
    def choice(a: A) = Choice[A](a, a.fullName, Enabled)

    val choices = p.choices.mapV(choice).sortBy(_.label)

    val selector = selectComp.set(ref = selectRef)(SelectOne.Props(p.edit.value, choices, Some(p.edit.set)))

    <.div(
      selector,
      p.abort .map(f => <.button(UiText.buttonAbortChange, ^.onClick --> f)),
      p.commit.map(f => <.button(UiText.buttonCommitChange, ^.onClick --> f)))
  }

  // ===================================================================================================================

  def pxCustomReqTypes(p: Px[Project]): Px[Set[A]] =
    p.map(_.config.customReqTypes.values.toSet)

  def pxChoices(initial: A, pxCustomReqTypes: Px[Set[A]]): Px[NonEmptySet[A]] =
    pxCustomReqTypes
      .map(_.filter(_.live :: Live))
      .map(NonEmptySet(initial, _))
}
