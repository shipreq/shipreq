package shipreq.webapp.client.app.ui.reqtable.edit

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom.raw.HTMLSelectElement
import scala.scalajs.js.{UndefOr, undefined}
import scalaz.Equal
import scalaz.effect.IO
import scalaz.syntax.equal._
import shipreq.base.util.effect.IoUtils.IoExt
import shipreq.base.util.{NonEmptySet, Px}
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.client.app.ui.SelectOne
import shipreq.webapp.client.app.ui.reqtable.Cell
import SelectOne.Choice

object ReqTypeSelector {

  type A = CustomReqType

  private implicit val equality: Equal[A] = Equal.equalBy(_.id)

  def apply(initial : A,
            fields  : Px[Set[A]],
            setState: Option[Cell.State] => IO[Unit]): Cell.State = {

    val fieldsN = fields.map(NonEmptySet(initial, _))

    val abort: IO[Unit] =
      setState(None)

    def commit(s: A): UndefOr[IO[Unit]] =
      if (s ≟ initial)
        undefined
      else
      // TODO If change occurred, send to server & lock cell. (If unchanged, clear state.)
        setState(None) >>> IO { println("Sent to ze server: " + s) }

    Cell.selfManage(setState, initial)(
      (s, u) => component(Props(s, u, abort, commit(s), fieldsN.value())))
  }

  val selectComp = SelectOne.Component[A]

  private val selectRef = Ref[HTMLSelectElement]("i")

  case class Props(state      : A,
                   stateUpdate: A => IO[Unit],
                   abort      : IO[Unit],
                   commit     : UndefOr[IO[Unit]],
                   fields     : NonEmptySet[A])

  val component = ReactComponentB[Props]("ReqTypeSelector")
    .render(render(_))
    .componentDidMount(selectRef(_).tryFocus())
    .build

  def render(p: Props): ReactElement = {
    def choice(a: A) = Choice[A](a, a.fullName, false)

    val choices = p.fields.mapV(choice).sortBy(_.label)

    val selector = selectComp.set(ref = selectRef)(SelectOne.Props(p.state, choices, Some(p.stateUpdate)))

    val button =
      p.commit.fold[ReactTag](
        <.button(UiText.buttonAbortChange,  ^.onClick ~~> p.abort))(
        io => <.button(UiText.buttonCommitChange, ^.onClick ~~> io))

    <.div(selector, button)
  }
}
