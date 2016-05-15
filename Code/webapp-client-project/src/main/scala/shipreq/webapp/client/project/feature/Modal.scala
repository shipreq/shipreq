package shipreq.webapp.client.project.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._

trait Modal {
  def render: ReactElement
}

object Modal {
  type State = Option[Modal]
  type SetFn = State ~=> Callback

  def none: State =
    None

  def apply(re: ReactElement): Modal =
    new Modal {
      override def render = re
    }

  @inline implicit def autoLiftOption(m: Modal): State =
    Some(m)

  @inline implicit class OptionModalOps(private val o: State) extends AnyVal {
    @inline def renderOrElse(default: => ReactElement): ReactElement =
      o.fold(default)(_.render)
  }

  implicit val reuse: Reusability[State] =
    Reusability.option(Reusability.never[Modal])
}