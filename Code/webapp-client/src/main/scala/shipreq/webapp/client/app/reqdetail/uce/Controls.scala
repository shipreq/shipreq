package shipreq.webapp.client.app.reqdetail.uce

import shipreq.webapp.client.app.Style.reqdetail.{useCaseStep => *}
import scalacss.ScalaCssReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import shipreq.base.util._
import shipreq.webapp.client.lib.DataReusability._

/**
  * Draws the buttons to the right of each step:
  *
  *     [-] [«] [»] [+]
  */
object Controls {
  sealed abstract class Action
  case object Delete     extends Action
  case object ShiftLeft  extends Action
  case object ShiftRight extends Action
  case object Add        extends Action

  type OnAction = Action => Callback

  final case class Props(delete    : Permission,
                         shiftLeft : Permission,
                         leftIsDown: Boolean,
                         shiftRight: Permission,
                         add       : Permission,
                         onAction  : OnAction) {
    @inline def render = Component(this)
  }

  def addTailStep(cb: Callback): Props =
    Props(Deny, Deny, false, Deny, add = Allow, onAction = {
      case Add => cb
      case _   => Callback.empty
    })

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.caseClassExcept('onAction)

  private val tagBase = <.button(*.ctrl)
  private val tagBlank = tagBase(^.visibility.hidden)
  private val tagAdd   = tagBase("+")
  private val tagDel   = tagBase("-")
  private val tagLeft  = tagBase("«")
  private val tagRight = tagBase("»")
  private val tagDown  = tagBase("↓")

  final class Backend($: BackendScope[Props, Unit]) {
    def onAction(a: Action): Callback =
      $.props >>= (_ onAction a)

    def btn(a: Action, allow: Permission, ctrl: ReactTag) =
      allow match {
        case Allow => ctrl(^.onClick --> onAction(a))
        case Deny  => tagBlank
      }

    def render(p: Props): ReactElement =
      <.div(*.ctrls,
        btn(Delete    , p.delete    , tagDel),
        btn(ShiftLeft , p.shiftLeft , if (p.leftIsDown) tagDown else tagLeft),
        btn(ShiftRight, p.shiftRight, tagRight),
        btn(Add       , p.add       , tagAdd))
  }

  val Component = ReactComponentB[Props]("Name")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build

  /*
  val Component = ReactComponentB[Props]("Name")
    .renderP { ($, p) =>
      def onAction(a: Action): Callback =
        CallbackTo($.props) >>= (_ onAction a)

      def btn(a: Action, allow: Permission, ctrl: ReactTag) =
        allow match {
          case Allow => ctrl(^.onClick --> onAction(a))
          case Deny  => tagBlank
        }

      <.div(*.ctrls,
        btn(Delete    , p.delete    , tagDel),
        btn(ShiftLeft , p.shiftLeft , if (p.leftIsDown) tagDown else tagLeft),
        btn(ShiftRight, p.shiftRight, tagRight),
        btn(Add       , p.add       , tagAdd))
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
  */
}

