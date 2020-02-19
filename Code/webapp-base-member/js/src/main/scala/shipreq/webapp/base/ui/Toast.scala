package shipreq.webapp.base.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq._
import java.time.Duration
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data.{Off, On}
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.lib.KeyGen
import shipreq.webapp.base.ui.BaseStyles.{toast => *}

/** This is an interface that can be passed around so that downstream components can add messages.
 */
final class Toast($: StateAccess.Write[CallbackTo, Toast.State]) {
  import Toast._

  def add(msg: VdomNode, duration: Duration = defaultDuration): Callback = {
    val bread = Bread(keyGen.next(), Reusable.byRef(msg), BreadState.ComingOn)

    val add  = (_: State).add(bread)
    val show = (_: State).setBreadState(bread, BreadState.On)
    val hide = (_: State).setBreadState(bread, BreadState.GoingOff)

    val waitAndShow       = $.modState(show).delayMs(10).toCallback
    val deleteWhenExpired = $.modState(hide).delay(duration).toCallback

    $.modState(add, waitAndShow >> deleteWhenExpired)
  }
}

object Toast {

  def apply($: StateAccess.Write[CallbackTo, Toast.State]): Toast =
    new Toast($)

  val defaultDuration = Duration.ofSeconds(4)

  private[Toast] val keyGen = new KeyGen

  type Props = StateSnapshot[State]

  sealed trait BreadState
  object BreadState {
    case object ComingOn extends BreadState
    case object On       extends BreadState
    case object GoingOff extends BreadState

    implicit def univEq: UnivEq[BreadState] = UnivEq.derive
  }

  final case class Bread(id: String, msg: Reusable[VdomNode], state: BreadState) {
    def isSameAs(b: Bread): Boolean =
      msg eq b.msg
  }

  final case class State(bread: List[Bread]) {

    def add(b: Bread): State =
      copy(bread ::: b :: Nil)

    def replace(updated: Bread): State =
      copy(bread.map(b => if (b.isSameAs(updated)) updated else b))

    def setBreadState(b: Bread, s: BreadState): State =
      replace(b.copy(state = s))

    def delete(b: Bread): State =
      copy(bread.filterNot(b.isSameAs))

    def deleteAll(bs: TraversableOnce[Bread]): State =
      // quadratic but ok cos tiny
      bs.foldLeft(this)(_.delete(_))
  }

  object State {
    def init: State =
      apply(Nil)
  }

  private val row   = <.div(*.row)
  private val toast = On.memo(on => <.div(*.toast(on)))
  private val empty = row(toast(Off))

  private def itemBase(b: Bread) =
    <.div(^.key := b.id, b.msg)

  private def render(ss: Props): VdomNode = {
    val s = ss.value

    if (s.bread.isEmpty)
      empty
    else if (s.bread.forall(_.state ==* BreadState.GoingOff))
      row(
        toast(Off)(
          ^.onTransitionEnd --> ss.modState(_.deleteAll(s.bread)),
          s.bread.toVdomArray(b =>
            itemBase(b)(*.item(On)))))
    else
      row(
        toast(On)(
          s.bread.toVdomArray { b =>
            val on = b.state match {
              case BreadState.ComingOn => Off
              case BreadState.On       => On
              case BreadState.GoingOff => Off
            }
            val onTransitionEnd: TagMod =
              b.state match {
                case BreadState.ComingOn => TagMod.empty
                case BreadState.On       => TagMod.empty
                case BreadState.GoingOff => ^.onTransitionEnd --> ss.modState(_.delete(b))
              }
            itemBase(b)(*.item(on), onTransitionEnd)
          }
        ))
  }

  implicit val reusabilityToast     : Reusability[Toast     ] = Reusability.byRef
  implicit val reusabilityBreadState: Reusability[BreadState] = Reusability.derive
  implicit val reusabilityBread     : Reusability[Bread     ] = Reusability.derive
  implicit val reusabilityState     : Reusability[State     ] = Reusability.derive

  val Component = ScalaComponent.builder[Props]("Toast")
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}