package shipreq.webapp.member.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import java.time.Duration
import scalacss.ScalaCssReact._
import shipreq.webapp.base.util.{Off, On, ReactKeyGen}
import shipreq.webapp.member.project.util.DataReusability._
import shipreq.webapp.member.ui.BaseStyles.{toast => *}

/** This is an interface that can be passed around so that downstream components can add messages.
 */
trait Toast {
  import Toast._

  final def add(msg: VdomNode, duration: Duration = defaultDuration): Callback =
    addWithCtrls(_ => msg, duration)

  final def addWithCtrls(msgFn: Ctrls => VdomNode): Callback =
    addWithCtrls(msgFn, defaultDuration)

  def addWithCtrls(msgFn: Ctrls => VdomNode, duration: Duration): Callback
}

object Toast {

  def apply($: StateAccess.Write[CallbackTo, AsyncCallback, Toast.State]): Toast =
    new Toast {
      override def addWithCtrls(msgFn: Ctrls => VdomNode, duration: Duration): Callback = {
        val id    = keyGen.next()
        val ctrls = Ctrls.forId($, id)
        val msg   = msgFn(ctrls)
        val bread = Bread(id, Reusable.byRef(msg), BreadState.ComingOn)

        val add  = (_: State).add(bread)
        val show = (_: State).setBreadState(bread, BreadState.On)
        val hide = (_: State).setBreadState(bread, BreadState.GoingOff)

        val waitAndShow       = $.modState(show).delayMs(10).toCallback
        val deleteWhenExpired = $.modState(hide).delay(duration).toCallback

        $.modState(add, waitAndShow >> deleteWhenExpired)
      }
    }

  val defaultDuration = Duration.ofSeconds(4)

  private[Toast] val keyGen = new ReactKeyGen

  type Props = StateSnapshot[State]

  final case class Ctrls(close: Callback)

  object Ctrls {
    def forId($: StateAccess.Write[CallbackTo, AsyncCallback, Toast.State], id: Key): Ctrls =
      Ctrls(
        close = $.modState(_.deleteById(id)),
      )
  }

  sealed trait BreadState
  object BreadState {
    case object ComingOn extends BreadState
    case object On       extends BreadState
    case object GoingOff extends BreadState

    implicit def univEq: UnivEq[BreadState] = UnivEq.derive
  }

  final case class Bread(id: Key, msg: Reusable[VdomNode], state: BreadState) {
    def isSameAs(b: Bread): Boolean =
      id == b.id
  }

  final case class State(bread: List[Bread]) {

    def add(b: Bread): State =
      copy(bread ::: b :: Nil)

    def replace(updated: Bread): State =
      copy(bread.map(b => if (b.isSameAs(updated)) updated else b))

    def setBreadState(b: Bread, s: BreadState): State =
      replace(b.copy(state = s))

    def delete(b: Bread): State =
      deleteById(b.id)

    def deleteById(id: Key): State =
      copy(bread.filter(_.id == id))

    def deleteAll(bs: IterableOnce[Bread]): State = {
      val ids = bs.iterator.map(_.id).toSet
      copy(bread.filter(b => !ids.contains(b.id)))
    }
  }

  object State {
    def init: State =
      apply(Nil)
  }

  private val toast = On.memo(on => <.div(*.toast(on)))

  private def itemBase(b: Bread) =
    <.div(^.key := b.id, b.msg)

  private def render(ss: Props): VdomNode = {
    val s = ss.value

    if (s.bread.isEmpty)
      toast(Off)
    else if (s.bread.forall(_.state ==* BreadState.GoingOff))
      toast(Off)(
        ^.onTransitionEnd --> ss.modState(_.deleteAll(s.bread)),
        s.bread.toVdomArray(b =>
          itemBase(b)(*.item(On))))
    else
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
      )
  }

  implicit val reusabilityToast     : Reusability[Toast     ] = Reusability.byRef
  implicit val reusabilityBreadState: Reusability[BreadState] = Reusability.derive
  implicit val reusabilityBread     : Reusability[Bread     ] = Reusability.derive
  implicit val reusabilityState     : Reusability[State     ] = Reusability.derive

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}