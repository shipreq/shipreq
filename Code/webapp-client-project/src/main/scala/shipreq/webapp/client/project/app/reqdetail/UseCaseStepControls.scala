package shipreq.webapp.client.project.app.reqdetail

import scalacss.ScalaCssReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import shipreq.webapp.base.UiText
import shipreq.webapp.client.base.feature.AsyncActionFeature
import shipreq.webapp.client.project.app.Style.reqdetail.{useCaseStep => *}
import shipreq.webapp.client.project.lib.DataReusability._

/**
  * Draws the buttons to the right of each step:
  *
  *     [-] [«] [»] [+]
  */
object UseCaseStepControls {

  private val tagBase    = <.button(*.ctrl)
  private val tagBlank   = tagBase(^.visibility.hidden)
  private val tagBlank3  = (tagBlank, tagBlank, tagBlank)
  private val tagAdd     = tagBase("+")
  private val tagDelete  = tagBase("-", ^.title := UiText.Life.delete)
  private val tagRestore = tagBase("^", ^.title := UiText.Life.restore)
  private val tagLeft    = tagBase("«")
  private val tagRight   = tagBase("»")
  private val tagDown    = tagBase("↓")
  private val tagUp      = tagBase("↑")
  private val disabled   = ^.disabled := true

  type AsyncState = AsyncActionFeature.D0.State[Any]

  sealed abstract class HasBase { def base: ReactTag }

  object Props {
    sealed abstract class ShiftLeft extends HasBase
    case object ShiftLeft extends ShiftLeft { override def base = tagLeft }
    case object ShiftDown extends ShiftLeft { override def base = tagDown }

    sealed abstract class ShiftRight extends HasBase
    case object ShiftRight extends ShiftRight { override def base = tagRight }
    case object ShiftUp    extends ShiftRight { override def base = tagUp }

    sealed abstract class Self

    case class WhenLive(delete    : Option[Callback],
                        shiftLeft : Option[(ShiftLeft, Callback)],
                        shiftRight: Option[(ShiftRight, Callback)]) extends Self

    case class WhenDead(restore: Callback) extends Self

    def tailStep(cb: Callback, a: AsyncState): Props =
      Props(None, Some((cb, a)))

    val none: Props =
      Props(None, None)
  }

  case class Props(self: Option[(Props.Self, AsyncState)],
                   add : Option[(Callback, AsyncState)]) {
    @inline def render = Component(this)
  }

  // Probably not worth it for the small amount of DOM this generates.
//  implicit val reusabilityProps: Reusability[Props] =
//    Reusability.???

  final class Backend($: BackendScope[Props, Unit]) {
    import Props._

    def mkBtn(b: ReactTag, o: Option[(Callback, AsyncState)]): ReactTag =
      o match {
        case Some((cb, a)) => mkBtn(a, b, cb)
        case None          => tagBlank
      }

    def mkBtn(a: AsyncState, b: ReactTag, o: Option[Callback]): ReactTag =
      o match {
        case Some(cb) => mkBtn(a, b, cb)
        case None     => tagBlank
      }

    def mkBtn(a: AsyncState, o: Option[(HasBase, Callback)]): ReactTag =
      o match {
        case Some((h, cb)) => mkBtn(a, h.base, cb)
        case None          => tagBlank
      }

    def mkBtn(a: AsyncState, b: ReactTag, cb: Callback): ReactTag = {
      import AsyncActionFeature._
      a match {
        case None
           | Some(_: Failed[Any]) => b(^.onClick --> cb)
        case Some(Locked)         => b(disabled)
      }
    }

    def render(p: Props): ReactElement = {
      val (b1, b2, b3) = p.self match {

        case None =>
          tagBlank3

        case Some((WhenDead(cb), a)) =>
          (mkBtn(a, tagRestore, cb), tagBlank, tagBlank)

        case Some((WhenLive(d, sl, sr), a)) =>
          (mkBtn(a, tagDelete, d), mkBtn(a, sl), mkBtn(a, sr))
      }

      val b4 = mkBtn(tagAdd, p.add)

      <.div(*.ctrls, b1, b2, b3, b4)
    }
  }

  val Component = ReactComponentB[Props]("UseCaseStepControls")
    .renderBackend[Backend]
//    .configure(Reusability.shouldComponentUpdate)
    .build
}

