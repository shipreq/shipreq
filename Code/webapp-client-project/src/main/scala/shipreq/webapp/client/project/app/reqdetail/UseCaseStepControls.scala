package shipreq.webapp.client.project.app.reqdetail

import scalacss.ScalaCssReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.StyleA
import shipreq.webapp.client.base.feature.AsyncFeature
import shipreq.webapp.client.base.ui.semantic.{Button, Icon}
import shipreq.webapp.client.project.app.Style.reqdetail.{useCaseStep => *}

/**
  * Draws the buttons to the right of each step:
  *
  *     [-] [«] [»] [+]
  */
object UseCaseStepControls {

  type AsyncState = AsyncFeature.ReadOnly.D0[Any]

  type HoverText = String

  case class ButtonDesc(callback: Callback, hoverText: HoverText)

  private def mkButton(as: AsyncState, style: StyleA, icon: Icon, ob: Option[ButtonDesc]) = {
    // When async action in progress, disable all buttons
    val attr = if (as.isEmpty) ob else None

    val base = Button(tipe = Button.Type.IconOnly(icon), state = Button.State.enabledWhen(attr.isDefined)).tag(style)
    attr match {
      case Some(b) => base(^.title := b.hoverText, ^.onClick --> b.callback)
      case None    => base
    }
  }

  // ===================================================================================================================

  val IconDelete     = Icon.Trash
  val IconRestore    = Icon.Undo
  val IconShiftLeft  = Icon.AngleDoubleLeft
  val IconShiftRight = Icon.AngleDoubleRight
  val IconAdd        = Icon.Plus

  sealed abstract class CurStepButtons
  object CurStepButtons {
    case class WhenLive(delete    : Option[ButtonDesc],
                        shiftLeft : Option[ButtonDesc],
                        shiftRight: Option[ButtonDesc]) extends CurStepButtons

    case class WhenDead(restore: ButtonDesc) extends CurStepButtons
  }

  def renderStep(curStepButtons: CurStepButtons,
                 curStepAsync  : AsyncState,
                 insertButton  : Option[ButtonDesc],
                 insertAsync   : AsyncState): VdomElement = {

    val buttons = curStepButtons match {

      case b: CurStepButtons.WhenLive =>
        mkButton(curStepAsync, *.ctrlButtonDelete    , IconDelete,     b.delete) ::
        mkButton(curStepAsync, *.ctrlButtonShiftLeft , IconShiftLeft,  b.shiftLeft) ::
        mkButton(curStepAsync, *.ctrlButtonShiftRight, IconShiftRight, b.shiftRight) ::
        mkButton(insertAsync,  *.ctrlButtonInsert    , IconAdd,        insertButton) ::
        Nil

      case CurStepButtons.WhenDead(r) =>
        mkButton(curStepAsync, *.ctrlButtonRestore   , IconRestore,   Some(r)) ::
        mkButton(curStepAsync, *.ctrlButtonShiftLeft , IconShiftLeft, None) ::
        mkButton(curStepAsync, *.ctrlButtonShiftRight, IconShiftRight,None) ::
        mkButton(insertAsync,  *.ctrlButtonInsert    , IconAdd,       None) ::
        Nil
    }

    <.div(*.ctrls, Button.group(buttons: _*))
  }

  val renderStepWhenUseCaseDead: VdomElement =
    <.div(*.ctrls)

  // ===================================================================================================================

  def renderTailStep(button: ButtonDesc, async: AsyncState): VdomElement = {
    val b = mkButton(async, *.ctrlButtonInsert, IconAdd, Some(button))
    <.div(*.ctrls, b)
  }
}

