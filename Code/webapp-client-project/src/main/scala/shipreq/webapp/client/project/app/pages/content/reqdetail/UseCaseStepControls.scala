package shipreq.webapp.client.project.app.pages.content.reqdetail

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import scalacss.StyleA
import shipreq.base.util.{Allow, LeftRight, Permission}
import shipreq.webapp.base.feature.{AsyncFeature, TableNavigationFeature}
import shipreq.webapp.base.ui.semantic.{Button, Icon}
import shipreq.webapp.client.project.app.Style.reqdetail.{useCaseStep => *}

/**
  * Draws the buttons to the right of each step:
  *
  *     [-] [«] [»] [+]
  */
object UseCaseStepControls {

  type AsyncState = AsyncFeature.Read.D0[Any]

  type HoverText = String

  case class ButtonDesc(callback: Callback, hoverText: HoverText)

  private def mkButton(as: AsyncState, style: StyleA, icon: Icon, ob: Option[ButtonDesc], editability: Permission) = {
    // When async action in progress, disable all buttons
    val attr = if (as.isEmpty && editability.is(Allow)) ob else None

    val base =
      Button(
        tipe = Button.Type.IconOnly(icon),
        state = Button.State.enabledWhen(attr.isDefined),
      ).tag(
        style,
        TableNavigationFeature.ignore,
      )

    attr match {
      case Some(b) => base(^.title := b.hoverText, ^.onClick --> b.callback)
      case None    => base
    }
  }

  // ===================================================================================================================
  import LeftRight.{Left, Right}

  val IconDelete  = Icon.Trash
  val IconRestore = Icon.Undo
  val IconAdd     = Icon.Plus
  val IconShift   = LeftRight.Values {
    case Left  =>  Icon.AngleDoubleLeft
    case Right =>  Icon.AngleDoubleRight
  }

  sealed abstract class CurStepButtons
  object CurStepButtons {
    case class WhenLive(delete: Option[ButtonDesc],
                        shift : LeftRight.Values[Option[ButtonDesc]]) extends CurStepButtons

    case class WhenDead(restore: ButtonDesc) extends CurStepButtons
  }

  def renderStep(curStepButtons: CurStepButtons,
                 curStepAsync  : AsyncState,
                 insertButton  : Option[ButtonDesc],
                 insertAsync   : AsyncState,
                 editability   : Permission): VdomElement = {

    val buttons = curStepButtons match {

      case b: CurStepButtons.WhenLive =>
        mkButton(curStepAsync, *.ctrlButtonDelete,       IconDelete,       b.delete,       editability) ::
        mkButton(curStepAsync, *.ctrlButtonShift(Left),  IconShift(Left),  b.shift(Left),  editability) ::
        mkButton(curStepAsync, *.ctrlButtonShift(Right), IconShift(Right), b.shift(Right), editability) ::
        mkButton(insertAsync,  *.ctrlButtonInsert,       IconAdd,          insertButton,   editability) ::
        Nil

      case CurStepButtons.WhenDead(r) =>
        mkButton(curStepAsync, *.ctrlButtonRestore,      IconRestore,      Some(r), editability) ::
        mkButton(curStepAsync, *.ctrlButtonShift(Left),  IconShift(Left),  None,    editability) ::
        mkButton(curStepAsync, *.ctrlButtonShift(Right), IconShift(Right), None,    editability) ::
        mkButton(insertAsync,  *.ctrlButtonInsert,       IconAdd,          None,    editability) ::
        Nil
    }

    <.div(*.ctrls, Button.group(buttons: _*))
  }

  val renderStepWhenUseCaseDead: VdomElement =
    <.div(*.ctrls)

  // ===================================================================================================================

  def renderTailStep(button: ButtonDesc, async: AsyncState, editability: Permission): VdomElement = {
    val b = mkButton(async, *.ctrlButtonInsert, IconAdd, Some(button), editability)
    <.div(*.ctrls, b)
  }
}
