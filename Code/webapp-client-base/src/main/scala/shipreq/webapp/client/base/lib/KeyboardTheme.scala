package shipreq.webapp.client.base.lib

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom.ext.KeyCode
import scalacss.ScalaCssReact._
import shipreq.webapp.base.text.LineCardinality
import shipreq.webapp.client.base.lib.KeyHandler._
import shipreq.webapp.client.base.ui.BaseStyles

/**
  * Keyboard functionality consistent throughout the entire app.
  */
object KeyboardTheme {
  val Escape    = Criterion(EventType.KeyDown , KeyCode.Escape)
  val Enter     = Criterion(EventType.KeyPress, KeyCode.Enter)
  val CtrlEnter = Criterion(EventType.KeyDown , KeyCode.Enter, ModKey.Ctrl)

  @inline def abortCriterion = Escape

  // TODO Change `=> Callback` to just `Callback`, here & in KeyHandlers

  def abort(abort: => Callback): KeyHandler =
    abortCriterion.handle(abort)

  /** It used to be the case that in single-line editors, Enter would be used to commit with Ctrl-Enter also allowed
    * for consistency with multi-line editors.
    * It's much simpler for a user to just remember that Ctrl-Enter is commit every and that Enter works like it does
    * in a text editor, just not everywhere. The penalty for trying to insert a newline into a single-line editor is
    * now nil, in that nothing happens; where as previously it would trigger a save which can be very annoying.
    */
  @inline def commitCriterion = CtrlEnter

  def commitO(commit: => Option[Callback], lc: LineCardinality): KeyHandler = {
    // LineCardinality is no longer used here but will be kept as an arg for a while longer until confidence in the new
    // style commit criteria is established.
    commitCO(CallbackTo(commit), lc)
  }

  def commitCO(commit: CallbackTo[Option[Callback]], lc: LineCardinality): KeyHandler =
    commitCriterion.handle(commit >>= (Callback.sequenceO(_)))

  def instructionsForCommitAbort(commit: Option[Callback],  abort: Callback): ReactTag = {
    var save: ReactNode = "save"
    for (c <- commit)
      save = <.a(^.onClick --> c, save)

    val cancel = <.a(^.onClick --> abort, "cancel")

    <.div(BaseStyles.editorInstructions,
      "ctrl-enter to ", save,
      ", esc to ", cancel, ".")
  }
}
