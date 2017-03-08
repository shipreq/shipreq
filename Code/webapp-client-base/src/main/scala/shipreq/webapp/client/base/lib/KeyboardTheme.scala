package shipreq.webapp.client.base.lib

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.ext.KeyCode
import scalacss.ScalaCssReact._
import shipreq.webapp.base.text.{LineCardinality, MultiLine, SingleLine}
import shipreq.webapp.client.base.lib.KeyHandler._
import shipreq.webapp.client.base.ui.BaseStyles.{editorInstructions => *}
import shipreq.webapp.client.base.ui.semantic.Icon

/**
  * Keyboard functionality consistent throughout the entire app.
  */
object KeyboardTheme {
  val Escape    = Criterion(EventType.KeyDown , KeyCode.Escape)
  val Enter     = Criterion(EventType.KeyPress, KeyCode.Enter)
  val CtrlEnter = Criterion(EventType.KeyDown , KeyCode.Enter, ModKey.Ctrl)

  @inline def abortCriterion = Escape

  def abort(abort: Callback): KeyHandler =
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
    commitCriterion.handle(commit >>= (Callback sequenceO _))

  private val link   = <.a(*.link)
  private val clause = <.span(*.clause)

  private val helpIcon = Icon.HelpCircle.tag(*.helpIcon)

  def instructionsForCommitAbort(lc    : LineCardinality,
                                 commit: Option[Callback],
                                 abort : Callback,
                                 help  : Option[Callback]): VdomTag = {
    var tag = <.div(*.container)

    def add(m: TagMod*): Unit =
      tag = tag(clause(m: _*))

    // New line
    lc match {
      case SingleLine => ()
      case MultiLine  => add("enter for new line,")
    }

    // Commit
    var save: VdomNode = "save"
    for (c <- commit)
      save = link(^.onClick --> c, save)
    add("ctrl-enter to ", save, ",")

    // Abort
    val cancel = link(^.onClick --> abort, "cancel")
    add("esc to ", cancel, ".")

    // Help
    for (h <- help) {
      val eh = (e: ReactEvent) => e.stopPropagationCB >> e.preventDefaultCB >> h
      add(helpIcon(^.onClick ==> eh))
    }

    tag
  }
}
