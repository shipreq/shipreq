package shipreq.webapp.base.lib

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.ext.KeyCode
import scalacss.ScalaCssReact._
import scala.scalajs.js
import shipreq.webapp.base.text.{LineCardinality, MultiLine, SingleLine}
import shipreq.webapp.base.lib.KeyHandler._
import shipreq.webapp.base.ui.BaseStyles.{editorInstructions => *}
import shipreq.webapp.base.ui.semantic.Icon

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
    commitCriterion.handle(commit >>= (Callback sequenceOption _))

  private val container: VdomTag = <.div(*.container)
  private val link     : VdomTag = <.a(*.link)
  private val clause   : VdomTag = <.span(*.clause)
  private val comma    : TagMod  = ","
  private val fullStop : TagMod  = "."

  private val helpIcon = Icon.HelpCircle.tag(*.helpIcon)

  def instructionsForCommitAbort(lc    : LineCardinality,
                                 commit: Option[Callback],
                                 abort : Option[Callback],
                                 help  : Option[Callback]): VdomTag = {

    val main: js.UndefOr[TagMod.Composite] = {
      var clauses = Vector.empty[TagMod]

      def add(m: TagMod*): Unit =
        clauses :+= TagMod.Composite(m.toVector)

      lc match {
        case SingleLine => ()
        case MultiLine  => add("enter for new line")
      }

      for (c <- commit) {
        add("ctrl-enter to ", link(^.onClick --> c, "save"))
      }

      for (a <- abort) {
        add("esc to ", link(^.onClick --> a, "cancel"))
      }

      if (clauses.isEmpty)
        js.undefined
      else {
        val last = clauses.length - 1
        var i = 0
        while (i <= last) {
          val a = clauses(i)
          val b = if (i == last) fullStop else comma
          clauses = clauses.updated(i, clause(a, b))
          i += 1
        }
        TagMod.Composite(clauses)
      }
    }

    help match {
      case Some(h) =>
        val eh = (e: ReactEvent) => e.stopPropagationCB >> e.preventDefaultCB >> h
        container(main.whenDefined, helpIcon(^.onClick ==> eh))
      case None =>
        // main.fold(EmptyVdom)(container(_))
        container(main.whenDefined)
    }
  }
}
