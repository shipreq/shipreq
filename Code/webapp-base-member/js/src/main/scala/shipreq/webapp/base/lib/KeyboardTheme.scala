package shipreq.webapp.base.lib

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.text.{LineCardinality, MultiLine, SingleLine}
import shipreq.webapp.base.lib.KeyHandler._
import shipreq.webapp.base.ui.BaseStyles.{editorInstructions => *}
import shipreq.webapp.base.ui.semantic.Icon

/**
  * Keyboard functionality consistent throughout the entire app.
  */
object KeyboardTheme {

  @inline def abortCriterion = Criterion.Escape
  @inline def abortKeyDesc   = "esc"

  def abort(abort: Callback): KeyHandler =
    abortCriterion.handle(abort)

  /** It used to be the case that in single-line editors, Enter would be used to commit with Ctrl-Enter also allowed
    * for consistency with multi-line editors.
    * It's much simpler for a user to just remember that Ctrl-Enter is commit every and that Enter works like it does
    * in a text editor, just not everywhere. The penalty for trying to insert a newline into a single-line editor is
    * now nil, in that nothing happens; where as previously it would trigger a save which can be very annoying.
    */
  @inline def commitCriterion = Criterion.CtrlEnter
  @inline def commitKeyDesc   = "ctrl-enter"

  def commitO(commit: => Option[Callback], lc: LineCardinality): KeyHandler = {
    // LineCardinality is no longer used here but will be kept as an arg for a while longer until confidence in the new
    // style commit criteria is established.
    commitCO(CallbackTo(commit), lc)
  }

  def commitCO(commit: CallbackTo[Option[Callback]], lc: LineCardinality): KeyHandler =
    commitCriterion.handle(commit >>= (Callback sequenceOption _))

  /** Commit and progress, as in "save and let's move on".
    *
    * Progress is different depending on the context.
    * For UC steps, it means close the current step, create a new child step and focus it.
    * For fields in the ReqTable new requirement form, it means close the form (implicitly yielding focus to the table).
    */
  def commitAndProgressCriterion = KeyHandler.Criterion.AltEnter
  def commitAndProgressKeyDesc   = "alt-enter"

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object Instructions {
    sealed trait Atom
    final case class Vdom(value: TagMod) extends Atom
    final case class Link(label: TagMod, onClick: Callback) extends Atom

    type Clause = NonEmptyVector[Atom]
    object Clause {

      def keyToAction(key: String)(action: String, actionCB: Callback): Clause =
        NonEmptyVector(Vdom(key + " to "), Vector.empty :+ Link(action, actionCB))

      def abort(c: Callback): Clause =
        keyToAction(abortKeyDesc)("cancel", c)

      def commit(c: Callback): Clause =
        keyToAction(commitKeyDesc)("save", c)

      val multiLine: Clause =
        NonEmptyVector one Vdom("enter for new line")
    }

    private val container : VdomTag = <.div(*.container)
    private val link      : VdomTag = <.a(*.link)
    private val clauseCont: VdomTag = <.span(*.clause)
    private val comma     : TagMod  = ","
    private val fullStop  : TagMod  = "."
    private val helpIcon  : VdomTag = Icon.HelpCircle.tag(*.helpIcon)

    private val renderAtom: Atom => TagMod = {
      case Vdom(v)    => v
      case Link(v, c) => link(^.onClick --> c, v)
    }

    def apply(clauses: TraversableOnce[Clause], help: Option[Callback]): VdomTag = {
      val text =
        TagMod.when(clauses.nonEmpty) {
          val rendered = Vector.newBuilder[TagMod]
          val it = clauses.toIterator
          while (it.hasNext) {
            val clause = it.next()
            val suffix = if (it.hasNext) comma else fullStop
            rendered  += clauseCont(TagMod.Composite(clause.whole.map(renderAtom) :+ suffix))
          }
          TagMod.Composite(rendered.result())
        }

      val helpButton =
        help.whenDefined { h =>
          val eh = (e: ReactEvent) => e.stopPropagationCB >> e.preventDefaultCB >> h
          helpIcon(^.onClick ==> eh)
        }

      container(text, helpButton)
    }

    def forTextEditor(lc    : LineCardinality,
                      commit: Option[Callback],
                      abort : Option[Callback],
                      help  : Option[Callback]): VdomTag =
      apply(clausesForTextEditor(lc, commit = commit, abort = abort), help = help)

    def clausesForTextEditor(lc    : LineCardinality,
                             commit: Option[Callback],
                             abort : Option[Callback]): List[Clause] = {
      var clauses = List.empty[Clause]

      abort.foreach(clauses ::= Clause.abort(_))

      commit.foreach(clauses ::= Clause.commit(_))

      lc match {
        case SingleLine => ()
        case MultiLine  => clauses ::= Clause.multiLine
      }

      clauses
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /**
    * @param instructions These will be prepended to the typical editor instructions.
    */
  final case class Shortcuts(keyHandlers: KeyHandlers, instructions: List[Instructions.Clause]) {
    def ++(that: Shortcuts): Shortcuts =
      Shortcuts(
        this.keyHandlers ++ that.keyHandlers,
        this.instructions ::: that.instructions)
  }

  object Shortcuts {
    val empty: Shortcuts =
      apply(KeyHandlers.empty, Nil)
  }

  def Shortcut[A](keyHandlers: KeyHandler, instructions: Option[Instructions.Clause]): Shortcuts =
    Shortcuts(keyHandlers.toKeyHandlers, instructions.toList)
}
