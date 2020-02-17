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

  def abort(abort: Callback): KeyHandler =
    abortCriterion.handle(abort)

  /** It used to be the case that in single-line editors, Enter would be used to commit with Ctrl-Enter also allowed
    * for consistency with multi-line editors.
    * It's much simpler for a user to just remember that Ctrl-Enter is commit every and that Enter works like it does
    * in a text editor, just not everywhere. The penalty for trying to insert a newline into a single-line editor is
    * now nil, in that nothing happens; where as previously it would trigger a save which can be very annoying.
    */
  @inline def commitCriterion = Criterion.CtrlEnter

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
    * For fields in the ReqTable new requirement form, it means save and move onto next new req (i.e. keep open).
    */
  def commitAndProgressCriterion = KeyHandler.Criterion.AltEnter

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
        keyToAction(abortCriterion.desc)("cancel", c)

      def commit(c: Callback, verb: String): Clause =
        keyToAction(commitCriterion.desc)(verb, c)

      val multiLine: Clause =
        NonEmptyVector one Vdom("enter for new line")
    }

    def defaultCommitVerb = "save"

    object Clauses {
      def forTextEditor(lc        : LineCardinality,
                        commit    : Option[Callback],
                        commitVerb: String,
                        abort     : Option[Callback]): List[Clause] = {
        var clauses = List.empty[Clause]

        abort.foreach(clauses ::= Clause.abort(_))

        commit.foreach(clauses ::= Clause.commit(_, commitVerb))

        lc match {
          case SingleLine => ()
          case MultiLine  => clauses ::= Clause.multiLine
        }

        clauses
      }
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
      val helpButton =
        help.whenDefined { h =>
          val eh = (e: ReactEvent) => e.stopPropagationCB >> e.preventDefaultCB >> h
          helpIcon(^.onClick ==> eh)
        }

      val content: TagMod =
        if (clauses.isEmpty)
          helpButton
        else {
          var rendered = Vector.empty[TagMod]
          val it = clauses.toIterator
          var last: VdomTag = null
          while (it.hasNext) {
            val clause = it.next()
            val suffix = if (it.hasNext) comma else fullStop
            last = clauseCont(TagMod.Composite(clause.whole.map(renderAtom) :+ suffix))
            rendered :+= last
          }

          // Here we add the help button to the last clause.
          // The reason is that we don't want word-wrapping to occur between the last clause and the help button
          // because a lone, tiny help button on its own line looks terrible.
          rendered = rendered.dropRight(1) :+ last(helpButton)

          TagMod.Composite(rendered)
        }

      container(content)
    }

    def forTextEditor(lc        : LineCardinality,
                      commit    : Option[Callback],
                      commitVerb: String,
                      abort     : Option[Callback],
                      help      : Option[Callback]): VdomTag =
      apply(Clauses.forTextEditor(lc, commit = commit, commitVerb = commitVerb, abort = abort), help = help)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object Shortcut {
    def apply(keyHandlers: KeyHandler, instructions: Option[Instructions.Clause]): Shortcuts =
      Shortcuts(keyHandlers.toKeyHandlers, instructions.toList)

    def option(criterion: Criterion, actionDesc: String, actionOption: Option[Callback]): Shortcuts =
      apply(
        criterion.handleWhenDefined(actionOption),
        actionOption.map(Instructions.Clause.keyToAction(criterion.desc)(actionDesc, _)))
  }

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
}
