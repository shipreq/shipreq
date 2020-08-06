package shipreq.webapp.base.feature.editcontrols

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.feature.EditControlsFeature
import shipreq.webapp.base.text.{LineCardinality, MultiLine, SingleLine}
import shipreq.webapp.base.ui.BaseStyles.{editorInstructions => *}
import shipreq.webapp.base.ui.OptionalFullscreen
import shipreq.webapp.base.ui.semantic.Icon

object Instructions {

  final val defaultAbortVerb  = "cancel"
  final val defaultCommitVerb = "save"

  sealed trait Atom
  final case class Vdom(value: TagMod) extends Atom
  final case class Link(label: TagMod, onClick: Callback) extends Atom

  type Clause = NonEmptyVector[Atom]
  object Clause {

    def keyToAction(key: String)(action: String, actionCB: Callback): Clause =
      NonEmptyVector(Vdom(key + " to "), Vector.empty :+ Link(action, actionCB))

    def abort(c: Callback, verb: String): Clause =
      keyToAction(Keys.abort.desc)(verb, c)

    def commit(c: Callback, verb: String): Clause =
      keyToAction(Keys.commit.desc)(verb, c)

    def commitAndProgress(c: Callback, verb: String): Clause =
      keyToAction(Keys.commitAndProgress.desc)(verb, c)

    val multiLine: Clause =
      NonEmptyVector one Vdom("enter for new line")
  }

  object Clauses {
    def apply(lc               : LineCardinality,
              commit           : Option[Clause],
              commitAndProgress: Option[Clause],
              abort            : Option[Clause]): List[Clause] = {
      var clauses = List.empty[Clause]
      abort            .foreach(clauses ::= _)
      commitAndProgress.foreach(clauses ::= _)
      commit           .foreach(clauses ::= _)
      lc match {
        case SingleLine => ()
        case MultiLine  => clauses ::= Clause.multiLine
      }
      clauses
    }
  }

  private val link          : VdomTag = <.a(*.link)
  private val clauseCont    : VdomTag = <.span(*.clause)
  private val comma         : TagMod  = ","
  private val fullStop      : TagMod  = "."
  private val helpIcon      : VdomTag = Icon.HelpCircle.tag(*.icon, ^.title := "help")
  private val fullscreenIcon: VdomTag = Icon.Maximize.tag(*.icon, ^.title := "fullscreen")
  private val monospaceIcon : VdomTag = Icon.TextWidth.tag(*.icon, ^.title := "use monospace font")

  private val renderAtom: Atom => TagMod = {
    case Vdom(v)    => v
    case Link(v, c) => link(^.onClick --> c, v)
  }

  def apply(clauses   : IterableOnce[Clause],
            help      : Option[Callback],
            fullscreen: Option[OptionalFullscreen.Ctx],
            monospace : Option[StateSnapshot[Boolean]]): VdomTag = {

    val buttons: TagMod = {
      val helpButton =
        help.whenDefined { h =>
          val eh = preventDefaultAndStopPropagation.andThen(_ >> h)
          helpIcon(^.onClick ==> eh)
        }

      val toggleFullscreenButton =
        fullscreen.whenDefined { ctx =>
          val eh = preventDefaultAndStopPropagation.andThen(_ >> ctx.toggleFullscreen)
          fullscreenIcon(^.onClick ==> eh)
        }

      val toggleMonospace =
        monospace.whenDefined { ss =>
          val eh = preventDefaultAndStopPropagation.andThen(_ >> ss.modState(!_))
          monospaceIcon(^.onClick ==> eh)
        }

      TagMod(
        toggleMonospace,
        toggleFullscreenButton,
        helpButton,
      )
    }

    val content: TagMod =
      if (clauses.iterator.isEmpty)
        buttons
      else {
        var rendered = Vector.empty[TagMod]
        val it = clauses.iterator
        var last: VdomTag = null
        while (it.hasNext) {
          val clause = it.next()
          val suffix = if (it.hasNext) comma else fullStop
          last = clauseCont(TagMod.Composite(clause.whole.map(renderAtom) :+ suffix))
          rendered :+= last
        }

        // Here we add the buttons to the last clause.
        // The reason is that we don't want word-wrapping to occur between the last clause and the buttons
        // because a lone, tiny help button on its own line looks terrible.
        rendered = rendered.dropRight(1) :+ last(buttons)

        TagMod.Composite(rendered)
      }

    val mode = EditControlsFeature.Mode.derive(fullscreen)

    <.div(*.container(mode), content)
  }
}
