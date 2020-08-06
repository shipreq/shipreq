package shipreq.webapp.base.feature.editcontrols

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.lib.KeyHandlers
import shipreq.webapp.base.text.LineCardinality
import shipreq.webapp.base.ui.OptionalFullscreen

final case class Control[-P](keys: CallbackTo[_ <: P] => KeyHandlers,
                             instruction: P => Option[Instructions.Clause])

object Control {
  val empty: Control[Any] =
    apply[Any](_ => KeyHandlers.empty, _ => None)
}

object Controls {

  def apply[P](lc: LineCardinality): Standard[P] = {
    import Control.{empty => z}
    new Standard(lc, z, z, z, None, None)
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  final class Standard[P](lc               : LineCardinality,
                          abort            : Control[P],
                          commit           : Control[P],
                          commitAndProgress: Control[P],
                          extras           : Option[CallbackTo[P] => ExtraControls],
                          help             : Option[Callback],
                         ) { self =>

    private def copy(abort            : Control[P]                             = self.abort,
                     commit           : Control[P]                             = self.commit,
                     commitAndProgress: Control[P]                             = self.commitAndProgress,
                     extras           : Option[CallbackTo[P] => ExtraControls] = self.extras,
                     help             : Option[Callback]                       = self.help,
                    ): Standard[P] =
      new Standard[P](
        lc                = lc,
        abort             = abort,
        commit            = commit,
        commitAndProgress = commitAndProgress,
        extras            = extras,
        help              = help,
      )

    private def allStatic: List[Control[P]] = (
      abort ::
      commit ::
      commitAndProgress ::
      Nil)

    def abort(f: P => Callback,
              verb: P => String = _ => Instructions.defaultAbortVerb): Standard[P] =
      copy(abort = Control[P](
        keys        = c => Keys.abort.handle(c.flatMap(f)).toKeyHandlers,
        instruction = p => Some(Instructions.Clause.abort(f(p), verb(p)))
      ))

    def abortWhenDefined(f   : P => Option[Callback],
                         verb: P => String = _ => Instructions.defaultAbortVerb): Standard[P] =
      copy(abort = Control[P](
        keys        = c => Keys.abort.handleWhenDefined(c.map(f)).toKeyHandlers,
        instruction = p => f(p).map(Instructions.Clause.abort(_, verb(p))),
      ))

    def commitWhenDefined(f   : P => Option[Callback],
                          verb: P => String = _ => Instructions.defaultCommitVerb): Standard[P] = {
      val control = Control[P](
        keys        = c => Keys.commit.handleWhenDefined(c.map(f)).toKeyHandlers,
        instruction = p => f(p).map(Instructions.Clause.commit(_, verb(p))),
      )
      copy(commit = control)
    }

    def commitAndProgress(f: P => Callback, verb: String): Standard[P] = {
      val control = Control[P](
        keys        = c => Keys.commitAndProgress.handle(c.flatMap(f)).toKeyHandlers,
        instruction = p => Some(Instructions.Clause.commitAndProgress(f(p), verb))
      )
      copy(commitAndProgress = control)
    }

    def commitAndProgressWhenDefined(f: P => Option[Callback], verb: String): Standard[P] = {
      val control = Control[P](
        keys        = c => Keys.commitAndProgress.handleWhenDefined(c.map(f)).toKeyHandlers,
        instruction = p => f(p).map(Instructions.Clause.commitAndProgress(_, verb)),
      )
      copy(commitAndProgress = control)
    }

    def withHelp(c: Callback): Standard[P] =
      copy(help = Some(c))

    def addExtras(f: CallbackTo[P] => ExtraControls): Standard[P] = {
      val e: CallbackTo[P] => ExtraControls =
        extras match {
          case Some(g) => p => g(p) ++ f(p)
          case None    => f
        }
      copy(extras = Some(e))
    }

    def addDynamicExtras(f: P => ExtraControls): ExtraDynamic[P] =
      new ExtraDynamic(this, f, None, None)

    // =================================================================================================================

    def keyHandlersPure(p: P): KeyHandlers =
      keyHandlers(CallbackTo.pure(p))

    def keyHandlers(f: CallbackTo[P]): KeyHandlers = {
      var k = KeyHandlers.empty
      k = allStatic.iterator.foldLeft(k)(_ ++ _.keys(f))
      for (e <- extras)
        k ++= e(f).keys
      k
    }

    def instructions(p: P): VdomTag =
      _instructions(p, None, None, ExtraControls.empty)

    private[Controls] def _instructions(p         : P,
                                        fullscreen: Option[OptionalFullscreen.Ctx],
                                        monospace : Option[StateSnapshot[Boolean]],
                                        extra     : ExtraControls): VdomTag = {
      var clauses = Instructions.Clauses(
        lc                = lc,
        abort             = abort.instruction(p),
        commit            = commit.instruction(p),
        commitAndProgress = commitAndProgress.instruction(p),
      )
      if (extra ne ExtraControls.empty)
        clauses = extra.instructions ::: clauses
      Instructions(
        clauses    = clauses,
        help       = help,
        fullscreen = fullscreen,
        monospace  = monospace,
      )
    }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  final class ExtraDynamic[P](standard  : Standard[P],
                              extras    : P => ExtraControls,
                              fullscreen: Option[OptionalFullscreen.Ctx],
                              monospace : Option[StateSnapshot[Boolean]],
                             ) { self =>

    private def copy(standard  : Standard[P]                    = self.standard,
                     extras    : P => ExtraControls             = self.extras,
                     fullscreen: Option[OptionalFullscreen.Ctx] = self.fullscreen,
                     monospace : Option[StateSnapshot[Boolean]] = self.monospace,
                    ): ExtraDynamic[P] =
      new ExtraDynamic(standard, extras, fullscreen, monospace)

    def addExtra(f: P => ExtraControls): ExtraDynamic[P] =
      copy(extras = p => extras(p) ++ f(p))

    def withFullscreenCtx(f: Option[OptionalFullscreen.Ctx]): ExtraDynamic[P] =
      copy(fullscreen = f)

    def withMonospace(ss: StateSnapshot[Boolean]): ExtraDynamic[P] =
      copy(monospace = Some(ss))

    def keyHandlers(p: P): KeyHandlers = {
      var k = standard.keyHandlersPure(p) ++ extras(p).keys
      for (f <- fullscreen)
        k += Keys.fullscreen.handle(f.toggleFullscreen)
      for (ss <- monospace)
        k += Keys.monospace.handle(ss.modState(!_))
      k
    }

    def instructions(p: P): VdomTag =
      standard._instructions(p, fullscreen, monospace, extras(p))
  }
}
