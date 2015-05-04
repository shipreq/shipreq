package shipreq.webapp.client.lib.ui

import japgolly.scalajs.react._, ScalazReact._
import org.scalajs.dom.ext.KeyValue
import scala.runtime.AbstractFunction1
import scalajs.js.{UndefOr, undefined}
import scalaz.effect.IO
import shipreq.base.util.UndefOrExt
import shipreq.base.util.effect.IoUtils, IoUtils.IoExt

final class KeyHandler(val run: ReactKeyboardEventH => UndefOr[IO[Unit]]) extends AbstractFunction1[ReactKeyboardEventH, IO[Unit]] {
  override def apply(e: ReactKeyboardEventH): IO[Unit] =
    run(e).fold(IoUtils.nop)(e.preventDefaultIO >>> e.stopPropagationIO >>> _)

  def |(b: KeyHandler): KeyHandler =
    new KeyHandler(e => run(e) orElse b.run(e))

  def filter(f: ReactKeyboardEventH => Boolean): KeyHandler =
    new KeyHandler(e => if (f(e)) run(e) else undefined)

  def filterModKeys(alt  : Boolean = false,
                    ctrl : Boolean = false,
                    shift: Boolean = false,
                    meta : Boolean = false): KeyHandler =
    filter(KeyHandler.modKeys(_, alt, ctrl, shift, meta))
}

object KeyHandler {
  def apply(f: ReactKeyboardEventH => UndefOr[IO[Unit]]): KeyHandler =
    new KeyHandler(f)

  private def reshapePF[A](pf: PartialFunction[A, UndefOr[IO[Unit]]]): A => UndefOr[IO[Unit]] =
    a => pf.applyOrElse(a, (_: A) => undefined)

  def pf(pf: PartialFunction[ReactKeyboardEventH, UndefOr[IO[Unit]]]): KeyHandler =
    new KeyHandler(reshapePF(pf))

  def by[A](f: ReactKeyboardEventH => A)(pf: PartialFunction[A, UndefOr[IO[Unit]]]): KeyHandler =
    new KeyHandler(reshapePF(pf) compose f)

  val byKey = by(_.key) _

  def modKeys(e    : ReactKeyboardEventH,
              alt  : Boolean = false,
              ctrl : Boolean = false,
              shift: Boolean = false,
              meta : Boolean = false): Boolean =
    e.altKey   == alt   &&
    e.ctrlKey  == ctrl  &&
    e.shiftKey == shift &&
    e.metaKey  == meta
}

// =====================================================================================================================

final class KeyHandlers(val onKeyDown: UndefOr[KeyHandler], val onKeyPress: UndefOr[KeyHandler]) {

  def +(that: KeyHandlers): KeyHandlers =
    new KeyHandlers(
      UndefOrExt.append(this.onKeyDown,  that.onKeyDown) (_ | _),
      UndefOrExt.append(this.onKeyPress, that.onKeyPress)(_ | _))

  import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
  def tagMod: TagMod = {
    var r = EmptyTag
    onKeyDown .foreach(r += ^.onKeyDown  ~~> _)
    onKeyPress.foreach(r += ^.onKeyPress ~~> _)
    r
  }
}

object KeyHandlers {

  def apply(onKeyDown : UndefOr[KeyHandler] = undefined,
            onKeyPress: UndefOr[KeyHandler] = undefined): KeyHandlers =
    new KeyHandlers(onKeyDown, onKeyPress)

  @inline implicit def toTagMod(k: KeyHandlers) = k.tagMod

  /**
   * - Escape aborts.
   * - Enter either inserts a newline, or commits.
   * - Ctrl-enter commits.
   */
  def commitAndAbort(abort: => IO[Unit], commit: => UndefOr[IO[Unit]], singleLine: Boolean) = {
    val cancelOnEscape = KeyHandler.byKey { case KeyValue.Escape => abort }
    val commitOnEnter  = KeyHandler.byKey { case KeyValue.Enter => commit }

    var r = KeyHandlers(
      onKeyDown = cancelOnEscape | commitOnEnter.filterModKeys(ctrl = true))

    // If enter unused, use for commit too
    if (singleLine)
      r += KeyHandlers(onKeyPress = commitOnEnter.filterModKeys())

    r
  }
}
