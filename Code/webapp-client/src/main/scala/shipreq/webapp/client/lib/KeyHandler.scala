package shipreq.webapp.client.lib

import japgolly.scalajs.react._
import org.scalajs.dom.ext.KeyValue
import scala.runtime.AbstractFunction1
import scalajs.js.{UndefOr, undefined}
import scalaz.{Monoid, \/}
import shipreq.base.util.ScalaExt._

final class KeyHandler(val run: ReactKeyboardEventH => Option[Callback]) extends AbstractFunction1[ReactKeyboardEventH, Callback] {
  override def apply(e: ReactKeyboardEventH): Callback =
    for {
      _  <- CallbackOption unless e.defaultPrevented
      cb <- CallbackOption liftOptionLike run(e)
      _  <- cb
      _  <- e.preventDefaultCB
    } yield ()

  def |(b: KeyHandler): KeyHandler =
    new KeyHandler(e => run(e) orElse b.run(e))

  def filter(f: ReactKeyboardEventH => Boolean): KeyHandler =
    new KeyHandler(e => if (f(e)) run(e) else None)

  def filterModKeys(alt  : Boolean = false,
                    ctrl : Boolean = false,
                    shift: Boolean = false,
                    meta : Boolean = false): KeyHandler =
    filter(KeyHandler.modKeys(_, alt, ctrl, shift, meta))
}

object KeyHandler {
  def apply(f: ReactKeyboardEventH => Option[Callback]): KeyHandler =
    new KeyHandler(f)

  private def reshapePF[A](pf: PartialFunction[A, Option[Callback]]): A => Option[Callback] =
    a => pf.applyOrElse(a, (_: A) => None)

  def pf(pf: PartialFunction[ReactKeyboardEventH, Option[Callback]]): KeyHandler =
    new KeyHandler(reshapePF(pf))

  def by[A](f: ReactKeyboardEventH => A)(pf: PartialFunction[A, Option[Callback]]): KeyHandler =
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

  implicit val monoid: Monoid[KeyHandler] =
    new Monoid[KeyHandler] {
      override def zero = KeyHandler(_ => None)
      override def append(a: KeyHandler, b: => KeyHandler) = a | b
    }
}

// =====================================================================================================================

final class KeyHandlers(val onKeyDown: Option[KeyHandler], val onKeyPress: Option[KeyHandler]) {

  def +(that: KeyHandlers): KeyHandlers = {
    new KeyHandlers(
      this.onKeyDown  ++ that.onKeyDown,
      this.onKeyPress ++ that.onKeyPress)
  }

  import japgolly.scalajs.react._, vdom.prefix_<^._
  def tagMod: TagMod = {
    var r = EmptyTag
    onKeyDown .foreach(r += ^.onKeyDown  ==> _)
    onKeyPress.foreach(r += ^.onKeyPress ==> _)
    r
  }
}

object KeyHandlers {

  def apply(onKeyDown : UndefOr[KeyHandler] = undefined,
            onKeyPress: UndefOr[KeyHandler] = undefined): KeyHandlers =
    new KeyHandlers(onKeyDown.toOption, onKeyPress.toOption)

  def empty = apply()

  @inline implicit def toTagMod(k: KeyHandlers) = k.tagMod

  def abort(abort: => Callback): KeyHandlers =
    KeyHandlers(onKeyDown = KeyHandler.byKey { case KeyValue.Escape => abort.some })

  /**
   * - Enter either inserts a newline, or commits.
   * - Ctrl-enter commits.
   */
  def commit(commit: => Option[Callback], singleLine: Boolean): KeyHandlers = {
    val commitOnEnter = KeyHandler.byKey { case KeyValue.Enter => commit }
    val base = KeyHandlers(onKeyDown = commitOnEnter.filterModKeys(ctrl = true))

    // If enter unused, use for commit too
    if (singleLine)
      base + KeyHandlers(onKeyPress = commitOnEnter.filterModKeys())
    else
      base
  }

  def commitDisjunction[A](parsed: Any \/ A)(f: A => Callback, singleLine: Boolean): KeyHandlers =
    commit(parsed.fold(_ => None, f(_).some), singleLine)
}
