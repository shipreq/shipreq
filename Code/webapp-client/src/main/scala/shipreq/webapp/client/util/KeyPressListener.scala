package shipreq.webapp.client.util

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import org.scalajs.dom.{console, document, KeyboardEvent}
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.raw.HTMLBodyElement
import scala.collection.immutable.BitSet
import scalaz.effect.IO

object KeyPressListener {

  def install[P, S, B <: KeyPressListener, N <: TopNode](useCapture: Boolean = false) =
    EventListener[KeyboardEvent].install[P,S,B,N]("keydown", _.backend._onKeyDown, _ => document, useCapture) compose
    EventListener[KeyboardEvent].install[P,S,B,N]("keyup",   _.backend._onKeyUp,   _ => document, useCapture)

  val modKeys: BitSet =
    BitSet(KeyCode.Alt, KeyCode.Ctrl, KeyCode.Shift,
      91, 92, 93, 224, // OSLeft & OSRight
      225)             // AltGraph
}

trait KeyPressListener extends OnUnmount {

  private[this] var _keysDown = BitSet.empty
  private[this] var _modsDown = BitSet.empty

  final def modsDown = _modsDown
  final def keysDown = _keysDown

  private def _keychange(e: KeyboardEvent)(f: (BitSet, Int) => BitSet): Int = {
    val i = e.keyCode
    if (KeyPressListener.modKeys contains i)
      _modsDown = f(_modsDown, i)
    else
      _keysDown = f(_keysDown, i)

    // keyUp events are not received in all cases (like alt-tab)
    // Fix what we can.
    def setMod(code: Int, on: Boolean): Unit =
      _modsDown = if (on) _modsDown + code else _modsDown - code
    setMod(KeyCode.Alt,   e.altKey)
    setMod(KeyCode.Ctrl,  e.ctrlKey)
    setMod(KeyCode.Shift, e.shiftKey)

    i
  }

  final def _onKeyDown(e: KeyboardEvent): Unit = {
    _keychange(e)(_ + _)
    // console.log(s"onKeyDown : ${_modsDown} : ${e.keyCode} ", e)
    onKeyDown(e).unsafePerformIO()
  }

  final def _onKeyUp(e: KeyboardEvent): Unit = {
    _keychange(e)(_ - _)
    // console.log(s"onKeyUp : ${_modsDown} : ${e.keyCode} ", e)
  }

  def onKeyDown(e: KeyboardEvent): IO[Unit]

  // ---------------------------------------------------------------------------
  // Helpers

  protected type KbEventH [O] = KeyboardEvent => O
  protected type KbEventHO[O] = KbEventH[Option[O]]

  protected def matchKeyCodeNoMods[O](pf: PartialFunction[Int, O]): KbEventHO[O] = {
    val pf2 = pf.lift
    e => if (modsDown.isEmpty) pf2(e.keyCode) else {
      // console.log(s"Dropping KB event because mods are down. $modsDown ", e)
      None
    }
  }

  /**
   * Ignores the event if it has a target with focus (like an &lt;input&gt;, &lt;textarea&gt; etc).
   */
  protected def filterUntargeted[O](f: KbEventHO[O]): KbEventHO[O] =
    e => e.target match {
      case _: HTMLBodyElement => f(e)
      case _ =>
        // console.log("Dropping KB event with wrong target: ", e)
        None
    }

  protected def consumeHandledKbEvent[A](f: KbEventHO[IO[A]]): KbEventHO[IO[A]] =
    e => f(e).map(io =>
      IO {
        e.preventDefault()
        e.stopPropagation()
      }.flatMap(_ => io)
    )
}
