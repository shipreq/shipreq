package shipreq.webapp.client.util

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import org.scalajs.dom.{console, document, Event, KeyboardEvent}
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.raw.HTMLBodyElement
import scala.collection.immutable.BitSet
import scalajs.js
import scalaz.effect.IO

object EventListener {

  /**
   *
   * @param eventType A string representing the
   *                  <a href="https://developer.mozilla.org/en-US/docs/DOM/event.type">event type</a> to listen for.
   * @param useCapture If true, useCapture indicates that the user wishes to initiate capture.
   *                   After initiating capture, all events of the specified type will be dispatched to the registered
   *                   listener before being dispatched to any EventTarget beneath it in the DOM tree.
   *                   Events which are bubbling upward through the tree will not trigger a listener designated to use
   *                   capture.
   */
  def install[P, S, B <: OnUnmount, E <: Event](eventType: String,
                                                listener: ComponentScopeM[P, S, B] => E => Unit,
                                                useCapture: Boolean = false) =
    OnUnmount.install compose ((_: ReactComponentB[P, S, B])
      .componentDidMount($ => {
      val fe = listener($)
      val f1: js.Function1[E, Unit] = (e: E) => fe(e)
      val f2 = f1.asInstanceOf[js.Function1[Event, Unit]] // TODO Workaround for scala-js-dom 0.8.0
      document.addEventListener(eventType, f1, useCapture)
      $.backend.onUnmount(document.removeEventListener(eventType, f2, useCapture))
    }))


  def installIO[P, S, B <: OnUnmount, E <: Event](eventType: String,
                                                listener: ComponentScopeM[P, S, B] => E => IO[Unit],
                                                useCapture: Boolean = false) =
    install[P, S, B, E](eventType, $ => { val f = listener($); f(_).unsafePerformIO() }, useCapture)
}


object KeyPressListener {

  def install[P, S, B <: KeyPressListener](useCapture: Boolean = false) =
    EventListener.install[P,S,B,KeyboardEvent]("keydown", _.backend._onKeyDown, useCapture) compose
    EventListener.install[P,S,B,KeyboardEvent]("keyup",   _.backend._onKeyUp,   useCapture)

  val modKeys: BitSet =
    BitSet(KeyCode.alt, KeyCode.ctrl, KeyCode.shift,
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
    setMod(KeyCode.alt,   e.altKey)
    setMod(KeyCode.ctrl,  e.ctrlKey)
    setMod(KeyCode.shift, e.shiftKey)

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
