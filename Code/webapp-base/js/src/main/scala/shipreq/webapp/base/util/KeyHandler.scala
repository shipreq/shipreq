package shipreq.webapp.base.util

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scala.collection.immutable.ListSet
import shipreq.webapp.base.util.KeyHandler._

final case class KeyHandler(criteria: Criteria, response: Response, eventDefault: Boolean = true) {

  def asNonDefault: KeyHandler =
    copy(eventDefault = false)

  def toKeyHandlers: KeyHandlers =
    KeyHandlers(this :: Nil)

  def toReact: TagMod =
    toKeyHandlers.toReact

  def +(k: KeyHandler): KeyHandlers =
    KeyHandlers(k :: this :: Nil)
}

object KeyHandler {

  @inline implicit def autoToRect(k: KeyHandler): TagMod =
    k.toReact

//  @inline implicit def autoPluralise(k: KeyHandler): KeyHandlers =
//    k.toKeyHandlers

  sealed abstract class EventType(val domKey: VdomAttr.Event[ReactKeyboardEventFrom])
  object EventType {
    case object KeyPress        extends EventType(^.onKeyPress)
    case object KeyDown         extends EventType(^.onKeyDown)
    case object KeyUp           extends EventType(^.onKeyUp)
    case object KeyPressCapture extends EventType(^.onKeyPressCapture)
    case object KeyDownCapture  extends EventType(^.onKeyDownCapture)
    case object KeyUpCapture    extends EventType(^.onKeyUpCapture)

    implicit def univEq: UnivEq[EventType] = UnivEq.derive
  }

  sealed abstract class ModKey {
    def isPressed(e: ReactKeyboardEvent): Boolean
  }
  object ModKey {
    case object Alt   extends ModKey { override def isPressed(e: ReactKeyboardEvent) = e.altKey }
    case object Ctrl  extends ModKey { override def isPressed(e: ReactKeyboardEvent) = e.ctrlKey }
    case object Shift extends ModKey { override def isPressed(e: ReactKeyboardEvent) = e.shiftKey }
    case object Meta  extends ModKey { override def isPressed(e: ReactKeyboardEvent) = e.metaKey }

    implicit def univEq: UnivEq[ModKey] = UnivEq.derive

    implicit def toSet(m: ModKey): ModKeys =
      ModKeys.empty + m
  }

  type ModKeys = ListSet[ModKey]
  object ModKeys {
    import ModKey._

    val All: ModKeys =
      Alt + Ctrl + Shift + Meta

    @inline def empty: ModKeys =
      ListSet.empty
  }

  // Technically this should be (EventType, List[Criterion])
  // where Criterion = KeyCode | KeyValue | KeyMod*
  final case class Criterion(desc     : String,
                             eventType: EventType,
                             keyCode  : Int,
                             modKeys  : ModKeys = ModKeys.empty) {

    def toCallbackOption(e: ReactKeyboardEvent): CallbackOption[Unit] =
      CallbackOption.require(satisfiedBy(e))

    def satisfiedBy(e: ReactKeyboardEvent): Boolean = {

      // Sometimes e.keyCode is undefined. I've noticed this when Chrome pops up suggestions of usernames it remembers
      // and I click one.
      val eventKeyCode: Int =
        try e.keyCode catch { case _: Throwable => Int.MinValue }

      (eventKeyCode ==* keyCode) &&
        ModKeys.All.forall(k => k.isPressed(e) ==* modKeys.contains(k))
    }

    def handle(response: Response): KeyHandler =
      KeyHandler(this, response)

    def handle(cb: Callback): KeyHandler =
      handle(_ => cb)

    def handleWhenDefined(cb: Option[Callback]): KeyHandler =
      handle(cb.getOrEmpty)

    def handleWhenDefined(cb: CallbackTo[Option[Callback]]): KeyHandler =
      handle(cb.flatten(_.getOrEmpty))
  }

  object Criterion {
    implicit def univEq: UnivEq[Criterion] = UnivEq.derive

    implicit val reusability: Reusability[Criterion] =
      Reusability.byRef

    implicit def toSet(m: Criterion): Criteria =
      Criteria.empty + m

    import org.scalajs.dom.ext.KeyCode
    private val ctrlOrCmd       = if (Browser.isMac) ModKey.Meta else ModKey.Ctrl
    private val ctrlOrCmdPrefix = if (Browser.isMac) "⌘ " else "ctrl-"
    private val altPrefix       = if (Browser.isMac) "⌥ " else "alt-"
    val Escape         = Criterion("esc",                     EventType.KeyDown,        KeyCode.Escape)
    val Enter          = Criterion("enter",                   EventType.KeyDown,        KeyCode.Enter)
    val AltEnter       = Criterion(altPrefix + "enter",       EventType.KeyDown,        KeyCode.Enter, ModKey.Alt)
    val AltT           = Criterion(altPrefix + "t",           EventType.KeyDown,        KeyCode.T,     ModKey.Alt)
    val AltZ           = Criterion(altPrefix + "z",           EventType.KeyDown,        KeyCode.Z,     ModKey.Alt)
    val CtrlOrCmdEnter = Criterion(ctrlOrCmdPrefix + "enter", EventType.KeyDown,        KeyCode.Enter, ctrlOrCmd)
    val CtrlOrCmdSpace = Criterion(ctrlOrCmdPrefix + "space", EventType.KeyDown,        KeyCode.Space, ctrlOrCmd)
    val TabCapture     = Criterion("tab",                     EventType.KeyDownCapture, KeyCode.Tab)
  }

  type Criteria = ListSet[Criterion]
  object Criteria {
    @inline def empty: Criteria =
      ListSet.empty
  }

  type Response = ReactKeyboardEvent => Callback
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

final case class KeyHandlers(handlers: List[KeyHandler]) extends AnyVal {

  def +(k: KeyHandler): KeyHandlers =
    KeyHandlers(k :: handlers)

  def ++(ks: KeyHandlers): KeyHandlers =
    // reverse_::: has most efficient concat implementation
    KeyHandlers(ks.handlers reverse_::: handlers)

  def toReact: TagMod = {
    var map = Map.empty[EventType, List[Response]]

    // Group by event type
    for {
      h <- handlers
      c <- h.criteria
    } {
      val response: Response =
        e => Callback.when(c satisfiedBy e) {
          val r = h response e
          if (h.eventDefault)
            r.asEventDefault(e).void
          else
            r
        }
      val cur = map.getOrElse(c.eventType, Nil)
      map = map.updated(c.eventType, response :: cur)
    }

    // Combine each event type
    TagMod.fromTraversableOnce(
      map.iterator.map { case (et, responses) =>
        val combinedResponse: ReactKeyboardEvent => Callback =
          e => Callback.sequence(responses.map(_ (e)))
        et.domKey ==> combinedResponse
      })
  }

  def baseToReact: KeyHandlers => TagMod = {
    val none = toReact
    more => if (more.handlers.isEmpty)
      none
    else
      (this ++ more).toReact
  }
}

object KeyHandlers {

  def empty = KeyHandlers(Nil)

  @inline implicit def autoToReact(k: KeyHandlers): TagMod =
    k.toReact

  @inline def base(keyHandlers: KeyHandlers): KeyHandlers => TagMod =
    keyHandlers.baseToReact
}
