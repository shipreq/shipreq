package shipreq.webapp.client.base.lib

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.univeq._
import scala.collection.immutable.ListSet
import shipreq.webapp.client.base.lib.KeyHandler._

case class KeyHandler(criteria: Criteria, response: Response) {
  def asEventDefault: KeyHandler =
    KeyHandler(criteria, e => CallbackOption.asEventDefault(e, response(e)))

  def toReact: TagMod =
    KeyHandler.toReact(this :: Nil)
}

object KeyHandler {

  sealed abstract class EventType(val domKey: ReactAttr)
  object EventType {
    case object KeyPress extends EventType(^.onKeyPress)
    case object KeyDown  extends EventType(^.onKeyDown)
    case object KeyUp    extends EventType(^.onKeyUp)

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
  case class Criterion(eventType: EventType, keyCode: Int, modKeys: ModKeys = ModKeys.empty) {
    def satisfiedBy(e: ReactKeyboardEvent): Boolean =
      (e.keyCode ==* keyCode) &&
      ModKeys.All.forall(k => k.isPressed(e) ==* modKeys.contains(k))

    def handle(response: Response): KeyHandler =
      KeyHandler(this, response)

    def handle(cb: => Callback): KeyHandler =
      handle(_ => cb)
  }

  object Criterion {
    implicit def univEq: UnivEq[Criterion] = UnivEq.derive

    implicit def toSet(m: Criterion): Criteria =
      Criteria.empty + m
  }

  type Criteria = ListSet[Criterion]
  object Criteria {
    @inline def empty: Criteria =
      ListSet.empty
  }

  type Response = ReactKeyboardEvent => Callback

  def toReact(handlers: TraversableOnce[KeyHandler]): TagMod = {
    var map = Map.empty[EventType, List[Response]]

    // Group by event type
    for {
      h <- handlers
      c <- h.criteria
    } {
      val response: Response =
        e => Callback.when(c satisfiedBy e)(h response e)
      val cur = map.getOrElse(c.eventType, Nil)
      map = map.updated(c.eventType, response :: cur)
    }

    // Combine each event type
    map.foldLeft(EmptyTag) {
      case (q, (et, responses)) =>
        val combinedResponse: ReactKeyboardEvent => Callback =
          e => Callback.sequence(responses.map(_(e)))
        q + (et.domKey ==> combinedResponse)
    }
  }
}
