package shipreq.webapp.base.jsfacade

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.|

@JSGlobal("LRUC")
@js.native
@nowarn
final class LRUCache(options: LRUCache.Options | Double) extends js.Any {
  import LRUCache._

  def set(key: Key, value: Value, maxAge: Double = js.native): Unit = js.native

  def get(key: Key): js.UndefOr[Value] = js.native

  def peek(key: Key): js.UndefOr[Value] = js.native

  def del(key: Key): Unit = js.native

  def reset(): Unit = js.native

  def keys(): js.Array[Key] = js.native

  def values(): js.Array[Value] = js.native

  def length: Double = js.native

  def itemCount: Int = js.native

  def has(key: Key): Boolean = js.native

  def prune(): Unit = js.native
}

object LRUCache {

  type Value   = Any
  type Key     = String | AnyRef
  type Length  = js.Function2[Value, Key, Double]
  type Dispose = js.Function2[Key, Value, Unit]

  @js.native
  @nowarn
  sealed trait Options extends js.Object {
    var max           : js.UndefOr[Double ] = js.native
    var length        : js.UndefOr[Length ] = js.native
    var dispose       : js.UndefOr[Dispose] = js.native
    var maxAge        : js.UndefOr[Double ] = js.native
    var stale         : js.UndefOr[Boolean] = js.native
    var noDisposeOnSet: js.UndefOr[Boolean] = js.native
    var updateAgeOnGet: js.UndefOr[Boolean] = js.native
  }
}