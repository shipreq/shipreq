package shipreq.webapp.base.protocol

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.utils.StaticLookupFn
import japgolly.univeq.UnivEq
import org.scalajs.dom.raw
import org.scalajs.dom.raw.{Blob, CloseEvent, Event, MessageEvent}
import scala.scalajs.js
import scala.scalajs.js.typedarray.ArrayBuffer
import shipreq.base.util.VarJs
import shipreq.webapp.base.protocol.WebSocketShared.CloseReason

trait WebSocket {
  import WebSocket._

  val binaryType: VarJs[BinaryType]
  val onOpen    : VarJs[js.Function1[Event, _]]
  val onMessage : VarJs[js.Function1[MessageEvent, _]]
  val onClose   : VarJs[js.Function1[CloseEvent, _]]
  val onError   : VarJs[js.Function1[Event, _]]

  def bufferedAmount(): Int
  def extensions(): String
  def readyState(): ReadyState
  val url: String

  def close(reason: CloseReason): Unit

  def send(data: ArrayBuffer): Unit
  def send(data: Blob): Unit
  def send(data: String): Unit
}

object WebSocket {

  sealed abstract class ReadyState(final val jsValue: Int)
  object ReadyState {
    case object Connecting extends ReadyState(0)
    case object Open       extends ReadyState(1)
    case object Closing    extends ReadyState(2)
    case object Closed     extends ReadyState(3)

    implicit def univEq: UnivEq[ReadyState] = UnivEq.derive
    val values = AdtMacros.adtValues[ReadyState]
    val byJsValue = StaticLookupFn.useArrayBy(values.whole)(_.jsValue).total
  }

  sealed abstract class BinaryType(final val jsValue: String)
  object BinaryType {
    case object Blob extends BinaryType("blob")
    case object ArrayBuffer extends BinaryType("arraybuffer")

    val values = AdtMacros.adtValues[BinaryType]
    val byJsValue = StaticLookupFn.useMapBy(values.whole)(_.jsValue).total
  }

  // ===================================================================================================================

  def apply(url: String): WebSocket =
    apply(new raw.WebSocket(url))

  def apply(url: String, protocol: String): WebSocket =
    apply(new raw.WebSocket(url, protocol))

  def apply(underlying: raw.WebSocket): WebSocket =
    Real(underlying)

  private final case class Real(underlying: raw.WebSocket) extends WebSocket {
    override val binaryType = VarJs.unsafeField[String](underlying, "binaryType").xmap(BinaryType.byJsValue)(_.jsValue)
    override val onOpen     = VarJs.unsafeField[js.Function1[Event, _]]       (underlying, "onopen")
    override val onMessage  = VarJs.unsafeField[js.Function1[MessageEvent, _]](underlying, "onmessage")
    override val onClose    = VarJs.unsafeField[js.Function1[CloseEvent, _]]  (underlying, "onclose")
    override val onError    = VarJs.unsafeField[js.Function1[Event, _]]       (underlying, "onerror")

    override def bufferedAmount() = underlying.bufferedAmount
    override def extensions()     = underlying.extensions
    override def readyState()     = ReadyState.byJsValue(underlying.readyState)
    override val url              = underlying.url

    override def close(reason: CloseReason) = underlying.close(reason.code.value, reason.phrase.value)

    override def send(data: ArrayBuffer) = underlying.send(data)
    override def send(data: Blob)        = underlying.send(data)
    override def send(data: String)      = underlying.send(data)

  }
}
