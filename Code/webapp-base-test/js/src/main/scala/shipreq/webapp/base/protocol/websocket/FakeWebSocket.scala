package shipreq.webapp.base.protocol.websocket

import org.scalajs.dom.raw.{Blob, CloseEvent, Event, MessageEvent, MessageEventInit}
import org.scalajs.dom.{console, raw}
import scala.scalajs.js
import scala.scalajs.js.typedarray
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}
import shipreq.base.util.{BinaryData, BinaryJs, VarJs}
import shipreq.webapp.base.protocol.websocket.WebSocket._
import shipreq.webapp.base.protocol.websocket.WebSocketShared.{CloseCode, CloseReason, CloseReasonPhrase}

final class FakeWebSocket(override val url: String, initialState: ReadyState = ReadyState.Connecting) extends WebSocket {
  import FakeWebSocket._

  override def toString: String = s"FakeWebSocket{readyState=${readyState()}, sent=${_sent.length}, pendingResponse=${_pendingResponse.length}}"

  override val binaryType = VarJs.free[BinaryType]                   (BinaryType.Blob)
  override val onOpen     = VarJs.free[js.Function1[Event, _]]       ((_: Event) => ())
  override val onMessage  = VarJs.free[js.Function1[MessageEvent, _]]((_: MessageEvent) => ())
  override val onClose    = VarJs.free[js.Function1[CloseEvent, _]]  ((_: CloseEvent) => ())
  override val onError    = VarJs.free[js.Function1[Event, _]]       ((_: Event) => ())
  val onSend              = VarJs.free[Message => Unit]              ((m: Message) => println(s"[FakeWebSocket.onSend] Sent: $m"))

  private var _readyState: ReadyState = initialState
  private var _bufferedAmount = 0
  private var _sent = Vector.empty[Message]
  private var _pendingResponse = Vector.empty[Message]

  override def bufferedAmount() = _bufferedAmount
  override def extensions()     = ""
  override def readyState()     = _readyState
  def sent()                    = _sent
  def responsesPending()        = _pendingResponse.nonEmpty

  def open(): Unit =
    readyState() match {
      case ReadyState.Open       => ()
      case ReadyState.Closing
         | ReadyState.Closed     => sys.error("WebSocket is already in CLOSING or CLOSED state.")
      case ReadyState.Connecting =>
        _readyState = ReadyState.Open
        onOpen.get()(new Event("open"))
    }

  def closing(): Unit =
    readyState() match {
      case ReadyState.Closing    => ()
      case ReadyState.Closed     => sys.error("WebSocket is already in CLOSED state.")
      case ReadyState.Connecting
         | ReadyState.Open       =>
        _readyState = ReadyState.Closing
    }

  def close(): Unit =
    close(CloseCode.noStatusCode)

  def close(code: CloseCode): Unit =
    close(CloseReason(code, CloseReasonPhrase.empty))

  override def close(reason: CloseReason): Unit =
    readyState() match {
      case ReadyState.Closed     => ()
      case ReadyState.Connecting
         | ReadyState.Open
         | ReadyState.Closing    =>
        _readyState = ReadyState.Closed
        val event = new Event("close").asInstanceOf[CloseEvent]
        val e = event.asInstanceOf[js.Dynamic]
        e.code      = reason.code.value
        e.reason    = reason.phrase.value
        e.wasClean  = true
      //e.isTrusted = true
        onClose.get()(event)
    }

  override def send(data: String)      = sendMsg(Message.Text(data))
  override def send(data: ArrayBuffer) = sendMsg(Message.ArrayBuffer(data))
  override def send(data: Blob)        = sendMsg(Message.Blob(data))

  private def sendMsg(m: Message): Unit = {
    val b = m.binaryData
    _bufferedAmount += b.length
    _sent :+= m
    if (m.binaryData.length > 0)
      _pendingResponse :+= m
    readyState() match {
      case ReadyState.Connecting => throw js.JavaScriptException("WebSocket is not open: readyState 0 (CONNECTING)")
      case ReadyState.Closing
         | ReadyState.Closed     => console.error("WebSocket is already in CLOSING or CLOSED state.")
      case ReadyState.Open       =>
        onSend.get()(m)
        _bufferedAmount -= b.length
    }
  }

  def recv(m: Message): Unit =
    m match {
      case Message.Text       (a) => recv(a)
      case Message.Blob       (a) => recv(a)
      case Message.ArrayBuffer(a) => recv(a)
    }

  def recv(data: String): Unit =
    recvMsg(data)

  def recv(data: ArrayBuffer): Unit =
    recvMsg(binaryType.get() match {
      case BinaryType.ArrayBuffer => data
      case BinaryType.Blob        => BinaryJs.arrayBufferToBlob(data)
    })

  def recv(data: Blob): Unit =
    recvMsg(binaryType.get() match {
      case BinaryType.ArrayBuffer => BinaryJs.blobToArrayBuffer(data)
      case BinaryType.Blob        => data
    })

  private def recvMsg(m: Any): Unit =
    readyState() match {
      case ReadyState.Open =>
        val p = js.Dynamic.literal(
          data      = m.asInstanceOf[js.Any],
        //isTrusted = true,
        ).asInstanceOf[MessageEventInit]
        val e = new MessageEvent("message", p)
        onMessage.get()(e)

      case s => sys.error(s"WebSocket isn't open. ReadyState = $s")
    }

  def respondToNextPending(respondWith: Message => Message): Unit =
    if (responsesPending()) {
      val i = _pendingResponse.head
      _pendingResponse = _pendingResponse.tail
      val o = respondWith(i)
      recv(o)
    }
}

// =====================================================================================================================

object FakeWebSocket {

  sealed trait Message {
    val binaryData: BinaryData
  }

  object Message {
    final case class Text(value: String) extends Message {
      override val binaryData = BinaryData.unsafeFromArray(value.getBytes("UTF-8"))
    }

    final case class Blob(value: raw.Blob) extends Message {
      override def toString = s"Blob(${binaryData.describe()})"
      val arrayBuffer = BinaryJs.blobToArrayBuffer(value)
      val byteBuffer = TypedArrayBuffer.wrap(arrayBuffer)
      override val binaryData = BinaryData.fromByteBuffer(byteBuffer)
    }

    final case class ArrayBuffer(value: typedarray.ArrayBuffer) extends Message {
      override def toString = s"ArrayBuffer(${binaryData.describe()})"
      val byteBuffer = TypedArrayBuffer.wrap(value)
      override val binaryData = BinaryData.fromByteBuffer(byteBuffer)
    }
  }

}