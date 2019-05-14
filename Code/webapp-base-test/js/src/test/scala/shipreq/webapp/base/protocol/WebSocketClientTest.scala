package shipreq.webapp.base.protocol

import boopickle.Pickler
import japgolly.scalajs.react.{AsyncCallback, Callback, CallbackTo}
import scala.util.Try
import scalaz.\/-
import utest._
import shipreq.base.util.{Retries, Url}
import shipreq.base.util.JsExt._
import shipreq.webapp.base.lib.LoggerJs
import shipreq.webapp.base.protocol.FakeWebSocket.Message
import shipreq.webapp.base.protocol.WebSocketShared.ReqId
import shipreq.webapp.base.protocol.WebSocket.ReadyState
import shipreq.webapp.base.test.WebappTestUtil._

object WebSocketClientTest extends TestSuite {
  import BoopickleMacros._
  import BinCodecGeneric._

  private final case class ReqMsg(msg: Int)
  private final case class ResMsg(msg: Int)
  private final case class PushMsg(msg: String)

  private object P extends  Protocol.WebSocket.ClientReqServerPush[Pickler] {
    override type ReqId  = WebSocketShared.ReqId
    override type ReqRes = Protocol.RequestResponse.Simple[Pickler, ReqMsg, ResMsg]
    override val url     = Url.Relative("/x")
    override val req     = Protocol(pickleCaseClass[ReqMsg])
    override val push    = Protocol(pickleCaseClass[PushMsg])
    val res              = Protocol(pickleCaseClass[ResMsg])
    val ReqRes: ReqRes   = Protocol.RequestResponse.simple(res)
  }

  private val serverProtocols = WebSocketServerHelper(P)

  private class Tester(initialState: ReadyState = ReadyState.Connecting) {
    var webSockets = Vector.empty[FakeWebSocket]

    var receivedPushes = Vector.empty[PushMsg]
    var receivedResponses = Vector.empty[ResMsg]

    val newWS = CallbackTo {
      val f = new FakeWebSocket("fake url", initialState)
      webSockets :+= f
      f
    }

    val client = WebSocketClient(newWS, P, Retries.none)
      .build(
        p => Callback(receivedPushes :+= p),
        _ => _=> Callback.empty,
        LoggerJs.off)

    def ws() = webSockets.last
    def latestMsg() = webSockets.reverseIterator.flatMap(_.sent().reverseIterator).next()

    def send(req: ReqMsg): AsyncCallback[ResMsg] =
      client.send(P.ReqRes)(req).runNow()

    object server {
      def parseRequest(msg: Message = latestMsg()) =
        BinaryJs.decodeUnsafe(msg.binaryData, serverProtocols.protocolCS)

      def respondBy(f: ReqMsg => ResMsg) = {
        val (reqId, reqMsg) = parseRequest()
        respond(reqId, f(reqMsg))
      }

      def respond(reqId: ReqId, resMsg: ResMsg) = {
        val res = \/-((reqId, P.res.andValue(resMsg)))
        val bd = BinaryJs.encode(serverProtocols.protocolSC)(res)
        ws().recv(Message.ArrayBuffer(bd.toArrayBuffer))
      }
    }

    def awaitAsyncCallback[A](a: AsyncCallback[A]): Try[A] = {
      var t = Option.empty[Try[A]]
      a.attemptTry.tap(r => t = Some(r)).toCallback.runNow()
      if (t.isEmpty) fail("AsyncCallback isn't complete.")
      t.get
    }

    def assertAsyncCallbackAlreadyFailed[A](a: AsyncCallback[A]): Unit = {
      val t = assertNoChange(receivedResponses.length)(awaitAsyncCallback(a))
      assert(t.toEither.isLeft)
    }

    def awaitResponse(a: AsyncCallback[ResMsg]): ResMsg = {
      assertDifference(receivedResponses.length)(1) {
        a.tap(r => receivedResponses :+= r).toCallback.runNow()
      }
      receivedResponses.last
    }
  }

  override def tests = Tests {

    'ok - {
      val t = new Tester; import t._
      ws().open()
      val ab = send(ReqMsg(3))
      server.respondBy(reqMsg => ResMsg(reqMsg.msg + 100))
      awaitResponse(ab) ==> ResMsg(103)
    }

    'failure - {
      'connectingToClosed - {
        val t = new Tester(); import t._
        ws().close()
        val ab = assertNoChange(ws().sent().length)(send(ReqMsg(3)))
        ws().close()
        assertAsyncCallbackAlreadyFailed(ab)
      }

      'closed - {
        val t = new Tester(WebSocket.ReadyState.Closed); import t._
        val ab = assertNoChange(ws().sent().length)(send(ReqMsg(3)))
        assertAsyncCallbackAlreadyFailed(ab)
      }

      'inFlight - {
        val t = new Tester; import t._
        ws().open()
        val ab = send(ReqMsg(3))
        ws().close()
        assertAsyncCallbackAlreadyFailed(ab)
      }
    }

  }
}
