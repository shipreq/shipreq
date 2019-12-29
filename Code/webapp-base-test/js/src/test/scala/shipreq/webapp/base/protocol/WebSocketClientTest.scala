package shipreq.webapp.base.protocol

import boopickle.DefaultBasic._
import japgolly.scalajs.react.{AsyncCallback, Callback, CallbackTo}
import scala.util.Try
import scalaz.\/-
import sourcecode.Line
import utest._
import shipreq.base.util._
import shipreq.base.util.JsExt._
import shipreq.webapp.base.lib.LoggerJs
import shipreq.webapp.base.protocol.FakeWebSocket.Message
import shipreq.webapp.base.protocol.WebSocketShared.{CloseCode, ReqId}
import shipreq.webapp.base.protocol.WebSocket.ReadyState
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.base.protocol.binary.SafePickler.ConstructionHelperImplicits._
import shipreq.webapp.base.test.WebappTestUtil._

object WebSocketClientTest extends TestSuite {
  import WebSocketClient.State

  private final case class ReqMsg(msg: Int)
  private final case class ResMsg(msg: Int)
  private final case class PushMsg(msg: String)

  private object P extends  Protocol.WebSocket.ClientReqServerPush[SafePickler] {
    override type ReqId  = WebSocketShared.ReqId
    override type ReqRes = Protocol.RequestResponse.Simple[SafePickler, ReqMsg, ResMsg]
    override val url     = Url.Relative("/x")
    override val req     = Protocol(transformPickler(ReqMsg.apply)(_.msg).asV10)
    override val push    = Protocol(transformPickler(PushMsg.apply)(_.msg).asV10)
    val res              = Protocol(transformPickler(ResMsg.apply)(_.msg).asV10)
    val ReqRes: ReqRes   = Protocol.RequestResponse.simple(res)
  }

  private val serverProtocols = WebSocketServerHelper(P)

  private class Tester(initialState: ReadyState = ReadyState.Connecting) {
    var webSockets = Vector.empty[FakeWebSocket]

    var stateChanges = Vector.empty[State]
    var receivedPushes = Vector.empty[PushMsg]
    var receivedResponses = Vector.empty[ResMsg]

    val newWS = CallbackTo {
      val f = new FakeWebSocket("fake url", initialState)
      webSockets :+= f
      f
    }

    var reauthAttempts = 0
    var reauthResult: Permission = Deny

    private def onStateChange(s: State): Callback =
      Callback {
        stateChanges :+= s
        //println(s"State change: $s")
      }

    val client = WebSocketClient.Builder(newWS, P, Retries.none)
      .build(
        reauthorise   = AsyncCallback.point{reauthAttempts += 1; reauthResult},
        onServerPush  = p => Callback(receivedPushes :+= p),
        onStateChange = _ => onStateChange,
        timers        = JsTimers.real,
        logger        = LoggerJs.off)

    client.connect.runNow()

    def ws(): FakeWebSocket =
      webSockets.lastOption.getOrElse(sys.error("webSockets is empty"))

    def latestMsg() = webSockets.reverseIterator.flatMap(_.sent().reverseIterator).next()

    def send(req: ReqMsg): AsyncCallback[ResMsg] =
      client.send(P.ReqRes)(req).runNow()

    object server {
      def parseRequest(msg: Message = latestMsg()) =
        serverProtocols.protocolCS.codec.decode(msg.binaryData).needRight

      def respondBy(f: ReqMsg => ResMsg) = {
        val (reqId, reqMsg) = parseRequest()
        respond(reqId, f(reqMsg))
      }

      def respond(reqId: ReqId, resMsg: ResMsg) = {
        val res = \/-((reqId, P.res.andValue(resMsg)))
        val bd = serverProtocols.protocolSC.codec.encode(res)
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

    def assertAndClearStateChanges(expect: State*)(implicit l: Line): Unit = {
      assertEq(stateChanges, expect.toVector)
      stateChanges = Vector.empty
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
        assertEq(reauthAttempts, 0)
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
        assertEq(reauthAttempts, 0)
      }
    }

    'auth - {
      'expiryWithImmediateLogin - {
        val t = new Tester(ReadyState.Open); import t._
        reauthResult = Allow
        assertEq(reauthAttempts, 0)
        assertAndClearStateChanges(State.Authorised(ReadyState.Open))

        ws().close(CloseCode.unauthorised)
        assertEq(reauthAttempts, 1)
        assertAndClearStateChanges(State.Unauthorised, State.Authorised(ReadyState.Open))

        ws().close()
        client.connect.runNow()
        assertEq("Shouldn't try to re-authenticate", reauthAttempts, 1)
        assertAndClearStateChanges(State.Authorised(ReadyState.Closed), State.Authorised(ReadyState.Open))
      }

      'expiryWithEventualLogin - {
        val t = new Tester(ReadyState.Open); import t._
        reauthResult = Deny
        assertEq(reauthAttempts, 0)
        assertAndClearStateChanges(State.Authorised(ReadyState.Open))

        ws().close(CloseCode.unauthorised)
        assertEq(reauthAttempts, 1)
        assertAndClearStateChanges(State.Unauthorised)

        client.connect.runNow()
        assertEq(reauthAttempts, 2)
        assertAndClearStateChanges()

        reauthResult = Allow
        client.connect.runNow()
        assertEq(reauthAttempts, 3)
        assertAndClearStateChanges(State.Authorised(ReadyState.Open))

        ws().close()
        client.connect.runNow()
        assertEq("Shouldn't try to re-authenticate", reauthAttempts, 3)
        assertAndClearStateChanges(State.Authorised(ReadyState.Closed), State.Authorised(ReadyState.Open))
      }
    }

  }
}
