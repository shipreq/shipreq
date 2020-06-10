package shipreq.webapp.base.protocol.websocket

import japgolly.scalajs.react.AsyncCallback
import scala.util.Try
import sourcecode.Line
import utest._
import shipreq.base.util._
import shipreq.webapp.base.protocol.websocket.WebSocketShared.CloseCode
import shipreq.webapp.base.protocol.websocket.WebSocket.ReadyState
import shipreq.webapp.base.test.WebappTestUtil._

object WebSocketClientTest extends TestSuite {
  import WebSocketClientTester._
  import WebSocketClient.State

  private def awaitAsyncCallback[A](a: AsyncCallback[A])(implicit l: Line): Try[A] = {
    var t = Option.empty[Try[A]]
    a.attemptTry.tap(r => t = Some(r)).toCallback.runNow()
    if (t.isEmpty) fail("AsyncCallback isn't complete.")
    t.get
  }

  override def tests = Tests {
    var receivedResponses = Vector.empty[ResMsg]

    def assertAsyncCallbackAlreadyFailed[A](a: AsyncCallback[A])(implicit l: Line): Unit = {
      val t = assertNoChange(receivedResponses.length)(awaitAsyncCallback(a))
      assert(t.toEither.isLeft)
    }

    def awaitResponse(a: AsyncCallback[ResMsg])(implicit l: Line): ResMsg = {
      assertDifference(receivedResponses.length)(1) {
        a.tap(r => receivedResponses :+= r).toCallback.runNow()
      }
      receivedResponses.last
    }

    "ok" - {
      val t = WebSocketClientTester(); import t._
      client.connect.runNow()
      ws().open()
      val ab = send(ReqMsg(3))
      server.respondBy(reqMsg => ResMsg(reqMsg.msg + 100))
      awaitResponse(ab) ==> ResMsg(103)
    }

    "failure" - {
      "connectingToClosed" - {
        val t = WebSocketClientTester(); import t._
        client.connect.runNow()
        ws().close()
        val ab = assertNoChange(ws().sent().length)(send(ReqMsg(3)))
        ws().close()
        assertAsyncCallbackAlreadyFailed(ab)
        assertEq(reauthAttempts, 0)
      }

      "inFlight" - {
        val t = WebSocketClientTester(); import t._
        client.connect.runNow()
        ws().open()
        val ab = send(ReqMsg(3))
        ws().close()
        assertAsyncCallbackAlreadyFailed(ab)
        assertEq(reauthAttempts, 0)
      }
    }

    "auth" - {
      "expiryWithImmediateLogin" - {
        val t = WebSocketClientTester(); import t._
        client.connect.runNow()
        ws().open()

        nextReauthResult = () => Allow
        assertEq(reauthAttempts, 0)
        assertAndClearStateChanges(State.Authorised(ReadyState.Open))

        ws().close(CloseCode.unauthorised)
        assertEq(reauthAttempts, 1)
        assertEq(ws().readyState(), ReadyState.Connecting)
        ws().open()
        assertAndClearStateChanges(State.Unauthorised, State.Authorised(ReadyState.Open))

        ws().close()
        client.connect.runNow()
        assertEq(ws().readyState(), ReadyState.Connecting)
        ws().open()
        assertAndClearStateChanges(State.Authorised(ReadyState.Closed), State.Authorised(ReadyState.Open))
        assertEq("Shouldn't try to re-authenticate", reauthAttempts, 1)
      }

      "expiryWithEventualLogin" - {
        val t = WebSocketClientTester(); import t._
        client.connect.runNow()
        ws().open()

        nextReauthResult = () => Deny
        assertEq(reauthAttempts, 0)
        assertAndClearStateChanges(State.Authorised(ReadyState.Open))

        ws().close(CloseCode.unauthorised)
        assertEq(reauthAttempts, 1)
        assertAndClearStateChanges(State.Unauthorised)

        client.connect.runNow()
        assertEq(reauthAttempts, 2)
        assertAndClearStateChanges()

        nextReauthResult = () => Allow
        client.connect.runNow()
        assertEq(reauthAttempts, 3)
        assertEq(ws().readyState(), ReadyState.Connecting)
        ws().open()
        assertAndClearStateChanges(State.Authorised(ReadyState.Open))

        ws().close()
        client.connect.runNow()
        assertEq(ws().readyState(), ReadyState.Connecting)
        ws().open()
        assertEq("Shouldn't try to re-authenticate", reauthAttempts, 3)
        assertAndClearStateChanges(State.Authorised(ReadyState.Closed), State.Authorised(ReadyState.Open))
      }
    }

    "bug" - {
      val tester = WebSocketClientTester()
      import tester._

      client.connect.runNow()
      ws().close(CloseCode.unauthorised)
      sendMsg()
      nextReauthResult = () => Allow
      sendMsg()
      ws().open()
      server.respondToNextPending() // The bug used to be here in that it would try to send the first request even though it had already been failed
      assertEq(ws().responsesPending(), false)
    }

  }
}
