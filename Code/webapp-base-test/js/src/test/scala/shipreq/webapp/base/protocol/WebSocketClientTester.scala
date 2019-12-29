package shipreq.webapp.base.protocol

import boopickle.DefaultBasic._
import japgolly.scalajs.react._
import java.time.Duration
import scalaz.{-\/, \/, \/-}
import sourcecode.Line
import utest._
import shipreq.base.test.JsTestTimers
import shipreq.base.util._
import shipreq.base.util.JsExt._
import shipreq.webapp.base.lib.LoggerJs
import shipreq.webapp.base.protocol.FakeWebSocket.Message
import shipreq.webapp.base.protocol.WebSocket.ReadyState
import shipreq.webapp.base.protocol.WebSocketShared.ReqId
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.base.protocol.binary.SafePickler.ConstructionHelperImplicits._
import shipreq.webapp.base.test.WebappTestUtil._

object WebSocketClientTester {

  def apply(): WebSocketClientTester =
    new WebSocketClientTester

  final case class ReqMsg(msg: Int)
  final case class ResMsg(msg: Int)
  final case class PushMsg(msg: Int)

  implicit def univEq: UnivEq[PushMsg] = UnivEq.derive

  object P extends  Protocol.WebSocket.ClientReqServerPush[SafePickler] {
    override type ReqId  = WebSocketShared.ReqId
    override type ReqRes = Protocol.RequestResponse.Simple[SafePickler, ReqMsg, ResMsg]
    override val url     = Url.Relative("/x")
    override val req     = Protocol(transformPickler(ReqMsg.apply)(_.msg).asV10)
    override val push    = Protocol(transformPickler(PushMsg.apply)(_.msg).asV10)
    val res              = Protocol(transformPickler(ResMsg.apply)(_.msg).asV10)
    val ReqRes: ReqRes   = Protocol.RequestResponse.simple(res)
  }

  val serverProtocols = WebSocketServerHelper(P)

  final val debug = false

  def debugPrintln(s: => Any): Unit =
    if (debug) println("             " + s)
}

class WebSocketClientTester {
  import WebSocketClientTester._
  import WebSocketClient.State

  var webSockets = Vector.empty[FakeWebSocket]
  var stateChanges = Vector.empty[State]
  var sentPushes = Vector.empty[PushMsg]
  var receivedPushes = Vector.empty[PushMsg]
  var sendResults = Vector.empty[Vector[Either[Throwable, ErrorMsg \/ ResMsg]]]
  var reauthAttempts = 0

  var nextReauthResult: () => Permission =
    () => Deny

  var nextServerPushMsg: () => PushMsg = {
    var prev = 0
    () => {
      prev += 1
      PushMsg(prev)
    }
  }

  val retries = Retries.exponentially(Duration.ofMillis(10))

  val timers = JsTestTimers()

  val newWS = CallbackTo {
    val f = new FakeWebSocket("fake url", ReadyState.Connecting)
    f.onSend.set(_ => ())
    webSockets :+= f
    f
  }

  def onStateChange(s: State): Callback =
    Callback {
      stateChanges :+= s
      debugPrintln(s"State change: $s")
    }

  val client = WebSocketClient.Builder(newWS, P, retries)
    .build(
      reauthorise   = AsyncCallback.point{reauthAttempts += 1; nextReauthResult()},
      onServerPush  = p => Callback(receivedPushes :+= p),
      onStateChange = _ => onStateChange,
      timers        = timers,
      logger        = LoggerJs.on) // don't turn this off - it being on has caught bugs before

  val invoker = client.invoker(P.ReqRes)

  def sendMsg(): Unit = {
    val reqId = sendResults.length
    sendResults :+= Vector.empty
    debugPrintln(s"Sending ReqMsg($reqId)")
    invoker(ReqMsg(reqId)).attempt.map { r =>
      debugPrintln(s"Received response for $reqId: $r")
      sendResults = sendResults.updated(reqId, sendResults(reqId) :+ r)
    }.toCallback.runNow()
    ()
  }

  def ws(): FakeWebSocket =
    webSockets.lastOption.getOrElse(sys.error("webSockets is empty"))

  def latestMsg() = webSockets.reverseIterator.flatMap(_.sent().reverseIterator).next()

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

    def respondToNextPending(): Unit =
      ws().respondToNextPending { sent =>
        debugPrintln(s"Server received ${sent.binaryData}")
        val (reqId, reqMsg) = serverProtocols.protocolCS.codec.decode(sent.binaryData).needRight
        val resMsg = ResMsg(reqMsg.msg * -1)
        debugPrintln(s"Responding to $reqId $reqMsg with $resMsg")
        val res = \/-((reqId, P.res.andValue(resMsg)))
        val bd = serverProtocols.protocolSC.codec.encode(res)
        Message.ArrayBuffer(bd.toArrayBuffer)
      }

    def push(msg: PushMsg = nextServerPushMsg()): Unit = {
      val bin = serverProtocols.protocolSC.codec.encode(-\/(msg))
      ws().recv(Message.ArrayBuffer(bin.toArrayBuffer))
      sentPushes :+= msg
    }
  }

  def send(req: ReqMsg): AsyncCallback[ResMsg] =
    client.send(P.ReqRes)(req).runNow()

  def assertAndClearStateChanges(expect: State*)(implicit l: Line): Unit = {
    assertEq(stateChanges, expect.toVector)
    stateChanges = Vector.empty
  }

  def checkInvariants(): Unit = {
    assertEq(receivedPushes, sentPushes)
    for (r <- sendResults)
      assert(r.length <= 1)
  }

}
