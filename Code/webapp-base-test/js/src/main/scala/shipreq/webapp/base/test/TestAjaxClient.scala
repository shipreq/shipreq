package shipreq.webapp.base.test

import japgolly.scalajs.react.{AsyncCallback, Callback, CallbackTo}
import scala.util.{Failure, Success, Try}
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.protocol.ajax.AjaxClient
import shipreq.webapp.base.protocol.binary.SafePickler

object TestAjaxClient {

  trait Req {
    val ajax      : Protocol.Ajax[SafePickler]
    val input     : ajax.protocol.RequestType
    val onResponse: Throwable \/ SafePickler.Result[ajax.protocol.ResponseType] => Callback

    override def toString =
      "Req[%08X]:%s(%s)".format(##, ajax.url, input)

    def force(p2: Protocol.Ajax[SafePickler]) = {
      assert(ajax eq p2)
      this.asInstanceOf[ReqOf[p2.type]]
    }

    private var _pendingResponse = true
    def responsePending = _pendingResponse
    def responded = !responsePending

    def markResponded(): Unit =
      if (responsePending)
        _pendingResponse = false
      else
        sys error "Request has already been responded to."
  }

  type ReqOf[P <: Protocol.Ajax[SafePickler]] = Req {val ajax: P}
}

class TestAjaxClient(autoRespondArg: Boolean) extends AjaxClient.Binary {
  import TestAjaxClient._

  var reqs = Vector.empty[Req]

  def reset(): Unit =
    reqs = Vector.empty

  var autoRespond: Boolean =
    autoRespondArg

  var autoResponsePFs: List[PartialFunction[Req, Callback]] =
    Nil

  var autoResponseFallback: Req => Callback =
    r => Callback(console.warn(s"${Console.YELLOW}Don't know how to respond to $r${Console.RESET}"))

  final def autoResponse(r: Req): Callback =
    autoResponsePFs.find(_.isDefinedAt(r)).getOrElse(autoResponseFallback)(r)

  def addAutoResponsePF(f: PartialFunction[Req, Callback]): Unit =
    autoResponsePFs :+= f

  def addAutoResponse(p: Protocol.Ajax[SafePickler])(f: ReqOf[p.type] => Callback): Unit =
    addAutoResponsePF {
      case r if r.ajax eq p => f(r.force(p))
    }

  override def invoker(p: Protocol.Ajax[SafePickler]): ServerSideProcInvoker[p.protocol.RequestType, ErrorMsg, p.protocol.ResponseType] =
    ServerSideProcInvoker.fromSimple { (req: p.protocol.RequestType) =>
      apply(p)(req).map(_.map { result =>
        val resCodec = p.protocol.prepareSend(req).response.codec
        AjaxClient.BinaryImpl.Result(result, resCodec.version).errMsgOrSuccess
      })
    }.mergeFailure

  def apply(p: Protocol.Ajax[SafePickler])(req: p.protocol.RequestType) = CallbackTo[AsyncCallback[SafePickler.Result[p.protocol.ResponseType]]] {

    var callbacks: List[Try[SafePickler.Result[p.protocol.ResponseType]] => Callback] =
      Nil

    var result: Try[SafePickler.Result[p.protocol.ResponseType]] =
      null

    def reactNow(): Unit = {
      if (result ne null)
        callbacks.foreach(_(result).runNow())
    }

    def newReq(): Unit = {
      val r = new Req {
        override val ajax: p.type = p
        override val input = req
        override val onResponse = i => Callback {
          result = i.fold(Failure(_), Success(_))
          reactNow()
        }
      }
      reqs :+= r
      if (autoRespond)
        autoRespondToLast()
    }

    AsyncCallback[SafePickler.Result[p.protocol.ResponseType]](f =>
      Callback {
        callbacks ::= f
        newReq()
      })
  }

  def assertReqsSent(count: Int): Unit =
    assertEq("AJAX requests", reqs.size, count)

  def last = reqs.last

  def respondToLast(p: Protocol.Ajax[SafePickler])(o: p.protocol.ResponseType): Unit =
    respondToLast2(p)(\/-(o))

  def respondToLast2(p: Protocol.Ajax[SafePickler])(o: SafePickler.Result[p.protocol.ResponseType]): Unit =
    last.force(p).onResponse(\/-(o)).runNow()

  def autoRespondToLast(): Unit = {
    val r = last
    r.markResponded()
    autoResponse(r).runNow()
  }

  def failLast(): Unit = {
    val r = last
    r.markResponded()
    r.onResponse(-\/(new Throwable("Dummy error from TestAjaxClient.failLast()"))).runNow()
  }

//  def lastTwo[I, O](r: ServerSideProc[I, O]) = {
//    val Vector(a, b) = reqs.takeRight(2).map(_.force(r))
//    (a, b)
//  }
//
//  def assertLastTwoRequestsEqual[I, O](p: ServerSideProc[I, O])(implicit e: Equal[I]): Unit = {
//    val (a, b) = lastTwo(p)
//    assertEq("Last two requests", a.input, b.input)
//  }
}
