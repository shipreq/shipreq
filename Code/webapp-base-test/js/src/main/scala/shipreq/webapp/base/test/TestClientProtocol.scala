package shipreq.webapp.base.test

import japgolly.scalajs.react.Callback
import org.scalajs.dom.console
import scalaz.Equal
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.data.TCB
import shipreq.webapp.base.protocol._
import TestClientProtocol._

object TestClientProtocol {
  trait Req {
    val proc   : ServerSideProc
    val input  : proc.protocol.Input
    val success: proc.protocol.Output => TCB.Success
    val failure: RemoteFailure[proc.protocol.Failure] => TCB.Failure

    override def toString =
      "Req[%08X]:%s(%s)".format(##, proc.key.trim, input)

    def force(p2: ServerSideProc) =
      this.asInstanceOf[Req {val proc: p2.type}]

    var _pendingResponse = true
    def responsePending = _pendingResponse
    def responded = !responsePending

    def markResponded(): Unit =
      if (responsePending)
        _pendingResponse = false
      else
        sys error "Request has already been responded to."
  }
  type ReqP[P <: ServerSideProc.Protocol] = Req { val proc: ServerSideProc.For[P] }
}

class TestClientProtocol(autoRespondArg: Boolean) extends ClientProtocol {

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
    (autoResponsePFs.find(_.isDefinedAt(r)).getOrElse(autoResponseFallback))(r)

  def addAutoResponsePF(f: PartialFunction[Req, Callback]): Unit =
    autoResponsePFs :+= f

  def addAutoResponse[P <: ServerSideProc.Protocol](p: P)(f: ReqP[P] => Callback): Unit =
    addAutoResponsePF {
      case r if r.proc.protocol ==* p => f(r.asInstanceOf[ReqP[P]])
    }

  def call(p: ServerSideProc)(_input  : p.protocol.Input,
                              _success: p.protocol.Output => TCB.Success,
                              _failure: RemoteFailure[p.protocol.Failure] => TCB.Failure): Callback = {
    //println(s"RPC: ${_r.d}(${_r.n}) ← ${_i}")
    Callback {
      val r = new Req {
        override val proc: p.type = p
        override val input   = _input
        override val success = _success
        override val failure = _failure
      }
      reqs :+= r
      if (autoRespond)
        autoRespondToLast()
    }
  }

  def assertReqsSent(count: Int): Unit =
    assertEq("AJAX requests", reqs.size, count)

  def last = reqs.last

  def respondToLast(p: ServerSideProc)(o: p.protocol.Output): Unit =
    last.force(p).success(o).runNow()

  def autoRespondToLast(): Unit = {
    val r = last
    r.markResponded()
    autoResponse(r).runNow()
  }

  def failLast(): Unit = {
    val r = last
    r.markResponded()
    r.failure(RemoteFailure.exception(new Throwable("Dummy error from TestClientProtocol.failLast()"))).runNow()
  }

  def lastTwo(r: ServerSideProc) = {
    val Vector(a, b) = reqs.takeRight(2).map(_.force(r))
    (a, b)
  }

  def assertLastTwoRequestsEqual(p: ServerSideProc)(implicit e: Equal[p.protocol.Input]): Unit = {
    val (a, b) = lastTwo(p)
    assertEq("Last two requests", a.input, b.input)
  }
}
