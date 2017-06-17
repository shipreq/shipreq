package shipreq.webapp.client.base.test

import japgolly.scalajs.react.Callback
import scalaz.{-\/, Equal}
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.protocol._
import shipreq.webapp.client.base.data.TCB
import shipreq.webapp.client.base.protocol._
import TestClientProtocol.Req

object TestClientProtocol {
  trait Req {
    val proc   : ServerSideProc
    val input  : proc.protocol.Input
    val success: proc.protocol.Output => TCB.Success
    val failure: RemoteFailure[proc.protocol.Failure] => TCB.Failure

    override def toString =
      s"Req($input)@${Integer.toHexString(##)}"

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
}

class TestClientProtocol(autoRespondArg: Boolean) extends ClientProtocol {

  var reqs = Vector.empty[Req]

  def reset(): Unit =
    reqs = Vector.empty

  var autoRespond = autoRespondArg

  def autoResponse(r: Req): Callback =
    Callback.empty

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
